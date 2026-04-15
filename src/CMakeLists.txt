# ===========================================================================
# src/CMakeLists.txt -- OpenAxiom source-tree targets
# ===========================================================================
# This file defines the native C/C++ libraries and executables that form
# the foundation of the OpenAxiom build, plus the Lisp runtime compilation
# (Phase 3).  Later PRs add:
#   - Boot translator and interpreter chain (Phase 4)
#   - Algebra substrate (Phase 5)
#   - Optional subsystems and packaging (Phase 6)
#
# It is organised as follows:
#
#   A. Common include paths and blanket LANGUAGE CXX
#   B. Helper: oa_apply_build_flags -- shared warning / profiling / debug opts
#   C. Static libraries: open-axiom-core, OpenAxiom, spad
#   D. Executables: hammer, open-axiom, oa-lisp
#   E. Phase 3: Lisp runtime compilation and image linking
#   F. Staging targets: stage-native-layout, phase1-native, phase2-native
# ===========================================================================


# -- A. Common include paths and blanket LANGUAGE CXX ----------------------
# Three include directories are shared by every native target:
#   src/include         -- hand-written public headers (via FILE_SET)
#   ${BUILD}/include    -- generated header variants (open-axiom/config)
#   ${BUILD}/config     -- openaxiom-c-macros.h (via FILE_SET)
# They propagate automatically through the FILE_SET declarations on
# open-axiom-core and the PUBLIC link dependency chain.

# All .c source files in the OpenAxiom tree are compiled as C++.
# The legacy C sources share headers with the C++ codebase that use
# namespaces, templates, and standard-library types (e.g. std::vector).
# Rather than sprinkling LANGUAGE CXX overrides on each source list,
# we apply the property to every .c file under src/ in one shot.
file(GLOB_RECURSE _oa_all_c_sources
  CONFIGURE_DEPENDS
  "${CMAKE_CURRENT_SOURCE_DIR}/*.c"
)
set_source_files_properties(${_oa_all_c_sources} PROPERTIES LANGUAGE CXX)

# open-axiom-core is the lowest-level static library.  It provides:
#   - POSIX signal handling (bsdsignal)
#   - Utility C functions used by the Lisp layer (cfuns-c)
#   - Socket I/O for the session manager / HyperDoc protocol (sockio-c)
#   - Memory / storage management (storage)
#   - Standard stream wrappers (std-streams)
#   - S-expression tokeniser and recursive-descent parser
#
# Every other native target links against open-axiom-core (directly or
# transitively), so its include dirs and features propagate via PUBLIC.
#
# Public headers are declared via FILE_SET (CMake 3.23+) so that
# build-time and install-time include paths, header installation, and
# export-set integration are all handled automatically -- no manual
# BUILD_INTERFACE / INSTALL_INTERFACE generator expressions needed.
add_library(open-axiom-core STATIC
  lib/bsdsignal.cxx
  lib/cfuns-c.cxx
  lib/sockio-c.cxx
  utils/storage.cxx
  io/std-streams.cxx
  io/InputFragment.cxx
  syntax/token.cxx
  syntax/sexpr.cxx
  syntax/Parser.cxx
)

# -- Public header FILE_SETs on open-axiom-core --
# All public headers under src/include/ are declared in a single FILE_SET.
# The generated config header is in a separate FILE_SET since it lives
# in the build tree (different BASE_DIRS).
target_sources(open-axiom-core
  PUBLIC FILE_SET public_headers TYPE HEADERS
    BASE_DIRS "${PROJECT_SOURCE_DIR}/src/include"
    FILES
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom.h"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/Charset"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/Constructor"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/Database"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/FileMapping"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/Input"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/InputFragment"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/Lisp"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/Parser"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/SourceFile"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/SyntaxTree"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/defaults"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/diagnostics"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/dialect"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/hash-table"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/iterator"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/sexpr"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/storage"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/string-pool"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/structure"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/token"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/token-value"
      "${PROJECT_SOURCE_DIR}/src/include/open-axiom/vm"
  PUBLIC FILE_SET generated_headers TYPE HEADERS
    BASE_DIRS "${PROJECT_BINARY_DIR}/config"
    FILES
      "${PROJECT_BINARY_DIR}/config/openaxiom-c-macros.h"
)

# Include dirs and compile features are applied by the common loop below
# (see "Apply common flags to all three static libraries").

# -- B. Helper: apply shared compiler flags to a target --------------------
# GCC and Clang targets share the same warning, profiling, and runtime-
# checking flags.  This function eliminates the three-way repetition
# that previously appeared for open-axiom-core, OpenAxiom, and spad.

function(oa_apply_build_flags target)
  if(NOT CMAKE_CXX_COMPILER_ID MATCHES "GNU|Clang")
    return()
  endif()
  target_compile_options(${target} PRIVATE -Wall)
  if(OA_ENABLE_PROFILING)
    target_compile_options(${target} PRIVATE -pg)
    target_link_options(${target} PRIVATE -pg)
  endif()
  if(OA_ENABLE_RUNTIME_CHECKING)
    target_compile_options(${target} PRIVATE -O0 -g)
  endif()
endfunction()

# -- C. Static libraries --------------------------------------------------

# -- open-axiom-core: low-level C/C++ runtime (signals, sockets, I/O, parser) --
#    (sources declared above; flags applied below via oa_apply_common_target_flags)

# libm -- needed for math functions used by the Lisp numeric tower.
if(UNIX)
  target_link_libraries(open-axiom-core PUBLIC m)
endif()

# libdl -- dynamic loading (dlopen/dlsym) used by delayed-FFI Lisps.
if(CMAKE_DL_LIBS)
  target_link_libraries(open-axiom-core PUBLIC ${CMAKE_DL_LIBS})
endif()

# libutil -- provides openpty() on Linux for the terminal subsystem.
find_library(UTIL_LIBRARY util)
if(UTIL_LIBRARY)
  target_link_libraries(open-axiom-core PUBLIC ${UTIL_LIBRARY})
endif()

# -- OpenAxiom: mid-level utility / VM / command-line library --
# String pool, command-line parsing, filesystem helpers, and the runtime
# VM / Lisp bridge / database layer.  OPENAXIOM_ROOT_DIRECTORY is baked
# in so the driver can locate the installed layout at run time.
add_library(OpenAxiom STATIC
  utils/string-pool.cc
  utils/command.cc
  utils/filesystem.cc
  io/Input.cc
  rt/vm.cc
  rt/Lisp.cc
  rt/Database.cc
)

# -- spad: legacy C sources (terminal / cursor / hashing) --
# These files have .c extensions but are compiled as C++ (see the
# blanket LANGUAGE CXX policy at the top of this file).
set(oa_spad_sources
  lib/cursor.c
  lib/edin.c
  lib/fnct_key.c
  lib/openpty.c
  lib/prt.c
  lib/wct.c
  lib/halloc.c
  lib/hash.c
)

add_library(spad STATIC
  ${oa_spad_sources}
)

target_compile_definitions(OpenAxiom PRIVATE OPENAXIOM_ROOT_DIRECTORY="${CMAKE_INSTALL_PREFIX}/${OA_INSTALL_LAYOUT_ROOT}")
target_link_libraries(OpenAxiom PUBLIC open-axiom-core)
target_link_libraries(spad PUBLIC open-axiom-core)

# -- Apply common flags to all three static libraries ----------------------
# Public headers and their include dirs are handled by the FILE_SET on
# open-axiom-core, which propagates transitively to OpenAxiom and spad
# via target_link_libraries(... PUBLIC open-axiom-core).
#
# The generated ${BUILD}/include directory (containing open-axiom/config)
# is still needed as a build-time-only include path since source code
# uses #include <open-axiom/config>.
set(oa_native_libs open-axiom-core OpenAxiom spad)
foreach(lib ${oa_native_libs})
  target_include_directories(${lib} PUBLIC
    $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
  )
  target_compile_features(${lib} PUBLIC cxx_std_20)
  oa_apply_build_flags(${lib})
  if(OA_ENABLE_THREADS)
    target_link_libraries(${lib} PUBLIC Threads::Threads)
  endif()
endforeach()

# X11 drawing helpers -- only built when X11 headers are present.
# These implement HyperDoc / spadcolors / dithering for the classic UI.
if(OA_HAVE_X11)
  target_sources(spad PRIVATE
    lib/pixmap.c
    lib/spadcolors.c
    lib/util.c
    lib/XDither.c
    lib/XShade.c
    lib/XSpadFill.c
  )
endif()

# -- D. Executables --------------------------------------------------------

# hammer -- the bootstrap tool that orchestrates later build phases
# (algebra compilation, database generation, etc.).
add_executable(hammer utils/hammer.cc)
target_link_libraries(hammer PRIVATE OpenAxiom open-axiom-core)

# open-axiom -- the main user-facing driver (installed into $PREFIX/bin).
add_executable(open-axiom driver/main.cc)
target_link_libraries(open-axiom PRIVATE OpenAxiom open-axiom-core)

# oa-lisp -- thin C++ driver that launches the Lisp image.  Installed as
# "lisp" inside the versioned layout so it does not collide with system
# Lisp binaries.
add_executable(oa-lisp rt/driver.cc)
set_target_properties(oa-lisp PROPERTIES OUTPUT_NAME lisp)
target_link_libraries(oa-lisp PRIVATE OpenAxiom open-axiom-core)

# -- E. Phase 3: Lisp compilation and base-image linking ------------------
# The Phase 3 chain:
#   1. Generate core.lisp from the .in template (configure_file)
#   2. Compile core.lisp -> core.FASL  (+ core.o for ECL)
#   3. Link the FASL into a standalone image: oa-base-lisp
#   4. Stage everything under OA_STAGE_ROOT for later phases
#
# The LISP_CMD variable below avoids repeating the multi-part
# `cmake -E env ...EXECUTABLE ...QUIET ...BATCH` invocation prefix.

set(oa_lisp_build_dir "${PROJECT_BINARY_DIR}/phase3/lisp")
set(oa_lisp_generated_core "${oa_lisp_build_dir}/core.lisp")
set(oa_lisp_generated_fasl "${oa_lisp_build_dir}/core.${OA_LISP_FASL_EXT}")
set(oa_lisp_generated_linkable "${oa_lisp_build_dir}/core.${OA_LISP_LNK_EXT}")
set(oa_lisp_generated_image "${oa_lisp_build_dir}/oa-base-lisp${CMAKE_EXECUTABLE_SUFFIX}")

file(MAKE_DIRECTORY "${oa_lisp_build_dir}")

# Variables substituted into core.lisp.in by configure_file.
# They bridge CMake-side knowledge (install paths, compiler identity,
# target triple, optional features) into the Lisp source so that the
# Lisp bootstrap code can locate native tools and honour build options.
#
# The variable names must match the @VAR@ placeholders in core.lisp.in
# exactly -- some are inherited from the Autotools era (e.g. CXX, LDFLAGS,
# SHREXT, LIBEXT) and cannot be renamed without updating the template.
set(host "${OA_TARGET_TRIPLE}")
set(build "${OA_TARGET_TRIPLE}")
set(target "${OA_TARGET_TRIPLE}")
set(CXX "${CMAKE_CXX_COMPILER}")
set(LDFLAGS "${CMAKE_EXE_LINKER_FLAGS}")
set(open_axiom_installdir "${CMAKE_INSTALL_PREFIX}/${OA_INSTALL_LAYOUT_ROOT}")
set(oa_keep_files "")
set(oa_enable_profiling nil)
if(OA_ENABLE_PROFILING)
  set(oa_enable_profiling t)
endif()
set(oa_use_llvm no)
if(OPENAXIOM_HOST_HAS_LLVM)
  set(oa_use_llvm yes)
endif()
set(oa_editor "$ENV{EDITOR}")
if(NOT oa_editor)
  set(oa_editor "vi")
endif()
set(oa_shrlib_prefix "${CMAKE_SHARED_LIBRARY_PREFIX}")
# SHREXT / LIBEXT: library extension with and without the leading dot.
# Consumed by the Lisp loader when building shared objects at algebra time.
set(SHREXT "${CMAKE_SHARED_LIBRARY_SUFFIX}")
string(REGEX REPLACE "^\\." "" LIBEXT "${CMAKE_STATIC_LIBRARY_SUFFIX}")
# Extra C runtime libraries needed on Windows (Winsock for sockio).
set(oa_c_runtime_extra "")
if(WIN32)
  set(oa_c_runtime_extra "\"-lwsock32\"")
endif()

# -- Step 1: Generate core.lisp from the template --
configure_file(
  "${PROJECT_SOURCE_DIR}/src/lisp/core.lisp.in"
  "${oa_lisp_generated_core}"
  @ONLY
)

# Guard: only emit the Lisp build rules when a supported runtime was found.
if(OA_LISP_EXECUTABLE AND OA_LISP_FLAVOR MATCHES "^(gcl|ecl|sbcl|clisp|clozure)$")
  # Common Lisp invocation prefix -- used by every custom command below.
  # Wraps the binary in `cmake -E env` for flavor-specific environment
  # variables (e.g. GCL_ANSI=t), followed by quiet/batch flags.
  set(OA_LISP_CMD
    ${CMAKE_COMMAND} -E env ${OA_LISP_ENV_VARS}
    "${OA_LISP_EXECUTABLE}" ${OA_LISP_QUIET_ARGS} ${OA_LISP_BATCH_ARGS}
  )

  # -- Step 2: Compile core.lisp -> FASL (+ .o for ECL) --
  if(OA_LISP_FLAVOR STREQUAL "ecl")
    # ECL uses the "system-p" flag to produce a relocatable .o alongside
    # the FASL; both are needed at the linking step.
    add_custom_command(
      OUTPUT "${oa_lisp_generated_fasl}" "${oa_lisp_generated_linkable}"
      COMMAND ${OA_LISP_CMD}
              ${OA_LISP_EVAL_FLAG} "(require (quote cmp))"
              ${OA_LISP_EVAL_FLAG} "(compile-file \"core.lisp\" :system-p t)"
              ${OA_LISP_EVAL_FLAG} "(c::build-fasl \"core.${OA_LISP_FASL_EXT}\" :lisp-files (quote (\"core.${OA_LISP_LNK_EXT}\")))"
              ${OA_LISP_EVAL_FLAG} "(quit)"
      WORKING_DIRECTORY "${oa_lisp_build_dir}"
      DEPENDS "${oa_lisp_generated_core}" stage-native-layout
      COMMENT "Compile the OpenAxiom core Lisp runtime with ECL"
      VERBATIM
    )
    set(oa_base_lisp_objects "(\"core.${OA_LISP_LNK_EXT}\")")
  else()
    # Non-ECL flavors: standard compile-file -> single FASL output.
    add_custom_command(
      OUTPUT "${oa_lisp_generated_fasl}"
      COMMAND ${OA_LISP_CMD}
              ${OA_LISP_EVAL_FLAG} "(progn (compile-file \"core.lisp\") (quit))"
      WORKING_DIRECTORY "${oa_lisp_build_dir}"
      DEPENDS "${oa_lisp_generated_core}" stage-native-layout
      COMMENT "Compile the OpenAxiom core Lisp runtime"
      VERBATIM
    )
    set(oa_base_lisp_objects "nil")
  endif()

  # -- Step 3: Link the FASL into a standalone executable image --
  add_custom_command(
    OUTPUT "${oa_lisp_generated_image}"
    COMMAND ${OA_LISP_CMD}
            ${OA_LISP_EVAL_FLAG} "(load \"core\")"
            ${OA_LISP_EVAL_FLAG} "(|AxiomCore|::|link| \"oa-base-lisp${CMAKE_EXECUTABLE_SUFFIX}\" (quote ${oa_base_lisp_objects}) \"|AxiomCore|::|topLevel|\")"
    WORKING_DIRECTORY "${oa_lisp_build_dir}"
    DEPENDS "${oa_lisp_generated_fasl}"
    COMMENT "Link the Phase 3 OpenAxiom base Lisp image"
    VERBATIM
  )

  # -- Step 4: Stage Lisp artifacts into the runtime layout --
  add_custom_target(stage-lisp-runtime
    COMMAND ${CMAKE_COMMAND} -E make_directory
      "${OA_STAGE_ROOT}/lisp"
      "${OA_STAGE_ROOT}/share/open-axiom/cmake"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${oa_lisp_generated_core}"
      "${OA_STAGE_ROOT}/lisp/"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${oa_lisp_generated_fasl}"
      "${OA_STAGE_ROOT}/lisp/"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${oa_lisp_generated_image}"
      "${OA_STAGE_ROOT}/bin/"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${PROJECT_BINARY_DIR}/config/openaxiom-lisp-settings.cmake"
      "${OA_STAGE_ROOT}/share/open-axiom/cmake/"
    DEPENDS stage-native-layout "${oa_lisp_generated_image}"
    COMMENT "Stage the Phase 3 Lisp runtime artifacts"
  )

  # Umbrella target -- the single name that CI and developers invoke.
  add_custom_target(phase3-lisp-runtime
    DEPENDS stage-lisp-runtime
    COMMENT "Build and stage the Phase 3 Lisp runtime foundation"
  )

endif()  # OA_LISP_EXECUTABLE guard

# -- F. Staging targets ----------------------------------------------------
# stage-native-layout copies all Phase 1-2 artifacts (binaries, libraries,
# headers) into OA_STAGE_ROOT -- an in-tree directory that mirrors the
# final installation layout.  Later phases read from this staging tree.
#
# Executables and libraries are staged via lists so adding a new target
# only requires appending to the list -- no new COMMAND block needed.

set(oa_stage_dirs
  "${OA_STAGE_ROOT}/bin"
  "${OA_STAGE_ROOT}/lib"
  "${OA_STAGE_ROOT}/include/open-axiom"
  "${OA_STAGE_ROOT}/share"
  "${OA_STAGE_ROOT}/doc/help"
  "${OA_STAGE_ROOT}/src/algebra"
)

# Targets staged into OA_STAGE_ROOT/bin/.
set(oa_stage_bin_targets  open-axiom hammer oa-lisp)

# Targets staged into OA_STAGE_ROOT/lib/.
set(oa_stage_lib_targets  open-axiom-core OpenAxiom spad)

# Build the COMMAND list programmatically.  CMake does not allow foreach
# inside add_custom_target, so we construct the commands externally and
# pass them via a helper script.  However, we can still use generator
# expressions and list the COMMANDs directly -- the lists here serve as
# the single source of truth and are referenced again by DEPENDS.
add_custom_target(stage-native-layout
  # -- Create the directory skeleton --
  COMMAND ${CMAKE_COMMAND} -E make_directory ${oa_stage_dirs}
  # -- Copy executables --
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "$<TARGET_FILE:open-axiom>"
    "$<TARGET_FILE:hammer>"
    "$<TARGET_FILE:oa-lisp>"
    "${OA_STAGE_ROOT}/bin/"
  # -- Copy static libraries --
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "$<TARGET_FILE:open-axiom-core>"
    "$<TARGET_FILE:OpenAxiom>"
    "$<TARGET_FILE:spad>"
    "${OA_STAGE_ROOT}/lib/"
  # -- Copy headers --
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/include/open-axiom.h"
    "${OA_STAGE_ROOT}/include/"
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    "${PROJECT_SOURCE_DIR}/src/include/open-axiom"
    "${OA_STAGE_ROOT}/include/open-axiom"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_BINARY_DIR}/config/openaxiom-c-macros.h"
    "${OA_STAGE_ROOT}/include/open-axiom/config"
  DEPENDS ${oa_stage_bin_targets} ${oa_stage_lib_targets}
  COMMENT "Stage native artifacts into an OpenAxiom-style runtime layout"
)

# phase1-native: just compile the three executables (no staging).
# Useful for quick compilation-only checks.
add_custom_target(phase1-native
  DEPENDS open-axiom hammer oa-lisp
  COMMENT "Build the initial native CMake scaffold targets"
)

# phase2-native: build everything AND populate the staging tree.
# This is the prerequisite for the Phase 3 Lisp chain because the Lisp
# compiler expects the native tools to already be in OA_STAGE_ROOT.
add_custom_target(phase2-native
  DEPENDS stage-native-layout
  COMMENT "Build and stage the completed Phase 2 native layer"
)
