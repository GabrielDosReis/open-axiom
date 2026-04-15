# ===========================================================================
# src/CMakeLists.txt -- OpenAxiom source-tree targets
# ===========================================================================
# This file defines the native C/C++ libraries and executables that form
# the foundation of the OpenAxiom build, plus the Lisp runtime compilation
# (Phase 3), and the boot translator and interpreter chain (Phase 4).
# Later PRs add:
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
#   G. Phase 4: Boot translator bootstrap and interpreter chain
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


# ===========================================================================
# G. Phase 4: Boot translator bootstrap and interpreter chain
# ===========================================================================
#
# The Boot translator (bootsys) is built through a three-stage bootstrap:
#
#   strap/   -- Compile cached .clisp files (checked into src/boot/strap/)
#               with the base Lisp image from Phase 3.  This produces
#               strap/bootsys -- the initial, trusted Boot translator.
#
#   stage1/  -- Use strap/bootsys to translate the .boot source files into
#               fresh .clisp files, then compile those with the base Lisp.
#               Produces stage1/bootsys.
#
#   stage2/  -- Use stage1/bootsys to translate .boot -> .clisp again and
#               compile.  Produces stage2/bootsys -- the final bootsys.
#
# The three-stage process ensures the translator is self-consistent:
# stage2/bootsys is produced by a bootsys that was itself produced from
# the same source files (stage1/bootsys), not from the potentially-stale
# cached .clisp files.
#
# The interpreter (interpsys) is then built by using bootsys to compile
# ~100 .boot and .lisp source files in src/interp/, with a carefully
# ordered dependency graph, and linking them into a standalone image.
#
# Key tools:
#   open-axiom --execpath=<lisp> --compile ...    Compile a .boot or .lisp
#   open-axiom --execpath=<lisp> --translate ...  Translate .boot -> .clisp
#   open-axiom --execpath=<lisp> --make ...       Link FASLs into an image
#
# The open-axiom driver (built in Phase 1) is the orchestrator for all
# Lisp-level operations -- it sets up the process environment, passes
# flags to the underlying Lisp, and manages load paths.
# ---------------------------------------------------------------------------

# -- Paths -----------------------------------------------------------------

set(oa_boot_srcdir  "${PROJECT_SOURCE_DIR}/src/boot")
set(oa_boot_builddir "${PROJECT_BINARY_DIR}/phase4/boot")
set(oa_interp_srcdir "${PROJECT_SOURCE_DIR}/src/interp")
set(oa_interp_builddir "${PROJECT_BINARY_DIR}/phase4/interp")

# The open-axiom driver executable -- orchestrates all Lisp operations.
set(OA_DRIVER "$<TARGET_FILE:open-axiom>")

# The base Lisp image from Phase 3 (installed into the staging tree).
set(OA_LISPSYS "${OA_STAGE_ROOT}/bin/lisp")

# -- Boot module list and dependency order ---------------------------------
# The Boot translator is composed of these 8 modules, listed in dependency
# order.  Each module may depend on earlier ones in this list.
#
# Module          Depends on
# -------         ----------
# utility         (none)
# tokens          utility
# includer        tokens
# scanner         tokens, includer
# pile            scanner, includer
# ast             includer
# parser          ast, scanner, includer
# translator      parser, ast, pile, scanner, includer

set(oa_boot_modules
  utility tokens includer scanner pile ast parser translator
)

# Create the stage directories.
file(MAKE_DIRECTORY
  "${oa_boot_builddir}/strap"
  "${oa_boot_builddir}/stage1"
  "${oa_boot_builddir}/stage2"
  "${oa_interp_builddir}"
)

# ---------------------------------------------------------------------------
# Helper function: oa_boot_compile_module
# ---------------------------------------------------------------------------
# Compiles a single .clisp file into a FASL (and LNK for ECL) using the
# base Lisp image.  The --load-directory flag tells the driver where to
# find previously compiled modules that this one depends on.
#
# Arguments:
#   stage       One of: strap, stage1, stage2
#   module      Module name without extension (e.g. "utility")
#   clisp_file  Absolute path to the .clisp source file
#   deps        List of FASL files this module depends on

function(oa_boot_compile_module stage module clisp_file deps)
  set(outdir "${oa_boot_builddir}/${stage}")
  set(fasl   "${outdir}/${module}.${OA_LISP_FASL_EXT}")
  set(outputs "${fasl}")
  if(OA_LISP_FLAVOR STREQUAL "ecl")
    list(APPEND outputs "${outdir}/${module}.${OA_LISP_LNK_EXT}")
  endif()

  add_custom_command(
    OUTPUT ${outputs}
    COMMAND ${OA_DRIVER}
      --execpath=${OA_LISPSYS}
      --output=${fasl}
      --compile
      --load-directory=${outdir}
      "${clisp_file}"
    WORKING_DIRECTORY "${outdir}"
    DEPENDS ${clisp_file} ${deps}
    COMMENT "[boot/${stage}] Compile ${module}.clisp"
    VERBATIM
  )
endfunction()

# ---------------------------------------------------------------------------
# Helper function: oa_boot_translate_module
# ---------------------------------------------------------------------------
# Uses a bootsys image to translate a .boot source file into .clisp.
#
# Arguments:
#   stage       One of: stage1, stage2
#   module      Module name without extension
#   bootsys     Path to the bootsys image to use for translation
#   deps        List of files this translation depends on

function(oa_boot_translate_module stage module bootsys deps)
  set(boot_src "${oa_boot_srcdir}/${module}.boot")
  set(outdir   "${oa_boot_builddir}/${stage}")
  set(clisp    "${outdir}/${module}.clisp")

  add_custom_command(
    OUTPUT "${clisp}"
    COMMAND ${OA_DRIVER}
      --execpath=${bootsys}
      --translate
      --import=skip
      --output=${clisp}
      "${boot_src}"
    WORKING_DIRECTORY "${outdir}"
    DEPENDS ${bootsys} ${boot_src} ${deps}
    COMMENT "[boot/${stage}] Translate ${module}.boot -> ${module}.clisp"
    VERBATIM
  )
endfunction()

# ---------------------------------------------------------------------------
# Helper function: oa_boot_link_image
# ---------------------------------------------------------------------------
# Links compiled FASL modules into a bootsys executable image.
#
# Arguments:
#   stage       One of: strap, stage1, stage2
#   fasl_list   List of FASL files to link

function(oa_boot_link_image stage fasl_list)
  set(outdir  "${oa_boot_builddir}/${stage}")
  set(image   "${outdir}/bootsys${CMAKE_EXECUTABLE_SUFFIX}")

  add_custom_command(
    OUTPUT "${image}"
    COMMAND ${OA_DRIVER}
      --execpath=${OA_LISPSYS}
      --make
      --main="|AxiomCore|::|topLevel|"
      --system=${OA_STAGE_ROOT}
      --prologue=(pushnew\ :open-axiom-boot\ *features*)
      --output=${image}
      --load-directory=${outdir}
      ${fasl_list}
    WORKING_DIRECTORY "${outdir}"
    DEPENDS ${fasl_list} stage-lisp-runtime
    COMMENT "[boot/${stage}] Link bootsys image"
    VERBATIM
  )
endfunction()


# ---------------------------------------------------------------------------
# Helper function: oa_boot_build_stage
# ---------------------------------------------------------------------------
# Builds one complete boot stage: translate (if prev_bootsys is given),
# compile, and link into a bootsys image.  Eliminates the repetition
# between stage1 and stage2, which are identical except for which
# bootsys drives the translation step.
#
# For the strap stage, clisp_source_dir points at the cached .clisp files
# and prev_bootsys is empty (no translation step needed).
#
# Arguments:
#   stage            Directory name: strap, stage1, stage2
#   clisp_source_dir Where to find .clisp inputs (only for strap)
#   prev_bootsys     Path to the bootsys that translates .boot -> .clisp.
#                    If empty, skip the translation step (strap mode).
#
# Sets in PARENT_SCOPE:
#   oa_${stage}_bootsys  Path to the bootsys image produced by this stage.

function(oa_boot_build_stage stage clisp_source_dir prev_bootsys)
  set(fasls)
  foreach(mod ${oa_boot_modules})
    if(prev_bootsys)
      # Translation stage: .boot -> .clisp via the previous bootsys.
      oa_boot_translate_module(${stage} "${mod}"
        "${prev_bootsys}" "${prev_bootsys}")
      set(clisp_src "${oa_boot_builddir}/${stage}/${mod}.clisp")
    else()
      # Strap: use cached .clisp files from the source tree.
      set(clisp_src "${clisp_source_dir}/${mod}.clisp")
    endif()

    # Compile: .clisp -> FASL with cumulative dependency chain.
    oa_boot_compile_module(${stage} "${mod}" "${clisp_src}" "${fasls}")
    list(APPEND fasls
      "${oa_boot_builddir}/${stage}/${mod}.${OA_LISP_FASL_EXT}")
  endforeach()

  # Link FASLs into a bootsys image.
  oa_boot_link_image(${stage} "${fasls}")
  set(oa_${stage}_bootsys
    "${oa_boot_builddir}/${stage}/bootsys${CMAKE_EXECUTABLE_SUFFIX}"
    PARENT_SCOPE)
endfunction()


# ===========================================================================
# Three-stage Boot bootstrap
# ===========================================================================
# Each stage is a single function call.  The strap stage compiles from
# cached .clisp; stage1 and stage2 translate fresh .clisp from .boot
# using the previous stage's bootsys.  The loop-over-stages pattern
# ensures all three stages share exactly the same compile/link logic.

# Stage strap: compile cached .clisp with the base Lisp image.
oa_boot_build_stage(strap "${oa_boot_srcdir}/strap" "")

# Stage 1: translate .boot with strap/bootsys, compile, link.
oa_boot_build_stage(stage1 "" "${oa_strap_bootsys}")

# Stage 2: translate .boot with stage1/bootsys, compile, link.
# Produces the final, self-consistent Boot translator.
oa_boot_build_stage(stage2 "" "${oa_stage1_bootsys}")


# -- Stage the final bootsys into OA_STAGE_ROOT --
add_custom_target(stage-bootsys
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${oa_stage2_bootsys}"
    "${OA_STAGE_ROOT}/bin/bootsys${CMAKE_EXECUTABLE_SUFFIX}"
  DEPENDS "${oa_stage2_bootsys}"
  COMMENT "Stage the final Boot translator (bootsys)"
)

# ECL needs a linkset file listing the object files for bootsys.
if(OA_LISP_FLAVOR STREQUAL "ecl")
  set(oa_boot_lnk_list)
  foreach(mod ${oa_boot_modules})
    list(APPEND oa_boot_lnk_list "\"${mod}.${OA_LISP_LNK_EXT}\"")
  endforeach()
  string(JOIN " " oa_boot_linkset_content ${oa_boot_lnk_list})

  add_custom_command(
    TARGET stage-bootsys POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/boot"
    COMMAND ${CMAKE_COMMAND}
      -Dfile=${OA_STAGE_ROOT}/boot/linkset
      -Dcontent=(${oa_boot_linkset_content})
      -P "${PROJECT_SOURCE_DIR}/cmake/WriteFile.cmake"
    COMMENT "Generate ECL linkset for bootsys"
    VERBATIM
  )
  foreach(mod ${oa_boot_modules})
    add_custom_command(
      TARGET stage-bootsys POST_BUILD
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${oa_boot_builddir}/stage2/${mod}.${OA_LISP_LNK_EXT}"
        "${OA_STAGE_ROOT}/boot/"
      VERBATIM
    )
  endforeach()
endif()

add_custom_target(phase4-boot
  DEPENDS stage-bootsys
  COMMENT "Build the Phase 4 Boot translator (three-stage bootstrap)"
)


# ===========================================================================
# Phase 4b: Interpreter (interpsys)
# ===========================================================================
# The interpreter is built by compiling ~100 .boot and .lisp files from
# src/interp/ using bootsys, then linking them into a standalone image
# called interpsys.  This image is the minimal OpenAxiom system needed
# to compile algebra in Phase 5.
#
# The dependency graph below is transcribed from src/interp/Makefile.in.
# Each entry records: <module>.<ext> depends on [<dep1> <dep2> ...].
# The generic compile rule is:
#   open-axiom --execpath=bootsys --compile --syslib=<libdir>
#       --output=<out> --load-directory=. <source>
# ---------------------------------------------------------------------------

set(OA_BOOTSYS "${OA_STAGE_ROOT}/bin/bootsys${CMAKE_EXECUTABLE_SUFFIX}")

# ---------------------------------------------------------------------------
# Helper: oa_interp_compile
# ---------------------------------------------------------------------------
# Compiles a single .boot or .lisp file from src/interp/ into a FASL.
#
# Arguments:
#   module    Base name (e.g. "boot-pkg")
#   ext       Source extension: "boot" or "lisp"
#   ...       Names of modules this one depends on (variable-length)
#
# Side effect:
#   Appends the FASL path to oa_interp_all_fasls in the PARENT scope.
#   This eliminates the need to list every module a second time when
#   assembling the link command -- the compile declarations themselves
#   serve as the single source of truth for the module inventory.

function(oa_interp_compile module ext)
  set(src  "${oa_interp_srcdir}/${module}.${ext}")
  set(fasl "${oa_interp_builddir}/${module}.${OA_LISP_FASL_EXT}")
  set(outputs "${fasl}")
  if(OA_LISP_FLAVOR STREQUAL "ecl")
    list(APPEND outputs "${oa_interp_builddir}/${module}.${OA_LISP_LNK_EXT}")
  endif()

  # Build the dependency list from module names passed as extra arguments.
  set(dep_fasls)
  foreach(dep ${ARGN})
    list(APPEND dep_fasls "${oa_interp_builddir}/${dep}.${OA_LISP_FASL_EXT}")
  endforeach()

  add_custom_command(
    OUTPUT ${outputs}
    COMMAND ${OA_DRIVER}
      --execpath=${OA_BOOTSYS}
      --syslib=${OA_STAGE_ROOT}/lib
      --compile
      --output=${fasl}
      --load-directory=${oa_interp_builddir}
      "${src}"
    WORKING_DIRECTORY "${oa_interp_builddir}"
    DEPENDS "${src}" ${OA_BOOTSYS} ${dep_fasls}
    COMMENT "[interp] Compile ${module}.${ext}"
    VERBATIM
  )

  # Auto-accumulate: every module that is compiled becomes part of the
  # final link set.  The caller never needs a separate OBJS list.
  set(tmp "${oa_interp_all_fasls}")
  list(APPEND tmp "${fasl}")
  set(oa_interp_all_fasls "${tmp}" PARENT_SCOPE)
endfunction()

# Accumulator for all interp FASLs -- populated by oa_interp_compile().
set(oa_interp_all_fasls)

# ---------------------------------------------------------------------------
# Interpreter module dependency graph
# ---------------------------------------------------------------------------
# This is the complete dependency DAG from src/interp/Makefile.in.
# Each call declares: oa_interp_compile(<module> <ext> [<dep1> <dep2> ...])
# where <depN> are module names (not file paths).

# -- Foundation layer: package, types, low-level runtime --
oa_interp_compile(boot-pkg   lisp)
oa_interp_compile(types      boot  boot-pkg)
oa_interp_compile(hash       lisp  types)
oa_interp_compile(sys-constants boot types)
oa_interp_compile(sys-os     boot  sys-constants)
oa_interp_compile(io         boot  sys-constants)
oa_interp_compile(sys-globals boot sys-constants hash)
oa_interp_compile(sys-driver boot  types)
oa_interp_compile(vmlisp     lisp  types sys-globals)
oa_interp_compile(sys-utility boot vmlisp sys-constants hash)

# -- Diagnostics and core types --
oa_interp_compile(diagnostics boot sys-globals vmlisp)
oa_interp_compile(union       lisp vmlisp)
oa_interp_compile(ggreater    lisp vmlisp)
oa_interp_compile(sys-macros  lisp diagnostics union)
oa_interp_compile(lexing      boot sys-utility sys-macros io)

# -- Utility and infrastructure --
oa_interp_compile(unlisp      lisp sys-macros)
oa_interp_compile(daase       lisp sys-utility)
oa_interp_compile(util        lisp lexing)
oa_interp_compile(g-util      boot ggreater sys-macros daase)
oa_interp_compile(g-opt       boot g-util)
oa_interp_compile(c-util      boot g-opt)
oa_interp_compile(g-error     boot diagnostics g-util)
oa_interp_compile(g-timer     boot sys-macros g-util)
oa_interp_compile(g-cndata    boot sys-macros c-util)
oa_interp_compile(clam        boot g-timer)
oa_interp_compile(clammed     boot g-timer)
oa_interp_compile(slam        boot g-timer)
oa_interp_compile(nlib        lisp sys-macros sys-utility)
oa_interp_compile(pathname    boot nlib)
oa_interp_compile(compat      boot pathname)
oa_interp_compile(database    boot clam nlib cattable compat g-cndata c-util)
oa_interp_compile(debug       lisp sys-macros lexing)
oa_interp_compile(msgdb       boot g-util)
oa_interp_compile(msg         boot sys-macros astr)
oa_interp_compile(fname       lisp sys-macros)
oa_interp_compile(word        boot g-util)
oa_interp_compile(monitor     lisp sys-macros sys-utility)
oa_interp_compile(sfsfun-l    lisp sys-macros)
oa_interp_compile(sfsfun      boot sys-macros)
oa_interp_compile(lisp-backend boot sys-macros nlib c-util)

# -- Compiler support --
oa_interp_compile(simpbool    boot sys-macros)
oa_interp_compile(cattable    boot simpbool c-util)
oa_interp_compile(nrunfast    boot c-util)
oa_interp_compile(profile     boot sys-macros sys-utility c-util)
oa_interp_compile(nruncomp    boot profile simpbool nrunfast)
oa_interp_compile(define      boot g-error nruncomp database)
oa_interp_compile(category    boot c-util g-cndata)
oa_interp_compile(functor     boot category lisplib nruncomp)
oa_interp_compile(compiler    boot msgdb pathname define)
oa_interp_compile(lisplib     boot database debug)
oa_interp_compile(c-doc       boot c-util)
oa_interp_compile(htcheck     boot sys-driver sys-macros)

# -- New parser (interpreter front-end) --
oa_interp_compile(astr        boot vmlisp)
oa_interp_compile(dq          boot types)
oa_interp_compile(posit       boot sys-macros astr)
oa_interp_compile(serror      boot posit)
oa_interp_compile(cstream     boot sys-macros)
oa_interp_compile(cformat     boot unlisp posit)
oa_interp_compile(incl        boot cstream cformat)
oa_interp_compile(scan        boot incl dq sys-utility)
oa_interp_compile(pile        boot scan)
oa_interp_compile(ptrees      boot posit serror)
oa_interp_compile(macex       boot ptrees)
oa_interp_compile(cparse      boot ptrees)
oa_interp_compile(pf2sex      boot ptrees)
oa_interp_compile(i-parser    boot cparse pf2sex)
oa_interp_compile(postpar     boot sys-macros)
oa_interp_compile(parse       boot postpar)
oa_interp_compile(spad-parser boot parse lexing)
oa_interp_compile(packtran    boot sys-macros)

# -- Old parser and SPAD support --
oa_interp_compile(spad        lisp spad-parser postpar debug)
oa_interp_compile(record      boot nlib pathname)
oa_interp_compile(rulesets    boot vmlisp)
oa_interp_compile(newfort     boot sys-macros)
oa_interp_compile(server      boot sys-macros)
oa_interp_compile(termrw      boot sys-macros)
oa_interp_compile(match       boot sys-macros)
oa_interp_compile(format      boot sys-macros)
oa_interp_compile(buildom     boot sys-macros c-util nruncomp)
oa_interp_compile(setvars     boot sys-macros debug)
oa_interp_compile(setvart     boot sys-macros)
oa_interp_compile(fortcall    boot sys-macros)

# -- Interpreter core --
oa_interp_compile(i-util      boot c-util)
oa_interp_compile(i-object    boot i-util)
oa_interp_compile(i-intern    boot i-object ptrees)
oa_interp_compile(i-analy     boot i-object)
oa_interp_compile(i-resolv    boot i-object)
oa_interp_compile(i-coerfn    boot i-analy i-resolv)
oa_interp_compile(i-coerce    boot i-coerfn)
oa_interp_compile(i-eval      boot i-analy)
oa_interp_compile(i-funsel    boot i-coerfn)
oa_interp_compile(i-map       boot i-object)
oa_interp_compile(i-output    boot sys-utility sys-macros)
oa_interp_compile(i-special   boot i-analy)
oa_interp_compile(i-syscmd    boot i-object)
oa_interp_compile(i-toplev    boot i-analy)
oa_interp_compile(int-top     boot incl i-toplev unlisp)
oa_interp_compile(osyscmd     boot int-top)
oa_interp_compile(trace       boot debug)
oa_interp_compile(hypertex    boot types)

# -- HyperDoc / browser --
oa_interp_compile(ht-util     boot sys-macros)
oa_interp_compile(htsetvar    boot sys-macros)
oa_interp_compile(ht-root     boot ht-util)
oa_interp_compile(bc-util     boot ht-util c-util)
oa_interp_compile(bc-matrix   boot bc-util)
oa_interp_compile(bc-misc     boot bc-util)
oa_interp_compile(bc-solve    boot bc-matrix bc-misc)
oa_interp_compile(br-util     boot bc-util)
oa_interp_compile(br-search   boot bc-util)
oa_interp_compile(br-con      boot bc-util)
oa_interp_compile(br-op1      boot bc-util)
oa_interp_compile(br-op2      boot br-op1)
oa_interp_compile(br-data     boot bc-util nruncomp)
oa_interp_compile(br-prof     boot bc-util)
oa_interp_compile(br-saturn   boot bc-util)
oa_interp_compile(showimp     boot c-util nruncomp)
oa_interp_compile(alql        boot br-search)
oa_interp_compile(topics      boot sys-macros sys-utility)

# -- Interpreter FASL inventory (auto-accumulated) -------------------------
# oa_interp_all_fasls was populated by the oa_interp_compile() calls
# above.  Every module declared above is automatically included in the
# link set -- there is no separate OBJS list to keep in sync.
set(oa_interp_objs ${oa_interp_all_fasls})

# Handle the FFI module: delayed-FFI Lisps install sys-os separately;
# standard-linking Lisps include it directly in the object list (already
# covered above since sys-os is in the list).
if(oa_delay_ffi STREQUAL "yes")
  # Copy the FFI module to the interp staging directory so the image
  # can find it at load time.
  add_custom_command(
    OUTPUT "${OA_STAGE_ROOT}/interp/sys-os.${OA_LISP_FASL_EXT}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/interp"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${oa_interp_builddir}/sys-os.${OA_LISP_FASL_EXT}"
      "${OA_STAGE_ROOT}/interp/"
    DEPENDS "${oa_interp_builddir}/sys-os.${OA_LISP_FASL_EXT}"
    COMMENT "[interp] Stage delayed-FFI module sys-os"
    VERBATIM
  )
  set(oa_interp_ffi_staged "${OA_STAGE_ROOT}/interp/sys-os.${OA_LISP_FASL_EXT}")
else()
  set(oa_interp_ffi_staged)
endif()

# -- Stage the messages file needed by interpsys --
set(oa_msgs_src "${PROJECT_SOURCE_DIR}/src/doc/msgs/s2-us.msgs")
set(oa_msgs_staged "${OA_STAGE_ROOT}/share/msgs/s2-us.msgs")
add_custom_command(
  OUTPUT "${oa_msgs_staged}"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/share/msgs"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${oa_msgs_src}" "${oa_msgs_staged}"
  DEPENDS "${oa_msgs_src}"
  COMMENT "[interp] Stage message catalog s2-us.msgs"
  VERBATIM
)

# -- Link interpsys --
# interpsys is the minimal interpreter image needed to compile algebra.
# It is built by loading all compiled FASL modules into bootsys.
set(oa_interpsys "${oa_interp_builddir}/interpsys${CMAKE_EXECUTABLE_SUFFIX}")

add_custom_command(
  OUTPUT "${oa_interpsys}"
  COMMAND ${OA_DRIVER}
    --execpath=${OA_BOOTSYS}
    --syslib=${OA_STAGE_ROOT}/lib
    --system=${OA_STAGE_ROOT}/
    --prologue=(pushnew\ :open-axiom-basic-system\ *features*)
    --make
    --output=${oa_interpsys}
    --main="BOOT::|systemMain|"
    --load-directory=${oa_interp_builddir}
    ${oa_interp_objs}
  WORKING_DIRECTORY "${oa_interp_builddir}"
  DEPENDS ${oa_interp_objs} ${OA_BOOTSYS} "${oa_msgs_staged}" ${oa_interp_ffi_staged}
  COMMENT "[interp] Link interpsys image"
  VERBATIM
)

# -- Stage interpsys --
add_custom_target(stage-interpsys
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${oa_interpsys}"
    "${OA_STAGE_ROOT}/bin/interpsys${CMAKE_EXECUTABLE_SUFFIX}"
  DEPENDS "${oa_interpsys}" stage-bootsys
  COMMENT "Stage the interpreter (interpsys)"
)

add_custom_target(phase4-interpsys
  DEPENDS stage-interpsys
  COMMENT "Build and stage the Phase 4 interpreter (interpsys)"
)

# -- Umbrella Phase 4 target --
add_custom_target(phase4-boot-interp
  DEPENDS phase4-boot phase4-interpsys
  COMMENT "Build the complete Phase 4: Boot translator + interpreter"
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
