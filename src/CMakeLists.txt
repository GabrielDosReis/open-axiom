# ===========================================================================
# src/CMakeLists.txt -- OpenAxiom source-tree targets
# ===========================================================================
# This file defines the native C/C++ libraries and executables that form
# the foundation of the OpenAxiom build, plus the Lisp runtime compilation,
# the boot translator and interpreter chain, the algebra substrate,
# and the databases, HyperDoc data, and optional executables.
# This file also provides install rules, CMake export targets, and CPack
# packaging configuration.
#
# It is organised as follows:
#
#   A. Common include paths and blanket LANGUAGE CXX
#   B. Helper: oa_apply_build_flags -- shared warning / profiling / debug opts
#   C. Static libraries: open-axiom-core, OpenAxiom, spad
#   D. Executables: hammer, open-axiom, lisp
#   E. Lisp runtime compilation and image linking
#   F. Staging targets: stage-native-layout, all-lib, all-staging
#   G. Boot translator bootstrap and interpreter chain
#   H. Algebra substrate
#   P. Databases, HyperDoc data, and optional native executables
#   I. Install rules and CMake export targets
#   Q. CPack packaging
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

# Winsock -- needed for sockio-c on Windows.
if(WIN32)
  target_link_libraries(open-axiom-core PUBLIC wsock32 ws2_32)
endif()

# -- Shared variant of open-axiom-core (for delayed-FFI Lisps) --
# SBCL, Clozure CL, and CLISP use dlopen/LoadLibrary to load the C
# runtime at image startup (the "delayed FFI" strategy).  They expect
# a shared library -- open-axiom-core.dll/.so -- in the staging tree.
if(oa_use_dynamic_lib STREQUAL "yes")
  add_library(open-axiom-core-shared SHARED
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
  set_target_properties(open-axiom-core-shared PROPERTIES
    OUTPUT_NAME open-axiom-core
    ARCHIVE_OUTPUT_NAME open-axiom-core-import
    # Build directly into the staging tree so that downstream consumers
    # (interpsys, algebra bootstrap) always see the up-to-date DLL.
    RUNTIME_OUTPUT_DIRECTORY "${OA_STAGE_ROOT}/lib"
    LIBRARY_OUTPUT_DIRECTORY "${OA_STAGE_ROOT}/lib"
    ARCHIVE_OUTPUT_DIRECTORY "${OA_STAGE_ROOT}/lib"
  )
  target_include_directories(open-axiom-core-shared PUBLIC
    $<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}/src/include>
    $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/include>
    $<BUILD_INTERFACE:${PROJECT_BINARY_DIR}/config>
  )
  target_compile_features(open-axiom-core-shared PUBLIC cxx_std_20)
  oa_apply_build_flags(open-axiom-core-shared)
  # The headers gate __declspec(dllexport) on DLL_EXPORT (see open-axiom.h).
  # Libtool defines this automatically; replicate that here.
  target_compile_definitions(open-axiom-core-shared PRIVATE DLL_EXPORT)
  if(UNIX)
    target_link_libraries(open-axiom-core-shared PUBLIC m)
  endif()
  if(CMAKE_DL_LIBS)
    target_link_libraries(open-axiom-core-shared PUBLIC ${CMAKE_DL_LIBS})
  endif()
  if(WIN32)
    target_link_libraries(open-axiom-core-shared PUBLIC wsock32 ws2_32)
  endif()
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
# The terminal/pty sources are POSIX-only; exclude them on Windows.
set(oa_spad_sources
  lib/halloc.c
  lib/hash.c
)
if(NOT WIN32)
  list(APPEND oa_spad_sources
    lib/cursor.c
    lib/edin.c
    lib/fnct_key.c
    lib/openpty.c
    lib/prt.c
    lib/wct.c
  )
endif()

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
  # Build static libraries directly into the staging tree.
  set_target_properties(${lib} PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${OA_STAGE_ROOT}/lib"
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

# oa-lisp -- thin C++ driver that launches the Lisp image.  Built as
# "lisp" (noinst_PROGRAMS in Autotools); not installed.
add_executable(oa-lisp rt/driver.cc)
set_target_properties(oa-lisp PROPERTIES OUTPUT_NAME lisp)
target_link_libraries(oa-lisp PRIVATE OpenAxiom open-axiom-core)

# -- E. Lisp compilation and base-image linking ------------------------------
# The Lisp chain:
#   1. Generate core.lisp from the .in template (configure_file)
#   2. Compile core.lisp -> core.FASL  (+ core.o for ECL)
#   3. Link the FASL into a standalone image: base-lisp
#   4. Stage everything under OA_STAGE_ROOT for later phases
#
# The LISP_CMD variable below avoids repeating the multi-part
# `cmake -E env ...EXECUTABLE ...QUIET ...BATCH` invocation prefix.

set(oa_lisp_build_dir "${PROJECT_BINARY_DIR}/phase3/lisp")
set(oa_lisp_generated_core "${oa_lisp_build_dir}/core.lisp")
set(oa_lisp_generated_fasl "${oa_lisp_build_dir}/core.${OA_LISP_FASL_EXT}")
set(oa_lisp_generated_linkable "${oa_lisp_build_dir}/core.${OA_LISP_LNK_EXT}")
set(oa_lisp_generated_image "${oa_lisp_build_dir}/base-lisp${CMAKE_EXECUTABLE_SUFFIX}")

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
  # The Lisp |..| bar-escape syntax contains pipe characters that cmd.exe
  # interprets as shell pipes, so we write the link form to a file and
  # --load it instead of passing it via --eval.
  set(oa_lisp_link_script "${oa_lisp_build_dir}/link-base-lisp.lisp")
  file(WRITE "${oa_lisp_link_script}"
    "(load \"core\")\n(|AxiomCore|::|link| \"base-lisp${CMAKE_EXECUTABLE_SUFFIX}\" (quote ${oa_base_lisp_objects}) \"|AxiomCore|::|topLevel|\")\n")

  add_custom_command(
    OUTPUT "${oa_lisp_generated_image}"
    COMMAND ${OA_LISP_CMD}
            ${OA_LISP_EVAL_FLAG} "(load \"link-base-lisp.lisp\")"
    WORKING_DIRECTORY "${oa_lisp_build_dir}"
    DEPENDS "${oa_lisp_generated_fasl}"
    COMMENT "Link the OpenAxiom base Lisp image"
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
      "${OA_STAGE_ROOT}/bin/lisp${CMAKE_EXECUTABLE_SUFFIX}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${PROJECT_BINARY_DIR}/config/openaxiom-lisp-settings.cmake"
      "${OA_STAGE_ROOT}/share/open-axiom/cmake/"
    DEPENDS stage-native-layout "${oa_lisp_generated_image}"
    COMMENT "Stage the Lisp runtime artifacts"
  )

  # Autotools equivalent: all-lisp
  # Artifacts: core.fasl (compiled Lisp bootstrap) and base-lisp (standalone
  #   Lisp image linked from core.fasl).  Both are staged into
  #   OA_STAGE_ROOT/lib/ and OA_STAGE_ROOT/bin/ respectively.
  # Depends on: stage-native-layout (native libs + headers in staging tree).
  add_custom_target(all-lisp
    DEPENDS stage-lisp-runtime
    COMMENT "Build core.fasl and the base-lisp image; stage into runtime layout"
  )



# ===========================================================================
# G. Boot translator bootstrap and interpreter chain
# ===========================================================================
#
# The Boot translator (bootsys) is built through a three-stage bootstrap:
#
#   strap/   -- Compile cached .clisp files (checked into src/boot/strap/)
#               with the base Lisp image.  This produces
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

# The base Lisp image (installed into the staging tree).
set(OA_LISPSYS "${OA_STAGE_ROOT}/bin/lisp${CMAKE_EXECUTABLE_SUFFIX}")

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

  # Write a command file to avoid cmd.exe mangling of Lisp |..| bar-escapes.
  # Use file(GENERATE ...) so that generator expressions (e.g. OA_DRIVER)
  # are resolved at generation time rather than configure time.
  set(cmdfile "${outdir}/link-bootsys.cmdfile")
  string(JOIN "\n" _cmd_content
    "${OA_DRIVER}"
    "--execpath=${OA_LISPSYS}"
    "--make"
    "--main=|AxiomCore|::|topLevel|"
    "--system=${OA_STAGE_ROOT}"
    "--prologue=(pushnew :open-axiom-boot *features*)"
    "--output=${image}"
    "--load-directory=${outdir}"
    ${fasl_list}
  )
  file(GENERATE OUTPUT "${cmdfile}" CONTENT "${_cmd_content}\n")

  add_custom_command(
    OUTPUT "${image}"
    COMMAND ${CMAKE_COMMAND}
      -DCMDFILE=${cmdfile}
      -DWORKDIR=${outdir}
      -P ${PROJECT_SOURCE_DIR}/cmake/RunCommand.cmake
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
# Use add_custom_command(OUTPUT ...) so that Ninja knows the build rule
# that produces the staged bootsys file.  Interpreter modules depend on
# this file path (OA_BOOTSYS) and need it resolvable in the dep graph.
add_custom_command(
  OUTPUT "${OA_STAGE_ROOT}/bin/bootsys${CMAKE_EXECUTABLE_SUFFIX}"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${oa_stage2_bootsys}"
    "${OA_STAGE_ROOT}/bin/bootsys${CMAKE_EXECUTABLE_SUFFIX}"
  DEPENDS "${oa_stage2_bootsys}"
  COMMENT "Stage the final Boot translator (bootsys)"
  VERBATIM
)

# Named target wrapping the staged file, for human use and ECL POST_BUILD.
add_custom_target(stage-bootsys
  DEPENDS "${OA_STAGE_ROOT}/bin/bootsys${CMAKE_EXECUTABLE_SUFFIX}"
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

# Autotools equivalent: all-boot
# Artifacts: bootsys -- the Boot-to-Lisp translator.  Built via a three-stage
#   bootstrap (strap -> stage1 -> stage2), each compiled with the base-lisp
#   image from all-lisp.  The final stage2/bootsys is staged into
#   OA_STAGE_ROOT/bin/.
# Depends on: all-lisp (base-lisp image), oa-lisp (Lisp launcher).
add_custom_target(all-boot
  DEPENDS stage-bootsys
  COMMENT "Three-stage bootstrap of the Boot translator (bootsys)"
)


# ===========================================================================
# Phase 4b: Interpreter (interpsys)
# ===========================================================================
# The interpreter is built by compiling ~100 .boot and .lisp files from
# src/interp/ using bootsys, then linking them into a standalone image
# called interpsys.  This image is the minimal OpenAxiom system needed
# to compile algebra.
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

# setvart must be last: its top-level initializeSetVariables call reads
# variables defined in many other modules (e.g. $plainRTspecialCharacters
# from i-output).  Autotools places it last for the same reason.
oa_interp_compile(setvart     boot sys-macros)

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
  # The shared library is built directly into ${OA_STAGE_ROOT}/lib/ via
  # output directory properties, so no copy is needed.  Record the path
  # so that the interpsys link command declares a file dependency on it.
  set(oa_staged_shared_lib "$<TARGET_FILE:open-axiom-core-shared>")
else()
  set(oa_interp_ffi_staged)
  set(oa_staged_shared_lib)
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

# Write a command file to avoid cmd.exe mangling of Lisp |..| bar-escapes.
# Use file(GENERATE ...) so that generator expressions are resolved.
set(oa_interpsys_cmdfile "${oa_interp_builddir}/link-interpsys.cmdfile")
string(JOIN "\n" _interpsys_cmd
  "${OA_DRIVER}"
  "--execpath=${OA_BOOTSYS}"
  "--syslib=${OA_STAGE_ROOT}/lib"
  "--system=${OA_STAGE_ROOT}/"
  "--prologue=(pushnew :open-axiom-basic-system *features*)"
  "--make"
  "--output=${oa_interpsys}"
  "--main=BOOT::|systemMain|"
  "--load-directory=${oa_interp_builddir}"
  ${oa_interp_objs}
)
file(GENERATE OUTPUT "${oa_interpsys_cmdfile}" CONTENT "${_interpsys_cmd}\n")

add_custom_command(
  OUTPUT "${oa_interpsys}"
  COMMAND ${CMAKE_COMMAND}
    -DCMDFILE=${oa_interpsys_cmdfile}
    -DWORKDIR=${oa_interp_builddir}
    -P ${PROJECT_SOURCE_DIR}/cmake/RunCommand.cmake
  WORKING_DIRECTORY "${oa_interp_builddir}"
  DEPENDS ${oa_interp_objs} ${OA_BOOTSYS} "${oa_msgs_staged}"
    ${oa_interp_ffi_staged} ${oa_staged_shared_lib}
  COMMENT "[interp] Link interpsys image"
  VERBATIM
)

# -- Stage interpsys --
# Autotools equivalent: all-interpsys
# Artifacts: interpsys -- the minimal OpenAxiom interpreter image.  Built by
#   compiling ~100 .boot and .lisp source files from src/interp/ using
#   bootsys, then linking them into a standalone Lisp image.  interpsys is
#   the tool that compiles SPAD algebra source files.
# Depends on: all-boot (bootsys), stage-bootsys (bootsys in staging tree).
add_custom_target(all-interpsys
  DEPENDS "${oa_interpsys}" stage-bootsys
  COMMENT "Compile ~100 interpreter modules and link the interpsys image"
)

# Umbrella: boot translator + interpreter.
add_custom_target(all-boot-interp
  DEPENDS all-boot all-interpsys
  COMMENT "Build bootsys and interpsys (Boot translator + interpreter)"
)



# -- H. Algebra substrate ---------------------------------------------------
# The algebra is the mathematical heart of OpenAxiom.  It is compiled from
# SPAD source files (.spad) that are extracted from pamphlets (.spad.pamphlet)
# by the hammer tool.  The build pipeline has five stages:
#
#   1. Whole-file extraction: hammer --tangle -> <name>.spad (for initdb)
#   2. Per-constructor extraction: hammer --tangle="<chunk>" -> <ABBREV>.spad
#   3. initdb:    scan whole .spad files -> build constructor database
#   4. Bootstrap: three stages (strap-0/1/2) progressively compile categories
#                 and domains using per-constructor .spad files
#   5. Layers:    25 layers (0-24) compile the full algebra with layer N
#                 depending on all layers < N
#
# The Autotools build generates src/algebra/extract.mk at configure time
# by scanning pamphlets for chunk markers like:
#   <<category SETCAT SetCategory>>=
# We replicate this with cmake/OpenAxiomScanAlgebra.cmake, which scans
# the pamphlets and populates OA_ALGEBRA_CTOR_PAMPHLET_<ABBREV> and
# OA_ALGEBRA_CTOR_CHUNK_<ABBREV> for each constructor.
#
# Layer data and spad file lists: cmake/OpenAxiomAlgebraLayers.cmake
# Constructor scanning:           cmake/OpenAxiomScanAlgebra.cmake
# ===========================================================================

include(OpenAxiomAlgebraLayers)
include(OpenAxiomScanAlgebra)

# -- Paths -----------------------------------------------------------------

set(oa_algebra_srcdir   "${PROJECT_SOURCE_DIR}/src/algebra")
set(oa_algebra_builddir "${PROJECT_BINARY_DIR}/phase5/algebra")
set(oa_algebra_outsrc   "${OA_STAGE_ROOT}/src/algebra")
set(oa_algebra_outdir   "${OA_STAGE_ROOT}/algebra")
set(oa_algebra_dbdir    "${PROJECT_SOURCE_DIR}/src/share/algebra")

# bootsys is needed for compiling initdb.clisp -> initdb.fasl
set(oa_bootsys "${OA_STAGE_ROOT}/bin/bootsys${CMAKE_EXECUTABLE_SUFFIX}")

# -- Scan algebra sources for constructors and file lists ------------------
# This discovers all pamphlets and plain .spad files, extracts constructor
# abbreviations, and builds the constructor-to-source mapping.  It replaces
# the Autotools configure.ac logic that generates extract.mk, and extends
# it to handle plain .spad files.  See OpenAxiomScanAlgebra.cmake for the
# full strategy documentation.

oa_scan_algebra_sources("${oa_algebra_srcdir}")

# -- Build the catch-all layer for unassigned constructors -----------------
# Must be called after oa_scan_algebra_sources() so that OA_ALGEBRA_CONSTRUCTORS
# is populated.  See OpenAxiomAlgebraLayers.cmake for the catch-all strategy.

oa_algebra_build_catchall()

# -- Paths for initdb (needed by oa_spad_compile_prefix) -------------------
set(oa_initdb_clisp "${oa_algebra_builddir}/initdb.clisp")
set(oa_initdb_fasl  "${oa_algebra_builddir}/initdb.${OA_LISP_FASL_EXT}")

# -- Common SPAD compilation prefix ----------------------------------------
# These flags are shared by every algebra compilation command (strap stages
# and layer compilation).  Factoring them here avoids a 6-flag repetition
# in oa_algebra_strap_stage() and oa_algebra_compile_layer().

set(oa_spad_compile_prefix
  ${OA_DRIVER}
    --execpath=${oa_interpsys}
    --system=${OA_STAGE_ROOT}
    --initial-db=${oa_initdb_fasl}
    --sysdb=${oa_algebra_dbdir}/
    --system-algebra
    --compile
)


# ---------------------------------------------------------------------------
# Step 1: Whole-file extraction (for initdb)
# ---------------------------------------------------------------------------
# Produce a complete .spad file for each entry in the SOURCES manifest.
# These whole-file extracts are scanned by --build-initdb to build the
# constructor database.
#
# Two source types are handled:
#   - Pamphlets: extracted via hammer --tangle -> <name>.spad
#   - Plain .spad: copied to the build tree as-is (configure_file COPYONLY)
#
# The output list (oa_all_spadfiles) is consumed by the initdb step.

set(oa_all_spadfiles "")

# -- Pamphlets: extract whole file via hammer --
foreach(spad ${OA_ALGEBRA_PAMPHLET_BASES})
  set(_pam "${oa_algebra_srcdir}/${spad}.spad.pamphlet")
  set(_out "${oa_algebra_outsrc}/${spad}.spad")

  add_custom_command(
    OUTPUT "${_out}"
    COMMAND $<TARGET_FILE:hammer>
      --tangle
      --output=${_out}
      "${_pam}"
    DEPENDS hammer "${_pam}"
    COMMENT "[algebra] Extract ${spad}.spad (whole file from pamphlet)"
    VERBATIM
  )
  list(APPEND oa_all_spadfiles "${_out}")
endforeach()

# -- Plain .spad: copy to the build tree --
# Plain .spad files need no extraction.  We copy them to the same output
# directory so that initdb scanning sees a uniform layout.
foreach(spad ${OA_ALGEBRA_PLAIN_SPADFILES})
  set(_src "${oa_algebra_srcdir}/${spad}.spad")
  set(_out "${oa_algebra_outsrc}/${spad}.spad")

  add_custom_command(
    OUTPUT "${_out}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_src}" "${_out}"
    DEPENDS "${_src}"
    COMMENT "[algebra] Copy ${spad}.spad (plain file)"
    VERBATIM
  )
  list(APPEND oa_all_spadfiles "${_out}")
endforeach()

# Autotools equivalent: part of 'all-algebra' (extract.mk)
# Artifacts: one .spad file per source in ${OA_STAGE_ROOT}/src/algebra/.
#   Pamphlet sources are extracted via `hammer --tangle`; plain .spad files
#   are copied directly.  These are the "whole" files used by initdb.
add_custom_target(all-extract-wholefile
  DEPENDS ${oa_all_spadfiles}
  COMMENT "Extract/copy whole .spad source files into the staging algebra dir"
)


# ---------------------------------------------------------------------------
# Step 2: Per-constructor extraction (for compilation)
# ---------------------------------------------------------------------------
# Each constructor gets its own .spad file in the build directory.  The
# SPAD compiler compiles one constructor at a time from these files.
#
# Two source types are handled:
#   - Pamphlet constructors: extracted via hammer --tangle="<chunk>"
#     For example, TYPE.spad is extracted from coerce.spad.pamphlet
#     using --tangle="category TYPE Type".
#   - Plain .spad constructors: the source file is already a valid .spad
#     file, so we copy it to the build tree under the constructor's name.
#     If the file contains multiple constructors, each gets its own copy
#     (the compiler selects the right one by abbreviation).
#
# The source type for each constructor is recorded in OA_ALGEBRA_CTOR_SOURCE_<A>
# by the scanner (see OpenAxiomScanAlgebra.cmake).

set(oa_all_ctor_spadfiles "")
file(MAKE_DIRECTORY "${oa_algebra_builddir}")

foreach(ctor ${OA_ALGEBRA_CONSTRUCTORS})
  set(_out "${oa_algebra_builddir}/${ctor}.spad")
  set(_source_type "${OA_ALGEBRA_CTOR_SOURCE_${ctor}}")

  if(_source_type STREQUAL "pamphlet")
    # Pamphlet constructor: extract via chunk name.
    set(_pambase "${OA_ALGEBRA_CTOR_PAMPHLET_${ctor}}")
    set(_chunk   "${OA_ALGEBRA_CTOR_CHUNK_${ctor}}")
    set(_pam     "${oa_algebra_srcdir}/${_pambase}.spad.pamphlet")

    # On Windows+Ninja, chunk names like "domain DROPT DrawOption"
    # contain spaces.  With VERBATIM, CMake quotes the whole argument
    # as "--tangle=domain DROPT DrawOption" which conflicts with the
    # outer cmd /C "..." wrapping.  Without VERBATIM, we embed the
    # quotes after the = so cmd.exe sees --tangle="domain ..." and
    # strips only the outermost pair.
    add_custom_command(
      OUTPUT "${_out}"
      COMMAND $<TARGET_FILE:hammer>
        "--tangle=\"${_chunk}\""
        --output=${_out}
        "${_pam}"
      DEPENDS hammer "${_pam}"
      COMMENT "[algebra] Extract ${ctor}.spad from ${_pambase}.spad.pamphlet"
    )

  elseif(_source_type STREQUAL "plain")
    # Plain .spad constructor: copy the source file.
    set(_sfbase "${OA_ALGEBRA_CTOR_SPADFILE_${ctor}}")
    set(_src    "${oa_algebra_srcdir}/${_sfbase}.spad")

    add_custom_command(
      OUTPUT "${_out}"
      COMMAND ${CMAKE_COMMAND} -E copy_if_different "${_src}" "${_out}"
      DEPENDS "${_src}"
      COMMENT "[algebra] Copy ${ctor}.spad from ${_sfbase}.spad (plain)"
      VERBATIM
    )

  else()
    message(FATAL_ERROR
      "Constructor ${ctor} has unknown source type '${_source_type}'. "
      "Expected 'pamphlet' or 'plain'.")
  endif()

  list(APPEND oa_all_ctor_spadfiles "${_out}")
endforeach()

# Autotools equivalent: part of 'all-algebra' (extract.mk)
# Artifacts: one .spad file per constructor in the build tree.  These are
#   the per-constructor fragments consumed by the SPAD compiler during
#   algebra bootstrap (all-algstrap) and layer compilation (all-algebra-layers).
add_custom_target(all-extract
  DEPENDS all-extract-wholefile ${oa_all_ctor_spadfiles}
  COMMENT "Extract per-constructor .spad source files (pamphlet + plain)"
)


# ---------------------------------------------------------------------------
# Step 3: Build initdb
# ---------------------------------------------------------------------------
# initdb is a constructor database built by scanning the whole-file .spad
# extracts.  It maps constructor abbreviations to source files and is
# required by every subsequent compilation step.
#
# Two stages: (a) scan .spad files -> initdb.clisp, (b) compile -> initdb.fasl

add_custom_command(
  OUTPUT "${oa_initdb_clisp}"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${oa_algebra_builddir}"
  COMMAND ${OA_DRIVER}
    --execpath=${oa_interpsys}
    --system=${OA_STAGE_ROOT}
    --spad-srcdir=${oa_algebra_outsrc}
    --output=${oa_initdb_clisp}
    --build-initdb
  WORKING_DIRECTORY "${oa_algebra_builddir}"
  DEPENDS ${oa_interpsys} ${oa_all_spadfiles}
  COMMENT "[algebra] Build initdb (scan whole .spad files)"
  VERBATIM
)

add_custom_command(
  OUTPUT "${oa_initdb_fasl}"
  COMMAND ${OA_DRIVER}
    --execpath=${oa_bootsys}
    --syslib=${OA_STAGE_ROOT}/lib
    --compile
    --output=${oa_initdb_fasl}
    --load-directory=${PROJECT_BINARY_DIR}/phase4/interp
    "${oa_initdb_clisp}"
  WORKING_DIRECTORY "${oa_algebra_builddir}"
  DEPENDS "${oa_initdb_clisp}" "${oa_bootsys}"
  COMMENT "[algebra] Compile initdb.clisp -> initdb.${OA_LISP_FASL_EXT}"
  VERBATIM
)

# Autotools equivalent: all-initdb
# Artifacts: initdb.${OA_LISP_FASL_EXT} -- a compiled Lisp file containing
#   the initial constructor database.  This file is loaded by interpsys
#   before any algebra compilation and tells the interpreter which
#   constructors exist and what operations they export.
# Depends on: interpsys (to compile), bootsys (for execution), extracted
#   whole .spad files (scanned for constructor metadata).
add_custom_target(all-initdb
  DEPENDS "${oa_initdb_fasl}"
  COMMENT "Build initdb.fasl -- the initial algebra constructor database"
)


# ---------------------------------------------------------------------------
# Step 4: Algebra bootstrap (strap-0, strap-1, strap-2)
# ---------------------------------------------------------------------------
# Bootstrap constructor lists -- extracted from Makefile.am strap-0/1/2.
include(OpenAxiomBootstrapOrder)

# ---------------------------------------------------------------------------
# Three progressive bootstrap stages compile categories and domains using
# the per-constructor .spad files.  Each constructor gets its own
# compilation invocation, exactly as in the Autotools build:
#
#   strap-0: --bootstrap (compile for exports only, skip defaults)
#   strap-1: --strap=strap-0 (recompile with strap-0 artifacts)
#   strap-2: --strap=strap-1 (recompile with strap-1 artifacts)
#
# The BOOTSTRAP command compiles a single per-constructor .spad file and
# produces a single FASL in the strap directory.

# Helper: oa_algebra_strap_deps -- compute the dependency for a strap stage.
#
# Stage 0 depends on initdb+interpsys; stage N>0 depends on the previous
# strap stage's stamp file.

function(oa_algebra_strap_deps stage outvar)
  if(stage EQUAL 0)
    set(${outvar} "${oa_initdb_fasl}" "${oa_interpsys}" PARENT_SCOPE)
  else()
    math(EXPR _prev "${stage} - 1")
    set(${outvar} "${oa_algebra_builddir}/strap-${_prev}.stamp" PARENT_SCOPE)
  endif()
endfunction()

# Helper function: compile one strap stage.
# Iterates over the constructors for that stage as defined in Makefile.am.
#
# Usage: oa_algebra_strap_stage(<stage> <flags...>)
#   <stage>: 0, 1, or 2
#   <flags>: additional flags (e.g. "--bootstrap" or "--strap=strap-0")

function(oa_algebra_strap_stage stage)
  set(_extra_flags ${ARGN})
  set(_strapdir "${oa_algebra_builddir}/strap-${stage}")
  set(_stamp   "${oa_algebra_builddir}/strap-${stage}.stamp")

  set(_commands "")
  list(APPEND _commands
    COMMAND ${CMAKE_COMMAND} -E make_directory "${_strapdir}"
  )

  # Compile each per-constructor .spad file through the bootstrap compiler.
  # The list comes from oa_algebra_strap_<N> (OpenAxiomBootstrapOrder.cmake).
  foreach(ctor ${oa_algebra_strap_${stage}})
    list(APPEND _commands
      COMMAND ${oa_spad_compile_prefix}
        --sysalg=${_strapdir}
        ${_extra_flags}
        --output=${_strapdir}/${ctor}.${OA_LISP_FASL_EXT}
        "${oa_algebra_builddir}/${ctor}.spad"
    )
  endforeach()

  list(APPEND _commands
    COMMAND ${CMAKE_COMMAND} -E touch "${_stamp}"
  )

  # Previous stage dependency
  oa_algebra_strap_deps(${stage} _deps)

  add_custom_command(
    OUTPUT "${_stamp}"
    ${_commands}
    WORKING_DIRECTORY "${oa_algebra_builddir}"
    DEPENDS ${_deps} ${oa_all_ctor_spadfiles}
    COMMENT "[algebra] Bootstrap strap-${stage}"
    VERBATIM
  )
endfunction()

# Stage 0: bootstrap (categories compiled for exports only)
oa_algebra_strap_stage(0 --bootstrap)

# Stage 1: recompile with strap-0 as the algebra base
oa_algebra_strap_stage(1 --strap=${oa_algebra_builddir}/strap-0 --optimize=3)

# Stage 2: recompile with strap-1 as the algebra base
oa_algebra_strap_stage(2 --strap=${oa_algebra_builddir}/strap-1 --optimize=3)

set(oa_strap_stamp "${oa_algebra_builddir}/strap-2.stamp")

# Autotools equivalent: all-algstrap
# Artifacts: strap-0/, strap-1/, strap-2/ directories, each containing ~200-400
#   compiled .${OA_LISP_FASL_EXT} files.  The three stages progressively
#   refine category/domain exports until the algebra is self-consistent.
#   Stage 0 compiles with --bootstrap (category exports only).
#   Stage 1 recompiles against strap-0 with full optimization.
#   Stage 2 recompiles against strap-1 with full optimization.
# Depends on: all-interpsys, all-extract (per-constructor .spad files).
add_custom_target(all-algstrap
  DEPENDS "${oa_strap_stamp}"
  COMMENT "Three-stage algebra bootstrap (strap-0 -> strap-1 -> strap-2)"
)


# ---------------------------------------------------------------------------
# Step 5: Full algebra compilation in layers
# ---------------------------------------------------------------------------
# Each constructor gets its own add_custom_command producing a real FASL
# output file.  Constructors within a layer can build in parallel; the
# layer ordering is enforced by target-level dependencies:
#
#   algebra-layer-0  depends on  all-algstrap
#   algebra-layer-N  depends on  algebra-layer-(N-1)
#
# The number of layers is dynamic: 25 explicit hand-curated layers, plus
# a catch-all layer if any constructors were not assigned to an explicit
# layer (see oa_algebra_build_catchall in OpenAxiomAlgebraLayers.cmake).
#
# This replaces the Autotools stamp-file pattern with native CMake target
# dependencies, giving Ninja full intra-layer parallelism while preserving
# the inter-layer ordering constraint.
#
# The SPAD compiler processes a per-constructor .spad file and produces
# <CTOR>.NRLIB/code.<FASLEXT>.  For category constructors with default
# definitions, it may also produce <CTOR>-.NRLIB/code.<FASLEXT> (the
# default package).  Whether a default package is generated is only
# known at compile time, so BYPRODUCTS declares it as a possible output
# without requiring it to exist.

file(MAKE_DIRECTORY "${oa_algebra_outdir}")

function(oa_algebra_compile_layer layer_idx)
  # Previous layer target (layer 0 depends on all-algstrap)
  if(layer_idx EQUAL 0)
    set(_prev_target all-algstrap)
  else()
    math(EXPR _prev "${layer_idx} - 1")
    set(_prev_target algebra-layer-${_prev})
  endif()

  set(_ctors ${oa_algebra_layer_${layer_idx}})
  set(_layer_outputs "")

  foreach(ctor ${_ctors})
    set(_fasl "${oa_algebra_outdir}/${ctor}.${OA_LISP_FASL_EXT}")

    # Compiling a category FOO also produces a default-package FOO-.
    # Declare it as a BYPRODUCT so Ninja knows this command may create
    # it.  For constructors in OA_ALGEBRA_DEFAULT_PACKAGES (known to
    # produce a default package), the copy is mandatory -- the build
    # fails if the FASL is missing, matching the Autotools behaviour.
    # For all other constructors, CopyIfExists silently skips.
    list(FIND OA_ALGEBRA_DEFAULT_PACKAGES "${ctor}" _defpkg_known)
    if(NOT _defpkg_known EQUAL -1)
      # Known default-package producer: mandatory copy (fails if missing).
      set(_defpkg_cmd
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
          "${oa_algebra_builddir}/${ctor}-.NRLIB/code.${OA_LISP_FASL_EXT}"
          "${oa_algebra_outdir}/${ctor}-.${OA_LISP_FASL_EXT}")
    else()
      # Not a known producer: best-effort copy (silently skips if absent).
      set(_defpkg_cmd
        COMMAND ${CMAKE_COMMAND}
          -Dsrc=${oa_algebra_builddir}/${ctor}-.NRLIB/code.${OA_LISP_FASL_EXT}
          -Ddst=${oa_algebra_outdir}/${ctor}-.${OA_LISP_FASL_EXT}
          -P ${PROJECT_SOURCE_DIR}/cmake/CopyIfExists.cmake)
    endif()

    add_custom_command(
      OUTPUT "${_fasl}"
      BYPRODUCTS "${oa_algebra_outdir}/${ctor}-.${OA_LISP_FASL_EXT}"
      COMMAND ${oa_spad_compile_prefix}
        --strap=${oa_algebra_builddir}/strap-2
        --optimize=3
        "${oa_algebra_builddir}/${ctor}.spad"
      COMMAND ${CMAKE_COMMAND} -E copy_if_different
        "${oa_algebra_builddir}/${ctor}.NRLIB/code.${OA_LISP_FASL_EXT}"
        "${_fasl}"
      ${_defpkg_cmd}
      WORKING_DIRECTORY "${oa_algebra_builddir}"
      DEPENDS "${oa_algebra_builddir}/${ctor}.spad" ${_prev_target}
      COMMENT "[algebra] Compile ${ctor} (layer ${layer_idx})"
      VERBATIM
    )
    list(APPEND _layer_outputs "${_fasl}")
  endforeach()

  add_custom_target(algebra-layer-${layer_idx}
    DEPENDS ${_layer_outputs}
    COMMENT "[algebra] Layer ${layer_idx} complete"
  )
endfunction()

foreach(layer_idx ${OA_ALGEBRA_LAYER_INDICES})
  oa_algebra_compile_layer(${layer_idx})
endforeach()

math(EXPR _last_layer "${OA_ALGEBRA_NUM_LAYERS} - 1")

# Autotools equivalent: all-algebra (algebra compilation portion)
# Artifacts: one .${OA_LISP_FASL_EXT} per constructor, organized in layers.
#   Constructors within the same layer compile in parallel; inter-layer
#   dependencies enforce ordering (layer N depends on layer N-1).
#   Layer 0 depends on all-algstrap (the bootstrap result).
add_custom_target(all-algebra-layers
  DEPENDS algebra-layer-${_last_layer}
  COMMENT "Compile the full OpenAxiom algebra (${OA_ALGEBRA_NUM_LAYERS} layers)"
)

# Autotools equivalent: all-algebra
# Umbrella target for the entire algebra substrate.  Depends on
# all-algebra-layers and (eventually) all-share and all-hyper-pre.
add_custom_target(all-algebra
  DEPENDS all-algebra-layers
  COMMENT "Build the complete OpenAxiom algebra (bootstrap + all layers)"
)


# ===========================================================================
# Database generation and HyperDoc data staging
# ===========================================================================
# These targets require interpsys and belong inside the
# Lisp guard.  Database generation builds the .daase files consumed by
# the interpreter at runtime; HyperDoc data staging copies pages,
# bitmaps, and viewports, then builds ht.db via the htadd tool.
# ---------------------------------------------------------------------------

# -- Database generation --
# Runs interpsys --build-databases to produce the .daase database files,
# libdb.text, comdb.text, and the USERS/DEPENDENTS indices.  These are
# required for the interpreter to look up algebra constructors at runtime.

set(oa_database_targets
  "${OA_STAGE_ROOT}/algebra/interp.daase"
  "${OA_STAGE_ROOT}/algebra/browse.daase"
  "${OA_STAGE_ROOT}/algebra/categor.daase"
  "${OA_STAGE_ROOT}/algebra/operation.daase"
  "${OA_STAGE_ROOT}/algebra/compress.daase"
)

add_custom_command(
  OUTPUT ${oa_database_targets}
  COMMAND ${CMAKE_COMMAND} -E make_directory
    "${OA_STAGE_ROOT}/algebra"
    "${OA_STAGE_ROOT}/algebra/USERS.DAASE"
    "${OA_STAGE_ROOT}/algebra/DEPENDENTS.DAASE"
  COMMAND ${OA_DRIVER}
    --execpath=${oa_interpsys}
    --system=${OA_STAGE_ROOT}
    --sysalg=${oa_algebra_outdir}
    --sysdb=${oa_algebra_dbdir}/
    --build-databases
  # Core .daase files.
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    interp.daase browse.daase categor.daase operation.daase compress.daase
    "${OA_STAGE_ROOT}/algebra/"
  # Auxiliary database artifacts: text indices and per-constructor
  # reverse-lookup tables used by )what, )show, and the browser.
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    libdb.text comdb.text
    "${OA_STAGE_ROOT}/algebra/"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    USERS.DAASE/index.KAF
    "${OA_STAGE_ROOT}/algebra/USERS.DAASE/"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    DEPENDENTS.DAASE/index.KAF
    "${OA_STAGE_ROOT}/algebra/DEPENDENTS.DAASE/"
  # Glossary files produced alongside databases (consumed by HyperDoc).
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    glosskey.text glossdef.text
    "${OA_STAGE_ROOT}/doc/"
  WORKING_DIRECTORY "${oa_algebra_builddir}"
  DEPENDS all-algebra-layers all-static-files "${oa_interpsys}"
  COMMENT "[databases] Build algebra databases (.daase files)"
  VERBATIM
)

# Autotools equivalent: all-databases
# Artifacts: interp.daase, browse.daase, categor.daase, operation.daase,
#   compress.daase, plus USERS.DAASE/index.KAF, DEPENDENTS.DAASE/index.KAF,
#   libdb.text, comdb.text, glosskey.text, glossdef.text.
#   These are the indexed databases that the interpreter queries at runtime
#   for constructor information, operation signatures, and category
#   relationships.  Built by running interpsys --build-databases.
# Depends on: all-algebra-layers, all-static-files, interpsys.
add_custom_target(all-databases
  DEPENDS ${oa_database_targets}
  COMMENT "Build .daase algebra databases and glossary indices"
)

# ---------------------------------------------------------------------------
# Step 7: Build AXIOMsys (the complete interpreter image)
# ---------------------------------------------------------------------------
# AXIOMsys is the final, installable OpenAxiom interpreter image.  Unlike
# interpsys (the minimal image used during the build), AXIOMsys includes:
#   - All algebra (via --sysalg pointing to the compiled algebra directory)
#   - exposed.lsp (which constructors are visible to users by default)
#   - initdb (the constructor database initialization object)
#   - makeint.lisp (build timestamp and image-linking code)
# The link command is nearly identical to interpsys but with:
#   --prologue="(pushnew :open-axiom-algebra-system *features*)"
#   --sysalg=<algebra dir>
#   plus exposed.fasl, initdb.fasl, and makeint.fasl as extra objects.

# -- 7a. Tangle exposed.lsp from pamphlet --
set(oa_exposed_lsp "${oa_algebra_builddir}/exposed.lsp")
set(oa_exposed_fasl "${oa_algebra_outdir}/exposed.${OA_LISP_FASL_EXT}")

add_custom_command(
  OUTPUT "${oa_exposed_lsp}"
  COMMAND $<TARGET_FILE:hammer>
    --tangle
    --output=${oa_exposed_lsp}
    "${PROJECT_SOURCE_DIR}/src/algebra/exposed.lsp.pamphlet"
  DEPENDS hammer "${PROJECT_SOURCE_DIR}/src/algebra/exposed.lsp.pamphlet"
  COMMENT "[axiomsys] Tangle exposed.lsp from pamphlet"
  VERBATIM
)

# -- 7b. Compile exposed.lsp -> exposed.fasl in the algebra staging dir --
# exposed.lsp depends on the boot-pkg module (defines the BOOT package).
add_custom_command(
  OUTPUT "${oa_exposed_fasl}"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${oa_algebra_outdir}"
  COMMAND ${OA_DRIVER}
    --execpath=${OA_BOOTSYS}
    --syslib=${OA_STAGE_ROOT}/lib
    --compile
    --output=${oa_exposed_fasl}
    --load-directory=${oa_interp_builddir}
    "${oa_exposed_lsp}"
  WORKING_DIRECTORY "${oa_algebra_builddir}"
  DEPENDS "${oa_exposed_lsp}" ${OA_BOOTSYS}
    "${oa_interp_builddir}/boot-pkg.${OA_LISP_FASL_EXT}"
  COMMENT "[axiomsys] Compile exposed.lsp -> exposed.${OA_LISP_FASL_EXT}"
  VERBATIM
)

# -- 7c. Generate makeint.lisp (build timestamp and image-link triggers) --
set(oa_makeint_lisp "${oa_interp_builddir}/makeint.lisp")
set(oa_makeint_fasl "${oa_interp_builddir}/makeint.${OA_LISP_FASL_EXT}")
set(oa_timestamp_file "${OA_STAGE_ROOT}/timestamp")

# The content mirrors what Autotools generates in src/interp/Makefile.in.
# |$StandardLinking| is true for ECL (which links via C objects instead of
# Lisp images).  |%basicSystemIsComplete| is true once interpsys is built.
# When neither is true, (build-interpsys) re-links the image and
# (|clearClams|) flushes the memoization caches.
file(WRITE "${oa_makeint_lisp}"
  "(import-module \"util\")\n"
  "(in-package \"BOOT\")\n"
  "(defconstant timestamp \"${oa_timestamp_file}\")\n"
  "(setq *build-version* \"OpenAxiom ${PROJECT_VERSION}\")\n"
  "(yearweek)\n"
  "(unless (or |$StandardLinking| (|%basicSystemIsComplete|))"
  " (build-interpsys) (|clearClams|))\n"
)

# -- 7d. Compile makeint.lisp -> makeint.fasl --
add_custom_command(
  OUTPUT "${oa_makeint_fasl}"
  COMMAND ${OA_DRIVER}
    --execpath=${OA_BOOTSYS}
    --syslib=${OA_STAGE_ROOT}/lib
    --compile
    --output=${oa_makeint_fasl}
    --load-directory=${oa_interp_builddir}
    "${oa_makeint_lisp}"
  WORKING_DIRECTORY "${oa_interp_builddir}"
  DEPENDS "${oa_makeint_lisp}" ${OA_BOOTSYS}
  COMMENT "[axiomsys] Compile makeint.lisp -> makeint.${OA_LISP_FASL_EXT}"
  VERBATIM
)

# -- 7e. Link AXIOMsys --
set(oa_axiomsys "${OA_STAGE_ROOT}/bin/AXIOMsys${CMAKE_EXECUTABLE_SUFFIX}")
set(oa_axiomsys_cmdfile "${oa_interp_builddir}/link-axiomsys.cmdfile")

string(JOIN "\n" _axiomsys_cmd
  "${OA_DRIVER}"
  "--execpath=${OA_BOOTSYS}"
  "--syslib=${OA_STAGE_ROOT}/lib"
  "--system=${OA_STAGE_ROOT}/"
  "--sysalg=${oa_algebra_outdir}/"
  "--prologue=(pushnew :open-axiom-algebra-system *features*)"
  "--make"
  "--output=${oa_axiomsys}"
  "--main=BOOT::|systemMain|"
  "--load-directory=${oa_interp_builddir}"
  ${oa_interp_objs}
  "${oa_initdb_fasl}"
  "${oa_makeint_fasl}"
)
file(GENERATE OUTPUT "${oa_axiomsys_cmdfile}" CONTENT "${_axiomsys_cmd}\n")

add_custom_command(
  OUTPUT "${oa_axiomsys}"
  COMMAND ${CMAKE_COMMAND} -E touch "${oa_timestamp_file}"
  COMMAND ${CMAKE_COMMAND}
    -DCMDFILE=${oa_axiomsys_cmdfile}
    -DWORKDIR=${oa_interp_builddir}
    -P ${PROJECT_SOURCE_DIR}/cmake/RunCommand.cmake
  WORKING_DIRECTORY "${oa_interp_builddir}"
  DEPENDS
    ${oa_interp_objs}
    ${OA_BOOTSYS}
    "${oa_msgs_staged}"
    ${oa_interp_ffi_staged}
    ${oa_staged_shared_lib}
    "${oa_initdb_fasl}"
    "${oa_makeint_fasl}"
    "${oa_exposed_fasl}"
    ${oa_database_targets}
  COMMENT "[axiomsys] Link AXIOMsys (complete interpreter with all algebra)"
  VERBATIM
)

# Autotools equivalent: all-axiomsys
# Artifacts: AXIOMsys -- the complete, installable OpenAxiom interpreter
#   image with all algebra constructors loaded.  This is the final system
#   image that end-users run.  Distinguished from interpsys by the
#   :open-axiom-algebra-system feature flag and --sysalg pointing to the
#   compiled algebra.
# Depends on: all-databases (databases must exist before linking),
#   all-algebra-layers (algebra .fasl files), interpsys modules (same OBJS),
#   exposed.fasl (constructor visibility), makeint.fasl (build timestamp).
add_custom_target(all-axiomsys
  DEPENDS "${oa_axiomsys}"
  COMMENT "Link AXIOMsys -- the complete OpenAxiom image with all algebra"
)

# -- Stage static files needed at runtime --
# summary and copyright go to the layout root; topics.data alongside
# the databases; gloss.text in the doc directory (needed by
# --build-databases for glossary cross-references).

# Autotools equivalent: part of all-share
# Artifacts: summary, copyright, topics.data, gloss.text staged into the
#   runtime tree.  These are static text files read by the interpreter at
#   startup and by --build-databases for glossary cross-references.
add_custom_target(all-static-files
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/etc/summary"
    "${OA_STAGE_ROOT}/"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/etc/copyright"
    "${OA_STAGE_ROOT}/"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/doc/topics.data"
    "${OA_STAGE_ROOT}/algebra/"
  COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/doc"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/doc/gloss.text"
    "${OA_STAGE_ROOT}/doc/"
  COMMENT "[static-files] Stage static runtime files"
)

# -- HyperDoc data staging (post-algebra, needs htadd which requires X11) --
# Pages, bitmaps, and viewports are copied to the staging tree.  After
# copying, htadd builds ht.db (the HyperDoc page index) by scanning
# all .ht and .pht files.

if(OA_HAVE_X11)
  set(oa_hyper_srcdir "${PROJECT_SOURCE_DIR}/src/hyper")

  # Autotools equivalent: all-hyper-pre
  # Artifacts: util.ht staged into the HyperDoc pages directory.  In Autotools
  #   this also built the htadd binary; in CMake htadd is compiled as a native
  #   target independently.  util.ht is a utility HyperDoc page needed by the
  #   algebra compilation process (referenced by some .spad constructors).
  set(oa_hyper_util_ht "${OA_STAGE_ROOT}/share/hypertex/pages/util.ht")
  add_custom_command(
    OUTPUT "${oa_hyper_util_ht}"
    COMMAND ${CMAKE_COMMAND} -E make_directory
      "${OA_STAGE_ROOT}/share/hypertex/pages"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${oa_hyper_srcdir}/pages/util.ht"
      "${oa_hyper_util_ht}"
    DEPENDS "${oa_hyper_srcdir}/pages/util.ht"
    COMMENT "[hypertex] Stage util.ht (pre-algebra HyperDoc page)"
    VERBATIM
  )
  add_custom_target(all-hyper-pre
    DEPENDS htadd "${oa_hyper_util_ht}"
    COMMENT "Build htadd and stage util.ht for pre-algebra HyperDoc support"
  )

  set(oa_hyper_staged_db "${OA_STAGE_ROOT}/share/hypertex/pages/ht.db")

  add_custom_command(
    OUTPUT "${oa_hyper_staged_db}"
    COMMAND ${CMAKE_COMMAND} -E make_directory
      "${OA_STAGE_ROOT}/share/hypertex/pages"
      "${OA_STAGE_ROOT}/share/hypertex/bitmaps"
    COMMAND ${CMAKE_COMMAND} -E copy_directory
      "${oa_hyper_srcdir}/pages"
      "${OA_STAGE_ROOT}/share/hypertex/pages"
    COMMAND ${CMAKE_COMMAND} -E copy_directory
      "${oa_hyper_srcdir}/bitmaps"
      "${OA_STAGE_ROOT}/share/hypertex/bitmaps"
    COMMAND $<TARGET_FILE:htadd> -s
      "${OA_STAGE_ROOT}/share/hypertex/pages/"
    WORKING_DIRECTORY "${OA_STAGE_ROOT}/share/hypertex/pages"
    DEPENDS htadd all-algebra-layers all-doc
    COMMENT "[hypertex] Stage HyperDoc pages/bitmaps and build ht.db"
    VERBATIM
  )

  # Autotools equivalent: all-hyper-post
  # Artifacts: ht.db (HyperDoc page index), staged .ht/.pht pages and
  #   bitmap images.  htadd scans all .ht/.pht files to produce ht.db,
  #   which HyperDoc uses to look up pages by name.
  # Depends on: htadd (native tool, built in all-native), all-algebra-layers.
  add_custom_target(all-hyper-post
    DEPENDS "${oa_hyper_staged_db}"
    COMMENT "Stage HyperDoc pages/bitmaps and build the ht.db page index"
  )

  # Autotools equivalent: part of all-share
  # Artifacts: pre-rendered 2D/3D graph snapshots in share/viewports/.
  #   HyperDoc embeds these in documentation pages.
  add_custom_target(all-viewports
    COMMAND ${CMAKE_COMMAND} -E make_directory
    "${OA_STAGE_ROOT}/share/viewports"
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    "${oa_hyper_srcdir}/viewports"
    "${OA_STAGE_ROOT}/share/viewports"
  COMMENT "[viewports] Stage HyperDoc viewports"
)

  # -- PS files from Gdraws (graph PostScript resources) --
  # The PS files are extracted from psFiles.pamphlet and staged into the
  # graph resource directory.  The graph programs load them at runtime.
  set(oa_gdraws_srcdir "${PROJECT_SOURCE_DIR}/src/graph/Gdraws")
  set(oa_ps_names
    colorpoly colorwol draw drawIstr drawarc drawcolor drawline
    drawlines drawpoint drawrect drawstr drwfilled end fillarc
    fillpoly fillwol header setup
  )
  set(oa_ps_outputs)
  foreach(ps ${oa_ps_names})
    set(_out "${OA_STAGE_ROOT}/graph/${ps}.ps")
    add_custom_command(
      OUTPUT "${_out}"
      COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/graph"
      COMMAND $<TARGET_FILE:hammer>
        --tangle=${ps}.ps
        --output=${_out}
        "${oa_gdraws_srcdir}/psFiles.pamphlet"
      DEPENDS hammer "${oa_gdraws_srcdir}/psFiles.pamphlet"
      COMMENT "[graph] Extract ${ps}.ps from psFiles.pamphlet"
      VERBATIM
    )
    list(APPEND oa_ps_outputs "${_out}")
  endforeach()

  # Autotools equivalent: part of all-share / all-graph
  # Artifacts: .ps (PostScript) files extracted from psFiles.pamphlet.
  #   The 2D/3D graph programs (view2D, view3D) load these at runtime
  #   for rendering mathematical graphs to PostScript output.
  add_custom_target(all-graph-ps
    DEPENDS ${oa_ps_outputs}
    COMMENT "Extract graph PostScript resources from psFiles.pamphlet"
  )

  # -- Umbrella data target (Lisp-dependent parts) --
  set(oa_all_data_deps all-databases all-static-files
    all-hyper-post all-viewports all-graph-ps)
endif()  # OA_HAVE_X11 (hypertex data, viewports, graph-ps)

if(NOT OA_HAVE_X11)
  set(oa_all_data_deps all-databases all-static-files)
endif()

# Autotools equivalent: combination of all-share + all-databases
# Umbrella target for all data artifacts needed at runtime:
# databases, static files, and (if X11) HyperDoc pages, viewports,
# and PostScript graph resources.
add_custom_target(all-data
  DEPENDS ${oa_all_data_deps}
  COMMENT "Build all runtime data (databases, HyperDoc, graph resources)"
)

# ---------------------------------------------------------------------------
# Step 8: Stage .input example/test files
# ---------------------------------------------------------------------------
# Each .input file is an OpenAxiom script (example, demo, or test case).
# Most live in .input.pamphlet files and must be tangled via hammer.
# The tangled/copied files are staged into OA_STAGE_ROOT/input/.

set(oa_input_srcdir "${PROJECT_SOURCE_DIR}/src/input")
set(oa_input_outdir "${OA_STAGE_ROOT}/input")
set(oa_all_input_files)

# Glob for pamphlets -- each produces one .input file.
file(GLOB oa_input_pamphlets "${oa_input_srcdir}/*.input.pamphlet")
foreach(pamphlet ${oa_input_pamphlets})
  get_filename_component(_base "${pamphlet}" NAME)            # e.g. "bug123.input.pamphlet"
  string(REPLACE ".pamphlet" "" _input_name "${_base}")       # e.g. "bug123.input"
  set(_out "${oa_input_outdir}/${_input_name}")

  add_custom_command(
    OUTPUT "${_out}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${oa_input_outdir}"
    COMMAND $<TARGET_FILE:hammer>
      --tangle
      --output=${_out}
      "${pamphlet}"
    DEPENDS hammer "${pamphlet}"
    COMMENT "[input] Tangle ${_input_name} from pamphlet"
    VERBATIM
  )
  list(APPEND oa_all_input_files "${_out}")
endforeach()

# Also copy any plain .input files that are not pamphlets.
file(GLOB oa_input_plain "${oa_input_srcdir}/*.input")
foreach(plain_input ${oa_input_plain})
  get_filename_component(_name "${plain_input}" NAME)
  set(_out "${oa_input_outdir}/${_name}")

  add_custom_command(
    OUTPUT "${_out}"
    COMMAND ${CMAKE_COMMAND} -E make_directory "${oa_input_outdir}"
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
      "${plain_input}" "${_out}"
    DEPENDS "${plain_input}"
    COMMENT "[input] Copy ${_name}"
    VERBATIM
  )
  list(APPEND oa_all_input_files "${_out}")
endforeach()

# Autotools equivalent: all-input
# Artifacts: ~526 .input files tangled from pamphlets (plus any plain .input
#   files) into OA_STAGE_ROOT/input/.  These are example scripts, demos, and
#   regression tests.  Does NOT require a Lisp runtime (only hammer).
# Depends on: hammer (for tangling pamphlets from noweb source).
add_custom_target(all-input
  DEPENDS ${oa_all_input_files}
  COMMENT "Tangle and stage all .input example/test scripts (~526 files)"
)

# ---------------------------------------------------------------------------
# Step 9: Regression test suite (all-check)
# ---------------------------------------------------------------------------
# Runs each .input file through AXIOMsys --test, producing .output files.
# Maps to CMake's ctest infrastructure via add_test().
#
# Autotools equivalent: all-check
# Depends on: all-axiomsys (the complete image), all-input (test scripts).

set(oa_test_outdir "${PROJECT_BINARY_DIR}/test-output")

# Tests that require graphical display, NAG libraries, or are known-broken
# are skipped by default.  They can be enabled via -DOPENAXIOM_TEST_SKIP
# override or by setting environment variables.
set(OPENAXIOM_TEST_SKIP_PATTERNS
  # Graphical tests (need X11 display)
  "draw" "graphics" "grpbug"
  # NAG library tests
  "nag"
  CACHE STRING "Patterns of .input files to skip in all-check"
)

foreach(input_file ${oa_all_input_files})
  get_filename_component(_name "${input_file}" NAME_WE)   # e.g. "bug123"

  # Check skip patterns
  set(_skip FALSE)
  foreach(pat ${OPENAXIOM_TEST_SKIP_PATTERNS})
    if(_name MATCHES "${pat}")
      set(_skip TRUE)
      break()
    endif()
  endforeach()
  if(_skip)
    continue()
  endif()

  add_test(
    NAME "input/${_name}"
    COMMAND ${OA_DRIVER}
      --execpath=${oa_axiomsys}
      --system=${OA_STAGE_ROOT}
      --sysalg=${oa_algebra_outdir}/
      --test
      "${input_file}"
    WORKING_DIRECTORY "${oa_test_outdir}"
  )
  set_tests_properties("input/${_name}" PROPERTIES
    TIMEOUT 300
    LABELS "input"
  )
endforeach()

# Convenience target: run all tests via ctest.
# cmake --build build --target all-check
# This is equivalent to: cd build && ctest -L input
add_custom_target(all-check
  COMMAND ${CMAKE_COMMAND} -E make_directory "${oa_test_outdir}"
  COMMAND ${CMAKE_CTEST_COMMAND}
    --test-dir "${PROJECT_BINARY_DIR}"
    --label-regex "input"
    --output-on-failure
  DEPENDS all-axiomsys all-input
  COMMENT "Run the OpenAxiom regression test suite (~500 .input scripts)"
)

else()
  # Placeholders so that `cmake --build . --target <name>` always exists,
  # even when no supported Lisp runtime was found.
  set(oa_lisp_targets
    all-lisp
    all-boot all-interpsys all-boot-interp
    all-extract all-initdb all-algstrap
    all-algebra-layers all-algebra
    all-databases all-axiomsys
    all-data all-input all-check
  )
  foreach(tgt ${oa_lisp_targets})
    add_custom_target(${tgt}
      COMMAND ${CMAKE_COMMAND} -E echo
        "${tgt} unavailable (no supported Lisp runtime configured)"
    )
  endforeach()
endif()  # OA_LISP_EXECUTABLE guard

# ===========================================================================
# Optional native executables
# ===========================================================================
# These targets are pure C/C++ and do not require a Lisp runtime.  They
# are guarded by feature flags (OA_HAVE_X11, OPENAXIOM_USE_SMAN,
# HAVE_REGEX_H, OPENAXIOM_USE_GUI) set during configuration.
#
# All .c files are compiled as C++ via the blanket LANGUAGE CXX policy
# at the top of this file.
# ===========================================================================

# -- asq: pamphlet extraction + compilation --------------------------------
# asq.c is tangled from asq.c.pamphlet by hammer (pure C++ tool, no
# Lisp dependency), then compiled as a normal executable.  It queries
# the algebra databases.
set(oa_asq_c "${PROJECT_BINARY_DIR}/phase6/asq.c")
file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/phase6")

add_custom_command(
  OUTPUT "${oa_asq_c}"
  COMMAND $<TARGET_FILE:hammer>
    --tangle
    --output=${oa_asq_c}
    "${PROJECT_SOURCE_DIR}/src/etc/asq.c.pamphlet"
  DEPENDS hammer "${PROJECT_SOURCE_DIR}/src/etc/asq.c.pamphlet"
  COMMENT "[asq] Extract asq.c from pamphlet"
  VERBATIM
)

add_executable(asq "${oa_asq_c}")
# asq.c is generated in the build tree, outside the src/ glob above.
set_source_files_properties("${oa_asq_c}" PROPERTIES LANGUAGE CXX)
target_link_libraries(asq PRIVATE spad open-axiom-core)

# Autotools equivalent: all-asq
# Artifacts: the asq executable (built above) plus databases, summary, and
#   copyright.  In Autotools, `all-asq` was the umbrella in src/etc/ that
#   gated everything from that directory.  Here it depends on asq (the
#   executable), all-databases, and all-static-files (summary/copyright).
# Note: asq itself is a pure C++ target with no Lisp dependency, but
#   the databases it queries require the full algebra build.
set(oa_all_asq_deps asq)
if(OA_LISP_EXECUTABLE)
  list(APPEND oa_all_asq_deps all-databases all-static-files)
endif()
add_custom_target(all-asq
  DEPENDS ${oa_all_asq_deps}
  COMMENT "Build asq and its runtime data (databases, summary, copyright)"
)

# -- J. clef: command-line editor ------------------------------------------
# clef provides line-editing and cursor control for the terminal-based
# OpenAxiom session.  Single source file, always built.

set(oa_clef_sources clef/edible.c)

add_executable(clef ${oa_clef_sources})
target_link_libraries(clef PRIVATE spad open-axiom-core)
oa_apply_build_flags(clef)


# -- K. Session manager (sman) --------------------------------------------
# sman orchestrates the OpenAxiom session: it launches the interpreter,
# HyperDoc, and the graphics viewport manager as child processes.
# Requires fork() and wait(), so it is gated on OPENAXIOM_USE_SMAN.
#
# Three executables:
#   sman        -- session manager (user-facing, installed in bin/)
#   session     -- session process (internal, installed in lib/)
#   spadclient  -- socket client for IDE integration (internal)

if(OPENAXIOM_USE_SMAN)
  set(oa_sman_sources    sman/sman.c)
  set(oa_session_sources sman/session.c)
  set(oa_spadclient_sources sman/spadclient.c)

  add_executable(sman ${oa_sman_sources})
  add_executable(session ${oa_session_sources})
  add_executable(spadclient ${oa_spadclient_sources})

  foreach(tgt sman session spadclient)
    oa_apply_build_flags(${tgt})
  endforeach()

  target_link_libraries(sman PRIVATE OpenAxiom spad open-axiom-core)
  target_link_libraries(session PRIVATE spad open-axiom-core)
  target_link_libraries(spadclient PRIVATE OpenAxiom spad open-axiom-core)
endif()


# -- L. Graphics subsystem -------------------------------------------------
# The graphics subsystem provides 2D and 3D viewport rendering via X11.
# Gated on OA_HAVE_X11 (requires libX11, libXt, libXpm).
#
# Components:
#   Gfun       -- shared drawing-primitives object (not a standalone exe)
#   viewman    -- viewport manager (dispatches 2D/3D requests)
#   view2D     -- 2D graph renderer
#   view3D     -- 3D graph renderer
#   viewAlone  -- standalone viewport viewer (user-facing)
#
# All graph .c sources are compiled as C++ (same rationale as spad lib).

if(OA_HAVE_X11)
  # X11 link libraries -- provided by find_package(X11) in the root.
  set(oa_x11_libs X11::X11 X11::Xt X11::Xpm)

  # -- Helper: oa_add_graph_executable ------------------------------------
  # Factors the repeated pattern for graph subsystem executables.  Each
  # needs the graph/include directory, its own subdirectory, X11 link
  # libraries, and the standard build flags.  libm propagates transitively
  # from open-axiom-core, so it is not listed here.
  #
  # Usage: oa_add_graph_executable(<name> <subdir> <sources>...
  #            [OBJECTS <obj-lib>] [EXTRA_LIBS <libs>...])
  function(oa_add_graph_executable name subdir)
    cmake_parse_arguments(OA "" "" "OBJECTS;EXTRA_LIBS" ${ARGN})
    set(_sources ${OA_UNPARSED_ARGUMENTS})
    if(OA_OBJECTS)
      add_executable(${name} ${_sources} $<TARGET_OBJECTS:${OA_OBJECTS}>)
    else()
      add_executable(${name} ${_sources})
    endif()
    target_include_directories(${name} PRIVATE
      "${PROJECT_SOURCE_DIR}/src/graph/include"
      "${PROJECT_SOURCE_DIR}/src/graph/${subdir}"
    )
    target_link_libraries(${name} PRIVATE spad ${oa_x11_libs} ${OA_EXTRA_LIBS})
    oa_apply_build_flags(${name})
  endfunction()

  # -- Gfun: shared drawing object (compiled into view2D and view3D) --
  add_library(Gfun OBJECT graph/Gdraws/Gfun.c)
  target_include_directories(Gfun PRIVATE
    "${PROJECT_SOURCE_DIR}/src/graph/include"
    "${PROJECT_SOURCE_DIR}/src/graph/Gdraws"
  )
  target_link_libraries(Gfun PRIVATE open-axiom-core)
  oa_apply_build_flags(Gfun)

  # -- viewman --
  oa_add_graph_executable(viewman viewman
    graph/viewman/cleanup.c
    graph/viewman/fun2D.c
    graph/viewman/fun3D.c
    graph/viewman/make2D.c
    graph/viewman/make3D.c
    graph/viewman/makeGraph.c
    graph/viewman/readView.c
    graph/viewman/sselect.c
    graph/viewman/viewman.c
  )

  # -- view2D --
  oa_add_graph_executable(view2D view2D
    graph/view2D/buttons2d.c
    graph/view2D/control2d.c
    graph/view2D/graph2d.c
    graph/view2D/main2d.c
    graph/view2D/pot2d.c
    graph/view2D/process2d.c
    graph/view2D/spadAction2d.c
    graph/view2D/stuff2d.c
    graph/view2D/viewport2D.c
    graph/view2D/write2d.c
    OBJECTS Gfun
    EXTRA_LIBS open-axiom-core
  )

  # -- view3D --
  oa_add_graph_executable(view3D view3D
    graph/view3D/buttons3d.c
    graph/view3D/closeView3d.c
    graph/view3D/component3d.c
    graph/view3D/control3d.c
    graph/view3D/illuminate3d.c
    graph/view3D/lightbut3d.c
    graph/view3D/lighting3d.c
    graph/view3D/main3d.c
    graph/view3D/mesh3d.c
    graph/view3D/msort3d.c
    graph/view3D/pot3d.c
    graph/view3D/process3d.c
    graph/view3D/project3d.c
    graph/view3D/quit3d.c
    graph/view3D/quitbut3d.c
    graph/view3D/save3d.c
    graph/view3D/savebut3d.c
    graph/view3D/smoothShade3d.c
    graph/view3D/spadAction3d.c
    graph/view3D/stuff3d.c
    graph/view3D/surface3d.c
    graph/view3D/transform3d.c
    graph/view3D/viewport3d.c
    graph/view3D/volume3d.c
    graph/view3D/write3d.c
    OBJECTS Gfun
    EXTRA_LIBS open-axiom-core
  )

  # -- viewAlone --
  oa_add_graph_executable(viewAlone viewAlone
    graph/viewAlone/viewAlone.c
    graph/viewAlone/spoonComp.c
    graph/viewAlone/spoon2D.c
  )
endif()  # OA_HAVE_X11 (graphics subsystem)


# -- M. HyperDoc executables -----------------------------------------------
# HyperDoc is the hypertext help system for OpenAxiom.  All HyperDoc
# tools depend on X11 at compile time (via pixmap.h in hyper.h).
# hthits and htsearch additionally require regex.h.
#
# htadd is needed by the data staging (ht.db generation).
#
# libm propagates transitively from open-axiom-core (PUBLIC), so it is
# not repeated on individual targets.

if(OA_HAVE_X11)

  # -- Helper: oa_add_hyper_executable ------------------------------------
  # Factors the repeated pattern for HyperDoc executables: each needs
  # the src/hyper include directory, spad + open-axiom-core linkage, and
  # the standard build flags.
  #
  # Usage: oa_add_hyper_executable(<name> <sources>... [EXTRA_LIBS <libs>...])
  function(oa_add_hyper_executable name)
    cmake_parse_arguments(OA "" "" "EXTRA_LIBS" ${ARGN})
    add_executable(${name} ${OA_UNPARSED_ARGUMENTS})
    target_include_directories(${name} PRIVATE
      "${PROJECT_SOURCE_DIR}/src/hyper"
    )
    target_link_libraries(${name} PRIVATE spad open-axiom-core ${OA_EXTRA_LIBS})
    oa_apply_build_flags(${name})
  endfunction()

  # -- htadd: adds pages to the HyperDoc database --
  oa_add_hyper_executable(htadd
    hyper/addfile.c hyper/htadd.c hyper/lex.c)

  # -- ex2ht: converts examples to HyperDoc pages --
  oa_add_hyper_executable(ex2ht hyper/ex2ht.c)

  # -- hthits / htsearch: require regex.h --
  if(HAVE_REGEX_H)
    oa_add_hyper_executable(hthits hyper/hthits.c)
    # htsearch is C++ (htsearch.cc), no LANGUAGE override needed.
    oa_add_hyper_executable(htsearch hyper/htsearch.cc)
  endif()  # HAVE_REGEX_H

  # -- hypertex --
  oa_add_hyper_executable(hypertex
    hyper/addfile.c
    hyper/cond.c
    hyper/dialog.c
    hyper/display.c
    hyper/event.c
    hyper/extent1.c
    hyper/extent2.c
    hyper/form-ext.c
    hyper/group.c
    hyper/htinp.c
    hyper/hyper.c
    hyper/initx.c
    hyper/input.c
    hyper/item.c
    hyper/keyin.c
    hyper/lex.c
    hyper/macro.c
    hyper/mem.c
    hyper/parse.c
    hyper/parse-aux.c
    hyper/parse-input.c
    hyper/parse-paste.c
    hyper/parse-types.c
    hyper/ReadBitmap.c
    hyper/scrollbar.c
    hyper/show-types.c
    hyper/spadint.c
    hyper/titlebar.c
    EXTRA_LIBS ${oa_x11_libs}
  )

  # -- spadbuf --
  oa_add_hyper_executable(spadbuf hyper/spadbuf.c
    EXTRA_LIBS ${oa_x11_libs}
  )

endif()  # OA_HAVE_X11 (htadd, ex2ht, hthits, htsearch, hypertex, spadbuf)


# -- N. Qt GUI frontend ----------------------------------------------------
# When Qt Widgets is available, build a graphical front-end that replaces
# the terminal-based driver.  The GUI provides a conversation-style
# interface with input editing, output rendering, and embedded graphics.

if(OPENAXIOM_USE_GUI)
  set(CMAKE_AUTOMOC ON)

  add_executable(open-axiom-gui
    gui/main.cc
    gui/server.cc
    gui/conversation.cc
    gui/main-window.cc
    gui/debate.cc
  )
  target_include_directories(open-axiom-gui PRIVATE
    "${PROJECT_SOURCE_DIR}/src/gui"
  )
  target_link_libraries(open-axiom-gui PRIVATE
    OpenAxiom open-axiom-core ${OA_QT_TARGET}
  )
  # cxx_std_20 propagates transitively from OpenAxiom (PUBLIC).
  oa_apply_build_flags(open-axiom-gui)

  set(CMAKE_AUTOMOC OFF)
endif()


# -- O. Documentation and shared resource staging --------------------------
# These copy rules stage help files, the glossary, the command list,
# the message catalog, and the TeX style file into the runtime layout.
# They are unconditional (no Lisp or X11 dependency).

# Autotools equivalent: all-doc + all-share
# Artifacts: help files, glossary (gloss.text), message catalogs (co-eng.msgs),
#   command.list, and TeX style file (open-axiom.sty).  All staged into
#   OA_STAGE_ROOT.  These are unconditional (no Lisp or X11 dependency).
add_custom_target(all-doc
  # Help files
  COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/doc/help"
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    "${PROJECT_SOURCE_DIR}/src/doc/help"
    "${OA_STAGE_ROOT}/doc/help"
  # Glossary
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/doc/gloss.text"
    "${OA_STAGE_ROOT}/doc/"
  # Message catalogs (co-eng.msgs alongside s2-us.msgs)
  COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/share/msgs"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/doc/msgs/co-eng.msgs"
    "${OA_STAGE_ROOT}/share/msgs/"
  # command.list
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/share/algebra/command.list"
    "${OA_STAGE_ROOT}/"
  # TeX style file
  COMMAND ${CMAKE_COMMAND} -E make_directory "${OA_STAGE_ROOT}/tex"
  COMMAND ${CMAKE_COMMAND} -E copy_if_different
    "${PROJECT_SOURCE_DIR}/src/share/tex/open-axiom.sty"
    "${OA_STAGE_ROOT}/tex/"
  COMMENT "[doc] Stage documentation and shared resources"
)


# -- P. Umbrella targets ----------------------------------------------------

# all-native: every native executable (core + optional).
set(oa_all_native_targets open-axiom hammer oa-lisp clef asq)
if(OPENAXIOM_USE_SMAN)
  list(APPEND oa_all_native_targets sman session spadclient)
endif()
if(OA_HAVE_X11)
  list(APPEND oa_all_native_targets htadd ex2ht viewman view2D view3D viewAlone hypertex spadbuf)
endif()
if(OA_HAVE_X11 AND HAVE_REGEX_H)
  list(APPEND oa_all_native_targets hthits htsearch)
endif()
if(OPENAXIOM_USE_GUI)
  list(APPEND oa_all_native_targets open-axiom-gui)
endif()

# Autotools equivalent: combination of all-lib + all-clef + all-sman + all-graph + all-hyper + all-gui
# Umbrella target for every native (C/C++) executable in the project.
# Conditionally includes: sman/session/spadclient (if AF_UNIX sockets),
#   htadd/ex2ht/viewman/view2D/view3D/viewAlone/hypertex/spadbuf (if X11),
#   hthits/htsearch (if X11 + regex.h), open-axiom-gui (if Qt).
add_custom_target(all-native
  DEPENDS ${oa_all_native_targets}
  COMMENT "Build all native C/C++ executables (core + optional subsystems)"
)

# all-optional: everything optional (native + docs + data if Lisp available).
set(oa_all_optional_deps all-native all-doc)
# Umbrella target: all native executables + documentation.
# This is the default target for CI builds that don't need the Lisp chain.
add_custom_target(all-optional
  DEPENDS ${oa_all_optional_deps}
  COMMENT "Build all native executables and documentation (no Lisp chain)"
)

# -- Component-level convenience targets ------------------------------------
# These match the Autotools all-<component> granularity so developers can
# build individual subsystems by name.

# Autotools equivalent: all-clef
# Artifacts: clef -- the command-line editor providing readline-like input
#   editing for the OpenAxiom terminal interface.
add_custom_target(all-clef
  DEPENDS clef
  COMMENT "Build the command-line editor (clef)"
)

# Autotools equivalent: all-sman
# Artifacts: sman (session manager), session (session process),
#   spadclient (socket client for IDE integration).
#   Conditional on OPENAXIOM_USE_SMAN (requires AF_UNIX socket support).
if(OPENAXIOM_USE_SMAN)
  add_custom_target(all-sman
    DEPENDS sman session spadclient
    COMMENT "Build the session manager suite (sman, session, spadclient)"
  )
endif()

# Autotools equivalent: all-graph
# Artifacts: viewman (viewport manager), view2D (2D renderer), view3D (3D
#   renderer), viewAlone (standalone viewer).  All require X11.
if(OA_HAVE_X11)
  add_custom_target(all-graph
    DEPENDS viewman view2D view3D viewAlone
    COMMENT "Build the X11 graphics subsystem (viewman, view2D, view3D, viewAlone)"
  )

  # Autotools equivalent: all-hyper
  # Artifacts: htadd (page DB builder), ex2ht (example converter),
  #   hypertex (hypertext viewer), spadbuf (buffer tool).
  #   hthits/htsearch require regex.h in addition to X11.
  set(oa_hyper_targets htadd ex2ht hypertex spadbuf)
  if(HAVE_REGEX_H)
    list(APPEND oa_hyper_targets hthits htsearch)
  endif()
  add_custom_target(all-hyper
    DEPENDS ${oa_hyper_targets}
    COMMENT "Build all HyperDoc tools (htadd, ex2ht, hypertex, spadbuf, ...)"
  )
endif()

# Autotools equivalent: all-gui
# Artifacts: open-axiom-gui (Qt-based graphical frontend).
if(OPENAXIOM_USE_GUI)
  add_custom_target(all-gui
    DEPENDS open-axiom-gui
    COMMENT "Build the Qt graphical frontend (open-axiom-gui)"
  )
endif()

# all-src: aggregate of all source-derived targets.
# Autotools equivalent: all-src (top-level source build).
# Depends on all-native + all-doc + all-input unconditionally, plus all
# Lisp-derived targets (interpreter, algebra, AXIOMsys, data) when a Lisp
# runtime is available.
set(oa_all_src_deps all-native all-doc all-input)
if(OA_LISP_EXECUTABLE)
  list(APPEND oa_all_src_deps all-interpsys all-algebra all-axiomsys all-data)
endif()
add_custom_target(all-src
  DEPENDS ${oa_all_src_deps}
  COMMENT "Build everything under src/ (native + docs + inputs + Lisp chain + data)"
)


# headers) into OA_STAGE_ROOT -- an in-tree directory that mirrors the
# final installation layout.  Later phases read from this staging tree.
#
# Executables and libraries are staged via lists so adding a new target
# only requires appending to the list -- no new COMMAND block needed.
# Note: only artifacts needed by later build phases are staged here.
# Executables like open-axiom (the driver) and hammer are used directly
# from the build tree, matching Autotools behaviour.

set(oa_stage_dirs
  "${OA_STAGE_ROOT}/bin"
  "${OA_STAGE_ROOT}/lib"
  "${OA_STAGE_ROOT}/include/open-axiom"
  "${OA_STAGE_ROOT}/share"
  "${OA_STAGE_ROOT}/doc/help"
  "${OA_STAGE_ROOT}/src/algebra"
)

# Libraries are built directly into ${OA_STAGE_ROOT}/lib/ via output-
# directory properties, so no copy step is needed for them.
set(oa_stage_lib_targets  open-axiom-core OpenAxiom spad)
if(oa_use_dynamic_lib STREQUAL "yes")
  list(APPEND oa_stage_lib_targets open-axiom-core-shared)
endif()

add_custom_target(stage-native-layout
  # -- Create the directory skeleton --
  COMMAND ${CMAKE_COMMAND} -E make_directory ${oa_stage_dirs}
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
  DEPENDS open-axiom hammer oa-lisp ${oa_stage_lib_targets}
  COMMENT "Stage native artifacts into an OpenAxiom-style runtime layout"
)

# Quick compilation check: just build the three core executables.
# Does NOT stage anything into OA_STAGE_ROOT.
add_custom_target(all-lib
  DEPENDS open-axiom hammer oa-lisp
  COMMENT "Build core executables only (open-axiom, hammer, oa-lisp) -- no staging"
)

# Autotools equivalent: stamp-subdirs + all-lib (staging portion)
# Build all core executables AND populate OA_STAGE_ROOT with a directory
# skeleton, headers, and native artifacts.  This is the prerequisite for
# the Lisp toolchain because oa-lisp and the staged libraries must exist
# before SBCL can compile Lisp sources.
add_custom_target(all-staging
  DEPENDS stage-native-layout
  COMMENT "Build core executables and stage into the runtime layout tree"
)

# -- I. Install rules ------------------------------------------------------
# open-axiom goes into the system PATH ($PREFIX/bin) so that users can
# invoke it directly.  All other binaries, libraries, and headers live
# under the versioned layout root so multiple versions can coexist.
#
# System PATH (bindir):           open-axiom (the driver)
# User-facing tools (bin/):       clef, asq, sman, viewAlone
# Internal executables (lib/):    session, spadclient, viewman, view2D, view3D,
#                                 hypertex, spadbuf, htadd, ex2ht, hthits,
#                                 htsearch
# Build-only (not installed):     hammer, lisp

install(TARGETS open-axiom
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# hammer and lisp are build-only tools (noinst_PROGRAMS in Autotools)
# and are not installed.

# Libraries + their FILE_SET headers are installed together.
# FILE_SET public_headers -> include/ (open-axiom.h + open-axiom/*)
# FILE_SET generated_headers -> include/openaxiom-c-macros.h
install(TARGETS open-axiom-core OpenAxiom spad
  EXPORT OpenAxiomTargets
  ARCHIVE DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/lib
  LIBRARY DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/lib
  FILE_SET public_headers DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/include
  FILE_SET generated_headers DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/include
)

install(FILES
  "${PROJECT_BINARY_DIR}/config/openaxiom-lisp-settings.cmake"
  DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/share/open-axiom/cmake
)

# -- CMake package export --
# Produces OpenAxiomTargets.cmake, OpenAxiomConfig.cmake, and
# OpenAxiomConfigVersion.cmake so that downstream projects can use
# find_package(OpenAxiom) to locate the libraries and headers.

set(OA_CMAKE_INSTALL_DIR "${OA_INSTALL_LAYOUT_ROOT}/lib/cmake/OpenAxiom")

install(EXPORT OpenAxiomTargets
  FILE OpenAxiomTargets.cmake
  NAMESPACE OpenAxiom::
  DESTINATION ${OA_CMAKE_INSTALL_DIR}
)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  "${PROJECT_SOURCE_DIR}/cmake/OpenAxiomConfig.cmake.in"
  "${PROJECT_BINARY_DIR}/cmake/OpenAxiomConfig.cmake"
  INSTALL_DESTINATION ${OA_CMAKE_INSTALL_DIR}
)

write_basic_package_version_file(
  "${PROJECT_BINARY_DIR}/cmake/OpenAxiomConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion
)

install(FILES
  "${PROJECT_BINARY_DIR}/cmake/OpenAxiomConfig.cmake"
  "${PROJECT_BINARY_DIR}/cmake/OpenAxiomConfigVersion.cmake"
  DESTINATION ${OA_CMAKE_INSTALL_DIR}
)

# -- Install rules for optional executables --

# The driver is the only executable installed to the system PATH.
# Everything else goes under the OpenAxiom layout root, matching Autotools.

# User-facing tools -> $OA_INSTALL_LAYOUT_ROOT/bin/
install(TARGETS clef asq
  RUNTIME DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/bin
)

if(OPENAXIOM_USE_SMAN)
  install(TARGETS sman
    RUNTIME DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/bin
  )
  # session and spadclient are internal executables launched by sman.
  install(TARGETS session spadclient
    RUNTIME DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/lib
  )
endif()

if(OA_HAVE_X11)
  install(TARGETS viewAlone
    RUNTIME DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/bin
  )
  # Internal executables launched by sman/hypertex -> lib/
  install(TARGETS viewman view2D view3D hypertex spadbuf htadd ex2ht
    RUNTIME DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/lib
  )
  if(HAVE_REGEX_H)
    install(TARGETS hthits htsearch
      RUNTIME DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/lib
    )
  endif()
endif()

if(OPENAXIOM_USE_GUI)
  install(TARGETS open-axiom-gui
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  )
endif()

# Help files
install(DIRECTORY "${PROJECT_SOURCE_DIR}/src/doc/help/"
  DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/doc/help
  FILES_MATCHING PATTERN "*.help"
)

# Glossary and message catalogs
install(FILES "${PROJECT_SOURCE_DIR}/src/doc/gloss.text"
  DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/doc
)
install(FILES
  "${PROJECT_SOURCE_DIR}/src/doc/msgs/s2-us.msgs"
  "${PROJECT_SOURCE_DIR}/src/doc/msgs/co-eng.msgs"
  DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/share/msgs
)

# Shared resources
install(FILES "${PROJECT_SOURCE_DIR}/src/share/algebra/command.list"
  DESTINATION ${OA_INSTALL_LAYOUT_ROOT}
)
install(FILES "${PROJECT_SOURCE_DIR}/src/share/tex/open-axiom.sty"
  DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/tex
)
install(FILES
  "${PROJECT_SOURCE_DIR}/src/etc/summary"
  "${PROJECT_SOURCE_DIR}/src/etc/copyright"
  DESTINATION ${OA_INSTALL_LAYOUT_ROOT}
)

# HyperDoc pages, bitmaps, viewports
if(OA_HAVE_X11)
  install(DIRECTORY "${PROJECT_SOURCE_DIR}/src/hyper/pages/"
    DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/share/hypertex/pages
  )
  install(DIRECTORY "${PROJECT_SOURCE_DIR}/src/hyper/bitmaps/"
    DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/share/hypertex/bitmaps
  )
  install(DIRECTORY "${PROJECT_SOURCE_DIR}/src/hyper/viewports/"
    DESTINATION ${OA_INSTALL_LAYOUT_ROOT}/share/viewports
  )
endif()


# ===========================================================================
# Q. CPack packaging
# ===========================================================================
# Basic CPack configuration for generating distributable packages.
# Supports TGZ (all platforms), DEB (Debian/Ubuntu), and RPM (Fedora/RHEL).
# Run `cpack --config CPackConfig.cmake` after building to generate packages.

set(CPACK_PACKAGE_NAME "open-axiom")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_PACKAGE_VENDOR "OpenAxiom Project")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "OpenAxiom computer algebra system")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://github.com/GabrielDosReis/open-axiom")
set(CPACK_PACKAGE_CONTACT "open-axiom-bugs@lists.sf.net")
set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/src/etc/copyright")

# Generator selection: TGZ always, DEB if dpkg available, RPM if rpmbuild.
set(CPACK_GENERATOR "TGZ")
find_program(_dpkg dpkg)
if(_dpkg)
  list(APPEND CPACK_GENERATOR "DEB")
  set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6, libstdc++6")
  if(OA_HAVE_X11)
    string(APPEND CPACK_DEBIAN_PACKAGE_DEPENDS ", libx11-6, libxt6, libxpm4")
  endif()
  set(CPACK_DEBIAN_PACKAGE_SECTION "math")
endif()
find_program(_rpmbuild rpmbuild)
if(_rpmbuild)
  list(APPEND CPACK_GENERATOR "RPM")
  set(CPACK_RPM_PACKAGE_LICENSE "BSD")
  set(CPACK_RPM_PACKAGE_GROUP "Applications/Engineering")
endif()

include(CPack)
