# ===========================================================================
# CMakeLists.txt -- top-level build configuration for OpenAxiom
# ===========================================================================
# This file replaces the Autotools entry point (configure.ac / Makefile.in)
# with a modern CMake build.  It is organised into numbered sections that
# mirror the layered bootstrap pipeline of the original build:
#
#   1. Project meta-data and language standards
#   2. User-facing build options
#   3. Platform feature detection (headers, symbols, types)
#   4. Platform-triple and path layout
#   5. External toolchain discovery (Lisp, Qt, LLVM, TeX)
#   6. Derived feature flags
#   7. Configuration summary
#   8. Generated headers and templates
#   9. Source tree
#
# The actual library / executable / Lisp targets live in src/CMakeLists.txt.
# ===========================================================================

# -- 1. Project meta-data and language standards ---------------------------

cmake_minimum_required(VERSION 3.31)

project(OpenAxiom VERSION 1.5.0 LANGUAGES C CXX)

# Standard CMake modules we rely on for feature probing.
include(CheckIncludeFile)
include(CheckSymbolExists)
include(CheckCXXSourceCompiles)
include(GNUInstallDirs)

# Our own CMake modules live under cmake/.
list(APPEND CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake")

# Export compile_commands.json for clangd / IDE integration.
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
# C11 for the few legacy .c files; C++20 for the rest of the codebase.
# C++20 is needed for designated initialisers, concepts, and std::span
# used in the OpenAxiom runtime libraries.
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
# PIC is required because some static libraries are linked into shared
# objects or into the Lisp image (ECL's standard-linking model).
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# -- 2. User-facing build options -----------------------------------------

option(OA_ENABLE_GRAPHICS         "Enable X11 graphics detection and later graphics components" ON)
option(OA_ENABLE_GUI              "Enable Qt GUI detection and later GUI support" ON)
option(OA_ENABLE_SMAN             "Enable session-manager support when the platform allows it" ON)
option(OA_ENABLE_DOCS             "Enable documentation-tool detection" ON)
option(OA_ENABLE_LLVM             "Enable LLVM toolchain detection" ON)
option(OA_ENABLE_THREADS          "Enable threads support flags when supported" OFF)
option(OA_ENABLE_PROFILING        "Enable profiling-friendly compiler and linker flags" OFF)
option(OA_ENABLE_RUNTIME_CHECKING "Enable extra runtime-checking friendly build flags" OFF)
option(OA_KEEP_INTERMEDIATE_FILES "Keep intermediary generated files in later phases" OFF)

# Which Lisp runtime to use for the interpreter / algebra build chain.
set(OA_WITH_LISP "sbcl" CACHE STRING "Base Lisp runtime for OpenAxiom")
set_property(CACHE OA_WITH_LISP PROPERTY STRINGS sbcl gcl ecl clisp clozure bemol polyml)

# Package identity -- substituted into the generated config header and
# passed to the Autotools-era code that still reads these symbols.
# These variables mirror the AC_INIT / AC_DEFINE block from configure.ac;
# they will be removed once the C/C++ sources no longer reference them.
set(PACKAGE "OpenAxiom")
set(PACKAGE_NAME "OpenAxiom")
set(PACKAGE_BUGREPORT "open-axiom-bugs@lists.sf.net")
set(PACKAGE_TARNAME "open-axiom")
set(PACKAGE_URL "https://github.com/GabrielDosReis/open-axiom")
set(PACKAGE_VERSION "${PROJECT_VERSION}")
set(PACKAGE_STRING "${PACKAGE_NAME} ${PROJECT_VERSION}")
set(VERSION "${PROJECT_VERSION}")
set(STDC_HEADERS 1)
set(SIZEOF_VOIDP "${CMAKE_SIZEOF_VOID_P}")
# LT_OBJDIR is a libtool remnant; some headers still test for it.
set(LT_OBJDIR ".libs/")
set(HAVE_LONG_FILE_NAMES 1)

# -- 3. Platform feature detection ----------------------------------------
# These checks populate HAVE_* variables that are substituted into the
# generated config header (openaxiom-c-macros.h).  The list is kept in
# sync with the Autotools AC_CHECK_HEADERS / AC_CHECK_FUNCS probes.

# -- Header availability --
check_include_file("dirent.h" HAVE_DIRENT_H)
check_include_file("dlfcn.h" HAVE_DLFCN_H)
check_include_file("fcntl.h" HAVE_FCNTL_H)
check_include_file("inttypes.h" HAVE_INTTYPES_H)
check_include_file("libutil.h" HAVE_LIBUTIL_H)
check_include_file("pty.h" HAVE_PTY_H)
check_include_file("signal.h" HAVE_SIGNAL_H)
check_include_file("stdint.h" HAVE_STDINT_H)
check_include_file("stdio.h" HAVE_STDIO_H)
check_include_file("stdlib.h" HAVE_STDLIB_H)
check_include_file("strings.h" HAVE_STRINGS_H)
check_include_file("string.h" HAVE_STRING_H)
check_include_file("sys/ioctl.h" HAVE_SYS_IOCTL_H)
check_include_file("sys/mman.h" HAVE_SYS_MMAN_H)
check_include_file("sys/socket.h" HAVE_SYS_SOCKET_H)
check_include_file("sys/stat.h" HAVE_SYS_STAT_H)
check_include_file("sys/types.h" HAVE_SYS_TYPES_H)
check_include_file("sys/wait.h" HAVE_SYS_WAIT_H)
check_include_file("termios.h" HAVE_TERMIOS_H)
check_include_file("unistd.h" HAVE_UNISTD_H)
check_include_file("util.h" HAVE_UTIL_H)
check_include_file("winsock2.h" HAVE_WINSOCK2_H)
check_include_file("regex.h" HAVE_REGEX_H)
check_include_file("X11/Intrinsic.h" HAVE_X11_INTRINSIC_H)
check_include_file("X11/xpm.h" HAVE_X11_XPM_H)

# -- Symbol / function availability --
check_symbol_exists(AF_LOCAL "sys/socket.h" HAVE_AF_LOCAL)
check_symbol_exists(AF_UNIX "sys/socket.h" HAVE_AF_UNIX)
check_symbol_exists(fork "unistd.h" HAVE_DECL_FORK)
check_symbol_exists(getegid "unistd.h" HAVE_DECL_GETEGID)
check_symbol_exists(geteuid "unistd.h" HAVE_DECL_GETEUID)
check_symbol_exists(getgid "unistd.h" HAVE_DECL_GETGID)
check_symbol_exists(getuid "unistd.h" HAVE_DECL_GETUID)
check_symbol_exists(kill "signal.h;sys/types.h" HAVE_DECL_KILL)
check_symbol_exists(mkdtemp "stdlib.h" HAVE_DECL_MKDTEMP)
check_symbol_exists(mkstemp "stdlib.h" HAVE_DECL_MKSTEMP)
check_symbol_exists(mktemp "stdlib.h" HAVE_DECL_MKTEMP)
check_cxx_source_compiles([=[
#include <sys/types.h>
#include <pty.h>
#if defined(__has_include)
# if __has_include(<util.h>)
#  include <util.h>
# endif
# if __has_include(<sys/ioctl.h>)
#  include <sys/ioctl.h>
# endif
# if __has_include(<termios.h>)
#  include <termios.h>
# endif
# if __has_include(<libutil.h>)
#  include <libutil.h>
# endif
#endif
int main() {
  int master_fd = 0;
  int slave_fd = 0;
  return openpty(&master_fd, &slave_fd, nullptr, nullptr, nullptr);
}
]=] HAVE_DECL_OPENPTY)
check_symbol_exists(sigaction "signal.h" HAVE_DECL_SIGACTION)
check_symbol_exists(tempnam "stdio.h" HAVE_DECL_TEMPNAM)
check_symbol_exists(wait "sys/wait.h" HAVE_DECL_WAIT)
check_symbol_exists(putenv "stdlib.h" HAVE_PUTENV)
check_symbol_exists(setenv "stdlib.h" HAVE_SETENV_)

# -- Memory-mapping flags --
check_symbol_exists(MAP_ANONYMOUS "sys/mman.h" HAVE_MAP_ANONYMOUS)
check_symbol_exists(MAP_ANON "sys/mman.h" HAVE_MAP_ANON)
if(HAVE_MAP_ANONYMOUS)
  set(OPENAXIOM_MM_ANONYMOUS_MAP_FLAG "MAP_ANONYMOUS")
elseif(HAVE_MAP_ANON)
  set(OPENAXIOM_MM_ANONYMOUS_MAP_FLAG "MAP_ANON")
else()
  set(OPENAXIOM_MM_ANONYMOUS_MAP_FLAG "0")
endif()

# CMAKE_CXX_BYTE_ORDER is set automatically since CMake 3.20; use it
# instead of the deprecated TestBigEndian module.
if(CMAKE_CXX_BYTE_ORDER STREQUAL "BIG_ENDIAN")
  set(WORDS_BIGENDIAN 1)
endif()

# -- 4. Platform-triple and path layout -----------------------------------
# The target triple follows the same convention as the Autotools build
# (e.g. x86_64-unknown-linux-gnu).  OA_STAGE_ROOT is the in-tree staging
# directory that mirrors the final install layout; OA_INSTALL_LAYOUT_ROOT
# is the relative path under the install prefix.

if(WIN32)
  set(OPENAXIOM_EXEEXT ".exe")
  set(OPENAXIOM_INTERNAL_PATH_SEPARATOR ";")
  set(OA_TARGET_TRIPLE "${CMAKE_SYSTEM_PROCESSOR}-pc-windows")
elseif(APPLE)
  set(OPENAXIOM_EXEEXT "")
  set(OPENAXIOM_INTERNAL_PATH_SEPARATOR ":")
  set(OA_TARGET_TRIPLE "${CMAKE_SYSTEM_PROCESSOR}-apple-darwin")
else()
  set(OPENAXIOM_EXEEXT "")
  set(OPENAXIOM_INTERNAL_PATH_SEPARATOR ":")
  set(OA_TARGET_TRIPLE "${CMAKE_SYSTEM_PROCESSOR}-unknown-linux-gnu")
endif()

# OA_STAGE_ROOT: a build-tree directory laid out exactly like the final
# install tree.  Later bootstrap phases (algebra, databases, etc.) read
# binaries and headers from this staging directory instead of the raw
# build tree so that paths match the eventual installed layout.
#
# OA_INSTALL_LAYOUT_ROOT: the triple-and-version-qualified directory
# under the install prefix (e.g. lib/open-axiom/x86_64-.../1.5.0).
# This keeps multiple installed versions / architectures coexisting.
set(OA_STAGE_ROOT "${PROJECT_BINARY_DIR}/${OA_TARGET_TRIPLE}")
set(OA_INSTALL_LAYOUT_ROOT "${CMAKE_INSTALL_LIBDIR}/open-axiom/${OA_TARGET_TRIPLE}/${PROJECT_VERSION}")

# -- 5. External toolchain discovery --------------------------------------
# Locate the Lisp runtime (central to the bootstrap chain), plus optional
# tools: Qt for the GUI front-end, LLVM for native code generation, and
# TeX for documentation.

# Lisp runtime -- accepts either a bare name (looked up via find_program)
# or an absolute path to the binary.
if(IS_ABSOLUTE "${OA_WITH_LISP}" AND EXISTS "${OA_WITH_LISP}")
  set(OA_LISP_EXECUTABLE "${OA_WITH_LISP}")
else()
  find_program(OA_LISP_EXECUTABLE NAMES ${OA_WITH_LISP})
endif()
find_program(OA_QT_MOC NAMES moc moc-qt5 moc-qt6)
find_program(OA_QT_QMAKE NAMES qmake qmake-qt5 qmake-qt6)
find_program(PDFLATEX_EXECUTABLE NAMES pdflatex)
find_program(LATEX_EXECUTABLE NAMES latex)
find_program(MAKEINDEX_EXECUTABLE NAMES makeindex)
find_program(LLVM_CONFIG_EXECUTABLE NAMES llvm-config llvm-config-18 llvm-config-17)

# Configure the Lisp runtime -- this probes the binary (if found) and sets
# flavor-specific variables for FASL extensions, FFI types, linking model,
# etc.  See cmake/OpenAxiomLispRuntime.cmake for full documentation.
include(OpenAxiomLispRuntime)
oa_configure_lisp_runtime()

# -- 6. Derived feature flags ---------------------------------------------
# Combine user options with detection results to produce the final
# feature switches consumed by the source tree.

if(OA_ENABLE_LLVM AND LLVM_CONFIG_EXECUTABLE)
  set(OPENAXIOM_HOST_HAS_LLVM 1)
else()
  set(OPENAXIOM_HOST_HAS_LLVM 0)
endif()

if(OA_ENABLE_GUI AND OA_QT_MOC AND OA_QT_QMAKE)
  # Use CMake's native Qt support instead of driving qmake externally.
  # Try Qt6 first, fall back to Qt5.  The Widgets component is the only
  # one needed for the conversation / debate / main-window GUI.
  find_package(Qt6 QUIET COMPONENTS Widgets)
  if(Qt6_FOUND)
    set(OA_QT_TARGET Qt6::Widgets)
    set(OA_QT_VERSION "${Qt6_VERSION}")
  else()
    find_package(Qt5 QUIET COMPONENTS Widgets)
    if(Qt5_FOUND)
      set(OA_QT_TARGET Qt5::Widgets)
      set(OA_QT_VERSION "${Qt5_VERSION}")
    endif()
  endif()
  if(OA_QT_TARGET)
    set(OPENAXIOM_USE_GUI 1)
  else()
    message(STATUS "Qt moc/qmake found but Qt Widgets package not available; GUI disabled")
    set(OPENAXIOM_USE_GUI 0)
  endif()
else()
  set(OPENAXIOM_USE_GUI 0)
endif()

if(OA_ENABLE_SMAN AND HAVE_DECL_FORK AND HAVE_DECL_WAIT)
  set(OPENAXIOM_USE_SMAN 1)
else()
  set(OPENAXIOM_USE_SMAN 0)
endif()

if(OA_ENABLE_GRAPHICS AND HAVE_X11_INTRINSIC_H AND HAVE_X11_XPM_H)
  # Now that we know the headers exist, use find_package(X11) to get
  # proper imported targets for linking (X11::X11, X11::Xt, X11::Xpm).
  # The header checks above remain because they gate OA_HAVE_X11 for
  # the generated config header; find_package adds link-time resolution.
  find_package(X11 REQUIRED COMPONENTS Xt Xpm)
  unset(X_DISPLAY_MISSING)
  set(OA_HAVE_X11 1)
else()
  set(X_DISPLAY_MISSING 1)
  set(OA_HAVE_X11 0)
endif()

if(OA_ENABLE_THREADS)
  find_package(Threads REQUIRED)
endif()

if(OA_ENABLE_DOCS)
  if(NOT PDFLATEX_EXECUTABLE AND NOT LATEX_EXECUTABLE)
    message(STATUS "Documentation tools not found; docs remain disabled for this configuration")
  endif()
endif()

if(WIN32)
  message(WARNING "Windows-native OpenAxiom builds remain unsupported; use WSL for the CMake migration path")
endif()

if(NOT OA_LISP_EXECUTABLE)
  message(WARNING "Requested Lisp runtime '${OA_WITH_LISP}' was not found in PATH; the Phase 3 Lisp target will remain unavailable")
endif()

# -- 7. Configuration summary ---------------------------------------------

message(STATUS "OpenAxiom CMake configuration summary:")
message(STATUS "  Lisp runtime      : ${OA_WITH_LISP} (${OA_LISP_EXECUTABLE})")
message(STATUS "  Lisp flavor       : ${OA_LISP_FLAVOR}")
message(STATUS "  FASL/LNK ext      : ${OA_LISP_FASL_EXT} / ${OA_LISP_LNK_EXT}")
message(STATUS "  delayed FFI       : ${oa_delay_ffi}")
message(STATUS "  LLVM detected     : ${OPENAXIOM_HOST_HAS_LLVM}")
message(STATUS "  X11 detected      : ${OA_HAVE_X11}")
message(STATUS "  Qt detected       : ${OPENAXIOM_USE_GUI} (${OA_QT_VERSION})")
message(STATUS "  SMAN enabled      : ${OPENAXIOM_USE_SMAN}")
message(STATUS "  regex.h available : ${HAVE_REGEX_H}")
message(STATUS "  pdf tools         : ${PDFLATEX_EXECUTABLE};${LATEX_EXECUTABLE};${MAKEINDEX_EXECUTABLE}")
message(STATUS "  target triple     : ${OA_TARGET_TRIPLE}")
message(STATUS "  stage root        : ${OA_STAGE_ROOT}")
message(STATUS "  install root      : ${OA_INSTALL_LAYOUT_ROOT}")

# -- 8. Generated headers and templates -----------------------------------
# Generate the C/C++ config header (substituting HAVE_* probes) and the
# Lisp settings file (substituting Lisp runtime variables).

file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/config")
file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/include/open-axiom")
file(MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/share/open-axiom/cmake")

# The C/C++ config header is generated twice:
#   1. Under config/  -- consumed by src/CMakeLists.txt include paths.
#   2. Under include/open-axiom/config -- matches the header path that
#      the source code uses (#include <open-axiom/config>).  The
#      Autotools build used a similar two-path scheme.
configure_file(
  "${PROJECT_SOURCE_DIR}/cmake/openaxiom-c-macros.h.in"
  "${PROJECT_BINARY_DIR}/config/openaxiom-c-macros.h"
  @ONLY
)
configure_file(
  "${PROJECT_SOURCE_DIR}/cmake/openaxiom-c-macros.h.in"
  "${PROJECT_BINARY_DIR}/include/open-axiom/config"
  @ONLY
)

# Lisp settings file -- captures the full Lisp runtime configuration so
# that later CMake-based build phases (algebra, databases) can simply
# include() this file instead of re-running detection.
configure_file(
  "${PROJECT_SOURCE_DIR}/cmake/openaxiom-lisp-settings.cmake.in"
  "${PROJECT_BINARY_DIR}/config/openaxiom-lisp-settings.cmake"
  @ONLY
)

# -- 9. Source tree --------------------------------------------------------

add_subdirectory(src)
