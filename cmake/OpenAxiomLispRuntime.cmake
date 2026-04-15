# OpenAxiomLispRuntime.cmake -- Lisp runtime detection and configuration
#
# This module probes the host Lisp system at CMake configure time and sets
# the full matrix of flavor-specific variables that later build stages need
# to compile Lisp sources, link base images, and generate FFI type tables.
#
# Public entry point:
#   oa_configure_lisp_runtime()
#     Reads OA_WITH_LISP and OA_LISP_EXECUTABLE from the calling scope,
#     queries the runtime, and exports ~25 variables into the parent scope.
#
# Supported Lisp flavors: SBCL, ECL, CLisp, GCL, Clozure.
#
# Internal helpers (not intended for external use):
#   oa_parse_lisp_flavor(OUTVAR RAW_TEXT)
#   oa_list_to_string(OUTVAR ...)
#   oa_query_lisp(OUTPUT_VAR EXPRESSION <expr>)
#   oa_set_ffi_type_table(flavor precision)


# ---------------------------------------------------------------------------
# oa_parse_lisp_flavor -- classify a Lisp implementation from free-form text
# ---------------------------------------------------------------------------
# Examines the output of (lisp-implementation-type) or similar and returns a
# canonical flavor tag: sbcl, ecl, clisp, clozure, gcl, or "unknown".
function(oa_parse_lisp_flavor OUTVAR RAW_TEXT)
  string(TOLOWER "${RAW_TEXT}" _lisp_text)
  set(_flavor "unknown")

  if(_lisp_text MATCHES "sbcl")
    set(_flavor "sbcl")
  elseif(_lisp_text MATCHES "extensible common lisp|(^|[^a-z])ecl([^a-z]|$)")
    set(_flavor "ecl")
  elseif(_lisp_text MATCHES "clisp")
    set(_flavor "clisp")
  elseif(_lisp_text MATCHES "clozure|ccl")
    set(_flavor "clozure")
  elseif(_lisp_text MATCHES "gcl")
    set(_flavor "gcl")
  endif()

  set(${OUTVAR} "${_flavor}" PARENT_SCOPE)
endfunction()

# ---------------------------------------------------------------------------
# oa_list_to_string -- join a CMake list into a space-separated string
# ---------------------------------------------------------------------------
function(oa_list_to_string OUTVAR)
  string(REPLACE ";" " " _value "${ARGN}")
  set(${OUTVAR} "${_value}" PARENT_SCOPE)
endfunction()

# ---------------------------------------------------------------------------
# oa_query_lisp -- evaluate a Lisp expression at configure time
# ---------------------------------------------------------------------------
# Runs OA_LISP_EXECUTABLE in batch mode, evaluating EXPRESSION.  Both stdout
# and stderr are captured and returned (some Lisps print useful diagnostics
# to stderr).  The caller must set OA_LISP_QUIET_ARGS, OA_LISP_BATCH_ARGS,
# OA_LISP_EVAL_FLAG, and optionally OA_LISP_ENV_VARS before calling.
#
# Any environment variables in OA_LISP_ENV_VARS (e.g. GCL_ANSI=t) are
# injected via "cmake -E env".
function(oa_query_lisp OUTPUT_VAR)
  set(options)
  set(oneValueArgs EXPRESSION)
  cmake_parse_arguments(OA_QUERY "${options}" "${oneValueArgs}" "" ${ARGN})

  if(NOT OA_LISP_EXECUTABLE)
    set(${OUTPUT_VAR} "" PARENT_SCOPE)
    return()
  endif()

  # Build the optional environment-variable prefix.
  set(_env_args)
  if(OA_LISP_ENV_VARS)
    set(_env_args ${OA_LISP_ENV_VARS})
  endif()

  execute_process(
    COMMAND ${CMAKE_COMMAND} -E env ${_env_args}
      "${OA_LISP_EXECUTABLE}" ${OA_LISP_QUIET_ARGS} ${OA_LISP_BATCH_ARGS}
      ${OA_LISP_EVAL_FLAG} "${OA_QUERY_EXPRESSION}"
    RESULT_VARIABLE _result
    OUTPUT_VARIABLE _output
    ERROR_VARIABLE _error
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_STRIP_TRAILING_WHITESPACE
  )

  # Merge stdout and stderr -- some Lisps emit results on either stream.
  set(${OUTPUT_VAR} "${_output}\n${_error}" PARENT_SCOPE)
endfunction()


# ---------------------------------------------------------------------------
# oa_set_ffi_type_table -- populate flavor-specific FFI type names
# ---------------------------------------------------------------------------
# Each Lisp spells the fundamental C types differently in its FFI layer.
# This macro sets _void_type ... _pointer_type in the *caller's* scope.
# The tables below are transcribed from config/open-axiom.m4.
#
# Uses a macro (not a function) so that the variables are set directly in
# the enclosing scope without needing PARENT_SCOPE on every line.
macro(oa_set_ffi_type_table _flavor _precision)
  if("${_flavor}" STREQUAL "gcl")
    set(_void_type   "void")
    set(_char_type   "char")
    set(_int_type    "int")
    set(_float_type  "float")
    set(_double_type "double")
    set(_string_type "string")
    if(${_precision} EQUAL 64)
      set(_pointer_type "(signed-integer 64)")
    else()
      set(_pointer_type "fixnum")
    endif()

  elseif("${_flavor}" STREQUAL "sbcl")
    set(_void_type   "void")
    set(_char_type   "char")
    set(_int_type    "int")
    set(_float_type  "float")
    set(_double_type "double")
    set(_string_type "c-string")
    set(_pointer_type "(* t)")

  elseif("${_flavor}" STREQUAL "clisp")
    set(_void_type   "nil")
    set(_char_type   "character")
    set(_int_type    "int")
    set(_float_type  "single-float")
    set(_double_type "double-float")
    set(_string_type "c-string")
    set(_pointer_type "c-pointer")

  elseif("${_flavor}" STREQUAL "ecl")
    set(_void_type   ":void")
    set(_char_type   ":char")
    set(_int_type    ":int")
    set(_float_type  ":float")
    set(_double_type ":double")
    set(_string_type ":cstring")
    set(_pointer_type ":pointer-void")

  elseif("${_flavor}" STREQUAL "clozure")
    set(_void_type   ":void")
    set(_char_type   ":unsigned-byte")
    set(_int_type    ":signed-fullword")
    set(_float_type  ":single-float")
    set(_double_type ":double-float")
    set(_string_type ":address")
    set(_pointer_type ":address")

  else()
    # Fallback -- use SBCL-compatible names as a reasonable default.
    set(_void_type   "void")
    set(_char_type   "char")
    set(_int_type    "int")
    set(_float_type  "float")
    set(_double_type "double")
    set(_string_type "c-string")
    set(_pointer_type "(* t)")
  endif()
endmacro()


# ===========================================================================
# oa_configure_lisp_runtime -- master configuration entry point
# ===========================================================================
# Reads:
#   OA_WITH_LISP         -- user-requested flavor name (e.g. "sbcl", "gcl")
#   OA_LISP_EXECUTABLE   -- absolute path to the Lisp binary (may be empty)
#
# Exports into the parent scope (see end of function for the full list):
#   OA_LISP_FLAVOR, OA_LISP_FASL_EXT, OA_LISP_LNK_EXT,
#   OA_LISP_QUIET_ARGS, OA_LISP_BATCH_ARGS, OA_LISP_EVAL_FLAG,
#   OA_LISP_ENV_VARS, OPENAXIOM_BASE_RTS, OPENAXIOM_HOST_LISP_PRECISION,
#   oa_lisp_flavor, oa_standard_linking, oa_delay_ffi, oa_use_dynamic_lib,
#   oa_quiet_flags, oa_eval_flags, oa_optimize_options,
#   void_type ... pointer_type  (FFI type names for core.lisp.in substitution)
function(oa_configure_lisp_runtime)
  string(TOLOWER "${OA_WITH_LISP}" _requested_flavor)

  # -------------------------------------------------------------------
  # 1. Set up the command-line protocol for each Lisp flavor.
  #    Each Lisp has its own way of being invoked non-interactively:
  #      quiet args   -- suppress banners and prompts
  #      batch args   -- skip user/site init files
  #      eval flag    -- the flag that precedes a Lisp expression
  #      env vars     -- environment variables needed (e.g. GCL_ANSI=t)
  # -------------------------------------------------------------------
  set(_quiet_args)
  set(_batch_args)
  set(_eval_flag --eval)
  set(_env_vars)

  if(_requested_flavor STREQUAL "gcl")
    set(_quiet_args -batch)
    set(_eval_flag -eval)
    # GCL 2.7 defaults to CLtL1 mode where DEFPACKAGE is unavailable.
    # Setting GCL_ANSI=t selects the ANSI image instead.
    set(_env_vars "GCL_ANSI=t")
  elseif(_requested_flavor STREQUAL "ecl")
    set(_batch_args -norc)
    set(_eval_flag -eval)
  elseif(_requested_flavor STREQUAL "sbcl")
    set(_quiet_args --noinform --noprint)
    set(_batch_args --no-sysinit --no-userinit --disable-debugger)
    set(_eval_flag --eval)
  elseif(_requested_flavor STREQUAL "clisp")
    set(_quiet_args --quiet)
    set(_batch_args -norc)
    set(_eval_flag -x)
  elseif(_requested_flavor STREQUAL "clozure")
    set(_quiet_args --quiet --no-init)
    set(_eval_flag --eval)
  endif()

  # Publish invocation protocol for both local use and parent scope.
  set(OA_LISP_QUIET_ARGS ${_quiet_args} PARENT_SCOPE)
  set(OA_LISP_BATCH_ARGS ${_batch_args} PARENT_SCOPE)
  set(OA_LISP_EVAL_FLAG  ${_eval_flag}  PARENT_SCOPE)
  set(OA_LISP_ENV_VARS   ${_env_vars}   PARENT_SCOPE)
  # Local copies so oa_query_lisp can read them within this function.
  set(OA_LISP_QUIET_ARGS ${_quiet_args})
  set(OA_LISP_BATCH_ARGS ${_batch_args})
  set(OA_LISP_EVAL_FLAG  ${_eval_flag})
  set(OA_LISP_ENV_VARS   ${_env_vars})

  # -------------------------------------------------------------------
  # 2. Establish default FASL extension and host-precision fallbacks.
  #    These are overridden below if the Lisp is actually queryable.
  # -------------------------------------------------------------------
  set(_detected_flavor "${_requested_flavor}")
  set(_fasl_ext "fasl")
  set(_precision "${CMAKE_SIZEOF_VOID_P}")
  math(EXPR _precision "${_precision} * 8")

  # Static per-flavor defaults for when the binary is not available.
  if(_requested_flavor STREQUAL "gcl")
    set(_fasl_ext "o")
  elseif(_requested_flavor STREQUAL "clisp"
      OR _requested_flavor STREQUAL "ecl")
    set(_fasl_ext "fas")
  endif()

  # -------------------------------------------------------------------
  # 3. Query the actual Lisp runtime, if available.
  #    Three queries: implementation type, *features*, FASL extension.
  # -------------------------------------------------------------------
  if(OA_LISP_EXECUTABLE)
    # 3a. Identify the implementation type to confirm the flavor.
    oa_query_lisp(_impl_dump
      EXPRESSION "(progn (format t \"oa_lisp_implementation=~a\" (lisp-implementation-type)) (quit))")
    oa_parse_lisp_flavor(_parsed_flavor "${_impl_dump}")
    if(NOT _parsed_flavor STREQUAL "unknown")
      set(_detected_flavor "${_parsed_flavor}")
    endif()

    # 3b. Dump *features* for precision detection and capability checks.
    oa_query_lisp(_feature_dump
      EXPRESSION "(progn (dolist (feature *features*) (format t \"~a \" feature)) (quit))")
    string(TOUPPER "${_feature_dump}" _feature_dump_upper)

    # 3c. Ask the compiler for the actual FASL file extension.
    oa_query_lisp(_fasl_dump
      EXPRESSION "(progn (format t \"oa_fasl_type=~a\" (pathname-type (compile-file-pathname \"foo.lisp\"))) (quit))")
    string(REGEX MATCH "oa_fasl_type=([^ \r\n]+)" _fasl_match "${_fasl_dump}")
    if(CMAKE_MATCH_1)
      set(_fasl_ext "${CMAKE_MATCH_1}")
    endif()

    # Determine host word size from feature flags.
    if(_feature_dump_upper MATCHES "X86-64|X86_64|WORD-SIZE=64|64-BIT")
      set(_precision 64)
    else()
      set(_precision 32)
    endif()

    # CLisp without FFI support cannot build OpenAxiom.
    if(_detected_flavor STREQUAL "clisp" AND NOT _feature_dump_upper MATCHES "FFI")
      message(FATAL_ERROR
        "The selected CLISP runtime does not report FFI support, which OpenAxiom requires")
    endif()
  endif()

  # -------------------------------------------------------------------
  # 4. Derive the linking model from the detected flavor.
  #
  #    ECL uses "standard linking" -- it compiles Lisp to C, producing
  #    native object files (.o) that are linked by the C toolchain.
  #    All other flavors produce their own linkable FASL format.
  #
  #    Delayed FFI means FFI stubs are resolved at image-load time
  #    rather than at compile time (SBCL, CLisp, Clozure).
  # -------------------------------------------------------------------
  string(REGEX REPLACE "^\\." "" _objext "${CMAKE_C_OUTPUT_EXTENSION}")
  if(NOT _objext)
    set(_objext "o")
  endif()

  set(_standard_linking "no")
  set(_delay_ffi        "no")
  set(_use_dynamic_lib  "yes")
  set(_lnkext           "${_fasl_ext}")

  if(_detected_flavor STREQUAL "ecl")
    set(_standard_linking "yes")
    set(_use_dynamic_lib  "no")
    set(_lnkext           "${_objext}")
  elseif(_detected_flavor STREQUAL "gcl")
    set(_use_dynamic_lib  "no")
  endif()

  if(_detected_flavor STREQUAL "sbcl"
      OR _detected_flavor STREQUAL "clozure"
      OR _detected_flavor STREQUAL "clisp")
    set(_delay_ffi "yes")
  endif()

  # -------------------------------------------------------------------
  # 5. Populate the FFI type table (delegated to the macro above).
  # -------------------------------------------------------------------
  oa_set_ffi_type_table("${_detected_flavor}" "${_precision}")

  # -------------------------------------------------------------------
  # 6. Choose Lisp compiler optimisation options.
  #    When runtime checking or profiling is requested we add safety
  #    (and debug, where supported) so the Lisp compiler preserves
  #    enough metadata for useful diagnostics.
  # -------------------------------------------------------------------
  set(_optimize_options "speed")
  if(OA_ENABLE_RUNTIME_CHECKING OR OA_ENABLE_PROFILING)
    if(_detected_flavor STREQUAL "gcl")
      # GCL does not support (optimize ... (debug N)).
      set(_optimize_options "speed safety")
    else()
      set(_optimize_options "speed safety debug")
    endif()
  endif()

  # Convert list-form flags into the space-separated strings that
  # core.lisp.in expects.
  oa_list_to_string(_quiet_flags ${_quiet_args})
  oa_list_to_string(_eval_flags  ${_batch_args} ${_eval_flag})

  # -------------------------------------------------------------------
  # 7. Export everything into the parent scope.
  # -------------------------------------------------------------------

  # Variables used by CMake build rules (src/CMakeLists.txt).
  set(OA_LISP_FLAVOR    "${_detected_flavor}"            PARENT_SCOPE)
  set(OA_LISP_FASL_EXT  "${_fasl_ext}"                   PARENT_SCOPE)
  set(OA_LISP_LNK_EXT   "${_lnkext}"                     PARENT_SCOPE)
  # Map the detected flavor to the C++ Runtime:: enum.  If the flavor
  # does not match any known enumerator, fall back to Runtime::unknown
  # so that the generated header is always valid C++.
  if(_detected_flavor MATCHES "^(gcl|ecl|sbcl|clisp|clozure|bemol|polyml)$")
    set(OPENAXIOM_BASE_RTS "Runtime::${_detected_flavor}" PARENT_SCOPE)
  else()
    set(OPENAXIOM_BASE_RTS "Runtime::unknown"             PARENT_SCOPE)
  endif()
  set(OPENAXIOM_HOST_LISP_PRECISION "${_precision}"       PARENT_SCOPE)

  # Variables substituted into core.lisp.in via @VAR@.
  set(oa_lisp_flavor      "${_detected_flavor}"  PARENT_SCOPE)
  set(oa_standard_linking  "${_standard_linking}" PARENT_SCOPE)
  set(oa_delay_ffi         "${_delay_ffi}"        PARENT_SCOPE)
  set(oa_use_dynamic_lib   "${_use_dynamic_lib}"  PARENT_SCOPE)
  set(oa_quiet_flags       "${_quiet_flags}"      PARENT_SCOPE)
  set(oa_eval_flags        "${_eval_flags}"       PARENT_SCOPE)
  set(oa_optimize_options  "${_optimize_options}"  PARENT_SCOPE)

  # FFI type names (substituted into core.lisp.in).
  set(void_type    "${_void_type}"    PARENT_SCOPE)
  set(char_type    "${_char_type}"    PARENT_SCOPE)
  set(int_type     "${_int_type}"     PARENT_SCOPE)
  set(float_type   "${_float_type}"   PARENT_SCOPE)
  set(double_type  "${_double_type}"  PARENT_SCOPE)
  set(string_type  "${_string_type}"  PARENT_SCOPE)
  set(pointer_type "${_pointer_type}" PARENT_SCOPE)
endfunction()
