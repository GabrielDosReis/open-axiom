# RunCommand.cmake -- Execute a command via execute_process().
#
# This wrapper avoids cmd.exe interpreting special characters (e.g. the
# pipe character |) which appear in Common Lisp symbol names like
# |AxiomCore|::|topLevel|.
#
# Usage:
#   cmake -DCMDFILE=<file> -P RunCommand.cmake
#
# CMDFILE is a text file containing the program and arguments, one per line.
# The first line is the program, subsequent lines are arguments.
# WORKDIR (optional) sets the working directory.

if(NOT CMDFILE)
  message(FATAL_ERROR "RunCommand.cmake: CMDFILE variable not set")
endif()

file(STRINGS "${CMDFILE}" _lines)
list(GET _lines 0 _prog)
list(REMOVE_AT _lines 0)

# STDINFILE (optional) is fed to the command as standard input.  GCL's
# compiler::link image build reads its link form this way (mirroring the
# Autotools `| ./base-lisp`).
if(STDINFILE)
  execute_process(
    COMMAND "${_prog}" ${_lines}
    WORKING_DIRECTORY "${WORKDIR}"
    INPUT_FILE "${STDINFILE}"
    RESULT_VARIABLE _rc
  )
else()
  execute_process(
    COMMAND "${_prog}" ${_lines}
    WORKING_DIRECTORY "${WORKDIR}"
    RESULT_VARIABLE _rc
  )
endif()
if(NOT _rc EQUAL 0)
  message(FATAL_ERROR "Command failed with exit code ${_rc}")
endif()
