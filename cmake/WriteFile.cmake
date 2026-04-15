# cmake/WriteFile.cmake -- Write a string to a file.
# This helper is invoked as a CMake script via:
#   cmake -Dfile=<path> -Dcontent=<string> -P WriteFile.cmake
# It is used where file(WRITE ...) cannot be used directly in a
# custom command (e.g. inside a POST_BUILD step).

if(NOT DEFINED file)
  message(FATAL_ERROR "WriteFile.cmake: 'file' variable not set")
endif()
if(NOT DEFINED content)
  message(FATAL_ERROR "WriteFile.cmake: 'content' variable not set")
endif()

file(WRITE "${file}" "${content}\n")
