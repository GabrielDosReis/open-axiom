# ===========================================================================
# cmake/CopyIfExists.cmake -- copy a file only if the source exists
# ===========================================================================
# Used as a -P script to conditionally copy default-package FASLs.
# The SPAD compiler produces <CTOR>-.NRLIB/code.<FASLEXT> only for
# categories that have default implementations.
#
# Usage:
#   cmake -Dsrc=<source> -Ddst=<destination> -P CopyIfExists.cmake
# ===========================================================================

if(EXISTS "${src}")
  file(COPY_FILE "${src}" "${dst}" ONLY_IF_DIFFERENT)
endif()
