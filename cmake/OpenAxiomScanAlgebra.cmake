# ===========================================================================
# cmake/OpenAxiomScanAlgebra.cmake -- algebra source manifest and constructor scan
# ===========================================================================
#
# STRATEGY
# --------
# The OpenAxiom algebra evolves by adding new pamphlets (*.spad.pamphlet)
# or plain SPAD files (*.spad) under src/algebra/.  This module reads an
# explicit source manifest and scans those files for constructor definitions.
#
# WHY A MANIFEST INSTEAD OF GLOBBING
# -----------------------------------
# An earlier version used file(GLOB CONFIGURE_DEPENDS) to discover algebra
# sources.  This was abandoned because:
#
#   (a) Fragile inclusion.  Stale extracted .spad files from a previous
#       build, editor backup files, or test files copied in for debugging
#       would be silently picked up by a *.spad glob.  The *.spad.pamphlet
#       extension is more distinctive, but the risk remains.
#
#   (b) Unreliable reconfigure.  CMake's own documentation warns that
#       CONFIGURE_DEPENDS is a best-effort hint not supported by all
#       generators, and even where supported, rapid add/remove sequences
#       can be missed.
#
#   (c) Intentionality.  An explicit manifest makes it clear which files
#       are part of the algebra.  A developer adding a new file must
#       consciously register it -- a one-line edit that also serves as a
#       reviewable record of what changed.
#
# The manifest is src/algebra/SOURCES -- one basename per line, comments
# and blank lines allowed.  Each entry is either:
#   - A pamphlet basename (e.g. "catdef") for which catdef.spad.pamphlet
#     must exist in src/algebra/.
#   - A plain .spad basename (e.g. "newfoo") for which newfoo.spad must
#     exist.  The scanner auto-detects which by checking for the .pamphlet
#     extension first.
#
# ARCHITECTURE
# ------------
# The module performs three jobs:
#
# 1. MANIFEST READING
#    Parse src/algebra/SOURCES.  For each basename, determine whether a
#    .spad.pamphlet or .spad file exists.  Error if neither is found.
#    Warn if the directory contains algebra-like files not in the manifest
#    (catches forgotten additions).
#
# 2. CONSTRUCTOR SCANNING
#    For pamphlets, scan for noweb chunk markers:
#        <<(category|domain|package) ABBREV FullName>>=
#    For plain .spad files, scan for abbreviation directives:
#        )abbrev (category|domain|package) ABBREV FullName
#    This replicates the Autotools configure.ac egrep that generates
#    src/algebra/extract.mk, but extends it to handle plain .spad files.
#
# 3. VARIABLE POPULATION
#    For each constructor found, record:
#      - OA_ALGEBRA_CTOR_SOURCE_<A>  : source type ("pamphlet" or "plain")
#      - OA_ALGEBRA_CTOR_PAMPHLET_<A>: basename of the pamphlet (pamphlet only)
#      - OA_ALGEBRA_CTOR_CHUNK_<A>   : chunk description for extraction (pamphlet only)
#      - OA_ALGEBRA_CTOR_SPADFILE_<A> : basename of the plain .spad file (plain only)
#    And aggregate lists:
#      - OA_ALGEBRA_CONSTRUCTORS     : sorted list of all constructor abbreviations
#      - OA_ALGEBRA_CTOR_COUNT       : total number of constructors
#      - OA_ALGEBRA_PAMPHLET_BASES   : unique pamphlet basenames (for whole-file extraction)
#      - OA_ALGEBRA_PLAIN_SPADFILES  : unique plain .spad basenames (no extraction needed)
#      - OA_ALGEBRA_ALL_SPADFILES    : union of the above two (for initdb scanning)
#
# DEVELOPMENT WORKFLOW
# --------------------
# To add a new algebra file:
#   (a) Create foo.spad.pamphlet or foo.spad in src/algebra/.
#   (b) Add "foo" to src/algebra/SOURCES (one line, anywhere in the file).
#   (c) Re-run cmake.
#   (d) The new file is scanned, constructors discovered, and any
#       unassigned constructor lands in the catch-all layer (see
#       OpenAxiomAlgebraLayers.cmake).
#   (e) Optionally, move the constructor to an earlier explicit layer
#       for better parallel build performance.
#
# Usage:
#   include(OpenAxiomScanAlgebra)
#   oa_scan_algebra_sources(<srcdir>)
#
# where <srcdir> is the path to src/algebra/ (which must contain SOURCES).
# ===========================================================================


# ---------------------------------------------------------------------------
# oa_scan_algebra_sources(<srcdir>)
#
# Read the SOURCES manifest in <srcdir>, classify each entry as pamphlet
# or plain .spad, scan for constructor definitions, and populate all
# per-constructor and aggregate variables in the caller's scope.
# ---------------------------------------------------------------------------

function(oa_scan_algebra_sources srcdir)

  # -----------------------------------------------------------------------
  # Phase 1: Read the SOURCES manifest
  # -----------------------------------------------------------------------
  # The manifest is a plain text file: one basename per line, blank lines
  # and lines starting with '#' are ignored.  Each basename identifies
  # either a .spad.pamphlet or a .spad file in the same directory.

  set(_manifest "${srcdir}/SOURCES")
  if(NOT EXISTS "${_manifest}")
    message(FATAL_ERROR
      "Missing algebra source manifest: ${_manifest}\n"
      "Create it with one basename per line (e.g. 'catdef' for catdef.spad.pamphlet).")
  endif()

  file(STRINGS "${_manifest}" _entries)

  set(_pamphlet_bases "")
  set(_plain_bases "")

  foreach(_entry ${_entries})
    # Skip comments and blank lines.
    string(STRIP "${_entry}" _entry)
    if("${_entry}" STREQUAL "" OR "${_entry}" MATCHES "^#")
      continue()
    endif()

    # Determine source type: pamphlet takes precedence over plain .spad.
    if(EXISTS "${srcdir}/${_entry}.spad.pamphlet")
      list(APPEND _pamphlet_bases "${_entry}")
    elseif(EXISTS "${srcdir}/${_entry}.spad")
      list(APPEND _plain_bases "${_entry}")
    else()
      message(FATAL_ERROR
        "SOURCES entry '${_entry}' has no matching file in ${srcdir}.\n"
        "Expected ${_entry}.spad.pamphlet or ${_entry}.spad")
    endif()
  endforeach()

  if(NOT _pamphlet_bases AND NOT _plain_bases)
    message(FATAL_ERROR
      "No valid entries in ${_manifest}.\n"
      "Add algebra basenames, one per line.")
  endif()

  # -----------------------------------------------------------------------
  # Phase 1b: Warn about files in the directory not in the manifest
  # -----------------------------------------------------------------------
  # This is a development aid: if someone adds a file but forgets to
  # update SOURCES, a configure-time warning catches it early.

  file(GLOB _existing_pamphlets "${srcdir}/*.spad.pamphlet")
  file(GLOB _existing_spads    "${srcdir}/*.spad")

  set(_manifest_all ${_pamphlet_bases} ${_plain_bases})

  foreach(_pam ${_existing_pamphlets})
    get_filename_component(_name "${_pam}" NAME)
    string(REGEX REPLACE "\\.spad\\.pamphlet$" "" _base "${_name}")
    list(FIND _manifest_all "${_base}" _idx)
    if(_idx EQUAL -1)
      message(WARNING
        "File ${_name} exists in ${srcdir} but is not listed in SOURCES. "
        "Add '${_base}' to SOURCES if it should be part of the algebra.")
    endif()
  endforeach()

  # For plain .spad files, only warn about those that do NOT have a
  # corresponding pamphlet (otherwise they are extracted artifacts).
  foreach(_sf ${_existing_spads})
    get_filename_component(_name "${_sf}" NAME)
    string(REGEX REPLACE "\\.spad$" "" _base "${_name}")
    # Skip if a pamphlet exists (extracted artifact, not a source).
    if(EXISTS "${srcdir}/${_base}.spad.pamphlet")
      continue()
    endif()
    list(FIND _manifest_all "${_base}" _idx)
    if(_idx EQUAL -1)
      message(WARNING
        "File ${_name} exists in ${srcdir} but is not listed in SOURCES. "
        "Add '${_base}' to SOURCES if it should be part of the algebra.")
    endif()
  endforeach()

  # -----------------------------------------------------------------------
  # Phase 2a: Scan pamphlets for chunk markers
  # -----------------------------------------------------------------------
  # Pamphlets use noweb chunk syntax to define constructors:
  #   <<category SETCAT SetCategory>>=
  #   <<domain   INT   Integer>>=
  # Each chunk corresponds to one constructor that `hammer --tangle=<chunk>`
  # can extract into a standalone ABBREV.spad file.

  set(_all_ctors "")

  foreach(_base ${_pamphlet_bases})
    set(_pam "${srcdir}/${_base}.spad.pamphlet")
    file(STRINGS "${_pam}" _lines
      REGEX "^<<(category|domain|package) [A-Z][A-Z0-9]+ [A-Za-z0-9]+>>="
    )

    foreach(_line ${_lines})
      # Extract the chunk description (everything between << and >>=)
      string(REGEX REPLACE "^<<(.*)>>=$" "\\1" _chunk "${_line}")
      # Extract the abbreviation (second word)
      string(REGEX REPLACE "^[a-z]+ ([A-Z][A-Z0-9]+) .*$" "\\1" _abbrev "${_chunk}")

      list(APPEND _all_ctors "${_abbrev}")
      set(OA_ALGEBRA_CTOR_SOURCE_${_abbrev}   "pamphlet" PARENT_SCOPE)
      set(OA_ALGEBRA_CTOR_PAMPHLET_${_abbrev}  "${_base}" PARENT_SCOPE)
      set(OA_ALGEBRA_CTOR_CHUNK_${_abbrev}     "${_chunk}"   PARENT_SCOPE)
    endforeach()
  endforeach()

  # -----------------------------------------------------------------------
  # Phase 2b: Scan plain .spad files for )abbrev directives
  # -----------------------------------------------------------------------
  # Plain .spad files use the SPAD )abbrev directive:
  #   )abbrev category SETCAT SetCategory
  #   )abbrev domain   INT   Integer
  # Each )abbrev line defines one constructor.  A single .spad file may
  # contain multiple )abbrev directives (multi-constructor files).

  foreach(_base ${_plain_bases})
    set(_sf "${srcdir}/${_base}.spad")
    file(STRINGS "${_sf}" _lines
      REGEX "^\\)abbrev (category|domain|package) [A-Z][A-Z0-9]+ [A-Za-z]"
    )

    foreach(_line ${_lines})
      # Parse: )abbrev kind ABBREV FullName
      string(REGEX REPLACE
        "^\\)abbrev [a-z]+ ([A-Z][A-Z0-9]+) .*$" "\\1" _abbrev "${_line}")

      list(APPEND _all_ctors "${_abbrev}")
      set(OA_ALGEBRA_CTOR_SOURCE_${_abbrev}    "plain"    PARENT_SCOPE)
      set(OA_ALGEBRA_CTOR_SPADFILE_${_abbrev}   "${_base}" PARENT_SCOPE)
    endforeach()
  endforeach()

  # -----------------------------------------------------------------------
  # Phase 3: Deduplicate, sort, and export aggregate lists
  # -----------------------------------------------------------------------

  list(REMOVE_DUPLICATES _all_ctors)
  list(SORT _all_ctors)
  list(LENGTH _all_ctors _count)

  list(REMOVE_DUPLICATES _pamphlet_bases)
  list(SORT _pamphlet_bases)

  list(REMOVE_DUPLICATES _plain_bases)
  list(SORT _plain_bases)

  # The combined list of all .spad basenames, used for whole-file initdb
  # scanning.  Pamphlet basenames need extraction; plain basenames are
  # used directly (or copied into the build tree).
  set(_all_spadfiles ${_pamphlet_bases} ${_plain_bases})
  list(SORT _all_spadfiles)

  set(OA_ALGEBRA_CONSTRUCTORS     ${_all_ctors}       PARENT_SCOPE)
  set(OA_ALGEBRA_CTOR_COUNT       ${_count}            PARENT_SCOPE)
  set(OA_ALGEBRA_PAMPHLET_BASES   ${_pamphlet_bases}   PARENT_SCOPE)
  set(OA_ALGEBRA_PLAIN_SPADFILES  ${_plain_bases}      PARENT_SCOPE)
  set(OA_ALGEBRA_ALL_SPADFILES    ${_all_spadfiles}     PARENT_SCOPE)

  list(LENGTH _pamphlet_bases _npam)
  list(LENGTH _plain_bases _nplain)
  message(STATUS
    "Algebra scan: ${_count} constructors from "
    "${_npam} pamphlets + ${_nplain} plain .spad files")
endfunction()
