# ===========================================================================
# cmake/OpenAxiomAlgebraLayers.cmake -- Algebra layer definitions
# ===========================================================================
#
# This file defines the explicit compilation layers for the OpenAxiom
# algebra, transcribed from src/algebra/Makefile.am.  Each layer is a
# list of constructor abbreviations that can be built in parallel once
# all constructors in the previous layer are complete.
#
# ARCHITECTURE
# ------------
# The algebra compilation pipeline is:
#   1. Extract .spad from pamphlets / copy plain .spad (whole-file, for initdb)
#   2. Per-constructor extraction (pamphlets) or copy (plain .spad)
#   3. Build initdb by scanning all .spad files (--build-initdb)
#   4. Three-stage bootstrap: strap-0/1/2 (category + domain bootstrap)
#   5. Final compilation in layers: layer N depends on layer N-1
#
# FILE DISCOVERY AND LAYER ASSIGNMENT
# ------------------------------------
# The list of algebra source files (pamphlets and plain .spad) is NOT
# hardcoded here.  It is discovered dynamically by OpenAxiomScanAlgebra.cmake
# from the SOURCES manifest.  Adding a new file to src/algebra/ requires
# adding it to src/algebra/SOURCES; the scanner warns about unlisted files.
#
# Constructors are assigned to layers in two ways:
#   (a) EXPLICIT LAYERS (0-24): hand-curated, battle-tested ordering that
#       encodes known inter-constructor dependencies.
#   (b) CATCH-ALL LAYER (25, if non-empty): any constructor found by the
#       scanner but not assigned to an explicit layer.  This layer depends
#       on the last explicit layer, so all explicit constructors are
#       guaranteed available.
#
# The catch-all layer is computed by oa_algebra_build_catchall(), which
# must be called after oa_scan_algebra_sources() has populated
# OA_ALGEBRA_CONSTRUCTORS.
#
# Each layer variable is named oa_algebra_layer_<N> and contains a
# space-separated list of constructor abbreviations.  The SPAD compiler
# produces <ABBREV>.NRLIB/code.<FASLEXT> for each constructor; the build
# system copies the result to the algebra output directory.
#
# Layer 0 contains the most fundamental types (TYPE, BOOLEAN, INT, ...);
# Layer 24 contains AST nodes, JVM targets, and the compiler frontend.
#
# DEFAULT PACKAGES (FOO- entries)
# --------------------------------
# When the SPAD compiler compiles a category FOO, it automatically
# generates a companion "default package" FOO- whose FASL is produced
# as a byproduct of the same compilation.  Therefore, default-package
# abbreviations (e.g. SETCAT-, RING-, FIELD-) must NOT appear in layer
# lists -- they are implicitly produced when the parent category is
# compiled.  The build system declares FOO-.fasl as a BYPRODUCTS of
# the FOO compilation command.  Listing FOO- explicitly would create
# a duplicate OUTPUT rule, which Ninja rejects.
#
# NOTE: Constructor abbreviations are UPPERCASE and correspond to the
# )abbrev directives in the .spad source files.  Multiple constructors
# can be defined in a single .spad file (e.g. catdef.spad defines 76
# constructors including SETCAT, RING, FIELD, etc.).
# ===========================================================================


# ---------------------------------------------------------------------------
# Layer 0 -- Fundamental types
# ---------------------------------------------------------------------------
# The bedrock of the algebra: Type itself, booleans, integers, strings.
# These have minimal or no algebra dependencies (they depend only on
# strap-2 artifacts from the bootstrap stages).

set(oa_algebra_layer_0
  TYPE UTYPE VOID EXIT
  BOOLEAN INT NNI PI SINT SYMBOL
  DFLOAT IDENT STRING PAIR MAYBE LIST
)


# ---------------------------------------------------------------------------
# Layer 1 -- Core categories, basic domains, and aggregates
# ---------------------------------------------------------------------------
# The largest layer.  Contains the mathematical category hierarchy
# (SETCAT, RING, FIELD, ...), aggregate categories (HOAGG, FLAGG,
# VECTCAT, ...), and fundamental domains (OUTFORM, STREAM, ...).
#
# Default packages (FOO-) are NOT listed here; they are produced
# automatically when the parent category (FOO) is compiled.

set(oa_algebra_layer_1
  BINOPC BINOP IDEMOPC SGPOPC SGPOP MONOPC FUNCTOR COMOPC COMOP
  BASTYPE SETCAT SGROUP
  LLINSET RLINSET LINSET ABELSG CHARNZ
  ABELGRP ABELMON ORDTYPE
  RMODULE ALGEBRA FRETRCT
  FINITE MONOID GROUP
  RING OINTDOM AMR
  BMODULE STEP LMODULE PFECAT
  AHYP CFCAT ELTAB KOERCE KONVERT
  KRCFROM KVTFROM IEVALAB EVALAB
  RETRACT REPSQ REPDB FAMR
  PRIMCAT PTRANFN SPFCAT HOMOTOP DIFEXT
  ORDSET OASGP DIFRING SRING OSGROUP PDRING
  MODULE PID OAGROUP OCAMON
  OAMON DIOID INTDOM CACHSET
  RNG ORDFIN OAMONS CABMON COMRING
  GCDDOM UFD ES
  FIELD VECTCAT RADCAT
  ENTIRER ORDRING FLINEXP
  DIFFDOM DIFFSPC DIFFMOD
  LINEXP PATMAB REAL CHARZ LOGIC
  PDDOM PDSPC FPATMAB
  DSEXT ORDSTRCT
  BOOLE SRING TRANFUN
  INS DIVRING EUCDOM
  FPS RNS PATAB
  POLYCAT QFCAT FEVALAB
  ITUPLE IDPT ITFUN2 SEGCAT
  FILECAT SMAGG MKRECORD MKFUNC
  PPCURVE PSCURVE RESLATC
  OUTFORM BINDING
  IARRAY1
  DATAARY PROPLOG BYTEORD
  AGG ELTAGG IXAGG
  BGAGG BRAGG ELAGG
  DLAGG DQAGG QUAGG SKAGG PRQAGG ALAGG
  FLAGG URAGG LNAGG
  A1AGG LSAGG SRAGG
  FSAGG STAGG CLAGG
  RCAGG SETAGG HOAGG
  TBAGG KDAGG DIAGG
  DIOPS FINAGG MDAGG
  MONOP PRIMARR SEXCAT
  PROPERTY ARITY OPERCAT STREAM
  COMBOPC EQ2 NONE1 CONDUIT IOMODE CTORKIND
  PDMOD DMEXT LZSTAGG MSETAGG
  ITFUN3 STREAM1 STREAM2 STREAM3 ANY1
  ALIST RTVALUE SYSPTR ATTREG REF
)


# ---------------------------------------------------------------------------
# Layer 2 -- Syntax, segments, arrays, sets
# ---------------------------------------------------------------------------

set(oa_algebra_layer_2
  SYNTAX INTRET SEGXCAT CONTOUR LIST3 MKUCFUNC FNCAT SCACHE
  IFARRAY FARRAY SET SIG FUNDESC DOMTMPLT MKBCFUNC RNGBIND
  SEG OVERSET CTORCAT CTOR
)


# ---------------------------------------------------------------------------
# Layer 3 -- Scope, mapping packages, operators
# ---------------------------------------------------------------------------

set(oa_algebra_layer_3
  SCOPE MAPHACK1 MAPHACK2 MAPHACK3 MAPPKG1 SEGBIND MAPPKG2 MAPPKG3
  INTBIT MONAD SEG2 BOP BOP1 COMMONOP CATCTOR CTORCALL
)


# ---------------------------------------------------------------------------
# Layer 4 -- Miscellaneous categories and domains
# ---------------------------------------------------------------------------

set(oa_algebra_layer_4
  ANON OSI COMM COMPPROP SEGBIND2 FAMONC IDPC NONE FCTRDATA
  COLOR PALETTE PARPCURV PARPC2 PARSCURV PARSC2 PARSURF PARSU2
  PATRES2 PATTERN1 SPACEC SPLNODE IDPOAM SUCH YSTREAM ENV
  ATRIG LALG RANDSRC
)


# ---------------------------------------------------------------------------
# Layer 5 -- Elementary functions, kernels, monads
# ---------------------------------------------------------------------------

set(oa_algebra_layer_5
  CARD DVARCAT ELEMFUN FCOMP IDPAM IDPO
  INCRMAPS KERNEL2 MODMONOM MONADWU NARNG
  ODVAR PATLRES PMLSAGG ORDMON PERMCAT RFDIST RIDIST SDVAR
  TRIGCAT ELABEXPR KERNEL IDPOAMS
)


# ---------------------------------------------------------------------------
# Layer 6 -- Propositions, automorphisms, ordered structures
# ---------------------------------------------------------------------------

set(oa_algebra_layer_6
  PROPFRML PROPFUN1 AUTOMOR CHARPOL PATMATCH OVAR ES1 ES2
  GRMOD HYPCAT MODRING NASRING
  SORTPAK ZMOD PROPFUN2 SAOS INDE FLAGG2 KTVLOGIC TREE
  BYTE SYSINT SYSNNI DIRPCAT
)


# ---------------------------------------------------------------------------
# Layer 7 -- Trees, direct products, polynomial categories
# ---------------------------------------------------------------------------

set(oa_algebra_layer_7
  BTCAT LMOPS FMONCAT FMCAT DIRPROD IFAMON GRALG
  SMP INTABL HASHTBL INT8 INT16 INT32 INT64 UINT8 UINT16 UINT32
  UINT64 BTREE FAGROUP FGROUP MODOP FMONOID GDMP PARTPERM HDP
  RMATCAT ICDEN
)


# ---------------------------------------------------------------------------
# Layer 8 -- Tables, factored objects, polynomials, linear algebra basics
# ---------------------------------------------------------------------------

set(oa_algebra_layer_8
  BSTREE BTOURN FACTFUNC TABLE ONECOMP FR2 FRUTIL MLO NAALG
  ORDCOMP LO OP UNISEG2 XALG FST RADIX POLY OFMONOID
  ARR2CAT LINDEP STACK PADICCT MOEBIUS PRTITION HDMP
  MPC2 MPC3 DMP GBINTERN VARIABLE SMATCAT IMATLIN
  IMATQF MODMON FINRALG CVMP LIST2 FINAALG
)


# ---------------------------------------------------------------------------
# Layer 9 -- Continued fractions, permutations, Groebner, up-polynomials
# ---------------------------------------------------------------------------

set(oa_algebra_layer_9
  DLP EAB IPADIC FMAGMA QUEUE MATLIN FAMONOID CONTFRAC WP PERM
  PERMGRP DDFACT FSERIES FT IDPAG INFINITY LA OMLO ORTHPOL
  PRODUCT POLTOPOL SQMATRIX GB RATRET RADUTIL UP PINTERPA XFALG
  ZLINDEP BBTREE TABLEAU MATSTOR FRNAALG FRAMALG
  CPIMA
)


# ---------------------------------------------------------------------------
# Layer 10 -- Any, factored, pattern, monogenic, matrix categories
# ---------------------------------------------------------------------------

set(oa_algebra_layer_10
  BPADIC ANY LWORD FR SEXOF CRAPACK DEQUEUE DLIST FLASORT PATRES
  FM FM1 FPC IROOT LIECAT LIST2MAP SEX MODFIELD
  MRING MTHING NCNTFRAC NCODIV ODR OREPCAT OWP PADIC
  PATTERN2 PBWLB PENDTREE PGE PGROEB PINTERP PFR PMDOWN PMINS
  PMTOOLS MONOGEN EMR PSCAT QFORM MTSCAT GHENSEL
  STTAYLOR TABLBUMP UPSCAT UDPO UNISEG VSPACE OREPCTO
  GENEEZ XPOLYC XPR BTAGG RMATRIX PTCAT XDPOLY
  XRPOLY CINTSLPE MATRIX MATCAT IARRAY2 FFIELDC
)


# ---------------------------------------------------------------------------
# Layer 11 -- Arrays, combinatorics, complex categories, points
# ---------------------------------------------------------------------------

set(oa_algebra_layer_11
  APPLYORE ARRAY1 ARRAY12 ARRAY2 ONECOMP2 ASTACK COMBINAT POINT
  UDVO CSTTOOLS MRF2 ITAYLOR ORDCOMP2 FLALG GALUTIL HEAP COMPLPAT
  CPMATCH INTCAT INTHEORY COMPCAT IRREDFFX LFCAT
  LODOCAT ORESUP OREUP QEQUAT PR PREASSOC PRIMARR2
  REDORDER SYMPOLY TUPLE XEXPPKG HB IBITS XF XPOLY INFORM
  INFORM1
)


# ---------------------------------------------------------------------------
# Layer 12 -- Complex, function spaces, algebraic manipulation
# ---------------------------------------------------------------------------

set(oa_algebra_layer_12
  ULSCAT TUBE BITS DIRPROD2 UPXSCAT SETMN POLYROOT STTF LPOLY
  LSMP LSMP1 MATCAT2 TRIMAT POLYCATQ STTFNC SYSTEM HOSTNAME
  PORTNUM UPOLYC2 PFBRU SGCF OUT PSEUDLIN BYTEBUF COMPLEX FS
  ALGMANIP AF EF FSPECF COMBF LF RATFACT SUPFRACF COMMUPC IAN AN
  IALGFACT SAE SAEFACT ALGFACT RFFACT SAERFFC
)


# ---------------------------------------------------------------------------
# Layer 13 -- Univariate polynomials, multivariate factoring, expression
# ---------------------------------------------------------------------------

set(oa_algebra_layer_13
  ASSOCEQ CARTEN UTSCAT PNTHEORY NTPOLFN UPOLYC
  MRATFAC NPCOEF MLIFT PDECOMP COORDSYS DBASE DHMATRIX DIOSP
  FACUTIL EXPR ACF FAXF LEADCDET COMPFACT FNLA GRAY
  IRSN INNMFACT ACFS MHROWRED NUMODE NUMQUAD GENUFACT
  MULTFACT ODESYS ODETOOLS ORDFUNS PERMAN UPXSCCA
  ULSCCAT PTPACK REP2 MSET SYMFUNC VECTOR2 VECTOR CHAR
  XPBWPOLY INBCON OUTBCON LEXP
)


# ---------------------------------------------------------------------------
# Layer 14 -- Largest layer: fractions, integration, power series, ...
# ---------------------------------------------------------------------------
# This is the largest layer and contains the bulk of the classical
# algebra: fractions, polynomial utilities, integration, algebraic
# function fields, power series, Groebner bases, etc.

set(oa_algebra_layer_14
  PLOT3D CLIF GALFACTU CDEN UPCDEN ICARD BALFACT BEZOUT BINARY
  BRILL CHVAR OPQUERY CYCLOTOM FFP IPF PF CYCLES DECIMAL DISPLAY
  FFPOLY FFX NORMRETR TWOFACT MFINFACT DPMO DPOLCAT
  EQ ERROR LGROBP PUSHVAR MPCPF GENMFACT EVALCYC FF FFF FFCGP
  FFCG GROEBSOL FFCGX FFPOLY2 FFHOM INBFF FFNBP FFNB FFNBX
  FFSLPE FGLMICPK REAL0 PRS SUBRESP FNAME FILE POLY2 CMPLXRT
  INFSP FLOATRP FRAC GENPGCD GALPOLYU GBEUCLID GBF GMODPOL
  GOSPER HEXADEC MDDFACT INMODGCD HEUGCD IBPTOOLS IFF IDEAL
  IDECOMP INPSIGN MONOTOOL INTHERTR IR LAUPOL INTTR RDETR
  INTRAT IR2 INTRF INTSLPE TANEXP INTTOOLS EFSTRUC FS2UPS EFUPXS
  BOUNDZRO ODEPRIM DSMP LODOOPS LODO LODO1 LODO2 UTSODE UTSODETL
  ODERAT INTG0 FFCAT IBATOOL FFINTBAS RADFF FDIVCAT
  FRIDEAL HELLFDIV FRMOD FDIV DBLRESP PFOTOOLS FSRED
  MMAP ALGFF FORDER FFCAT2 FRIDEAL2 FDIV2 RDIV PFO INTHERAL
  FSUPFACT INTALG INTAF ODERED ODEPAL INTPAF PRIMELT FSPRMELT
  RDEEF SMITH RDETRS RDEEFS PMASSFS FS2 ITRIGMNP TRIGMNIP
  PMPREDFS INTPM INTEF IR2F RULE APPRULE TRMANIP FSCINT FSINT
  UTS EFULS ULSCONS ULS EXPUPXS UPXSSING EXPEXPAN FS2EXPXP
  LIMITPS ISUMP SIGNRF SIGNEF TOOLSIGN LIMITRF LPEFRAC LSPP
  MCDEN MPOLY MPRFF MULTSQFR NSUP ODP ODEPRRIC PADICRC PADICRAT
  PCOMP PFBR PFRPAC PGCD PLEQN PMPLCAT PMQFCAT POLUTIL POLYLIFT
  POLY2UP PSQFR QALGSET QFCAT2 RCFIELD REAL0Q REALSOLV
  RESRING SYSSOLP RETSOL RF RFFACTOR RRCC SCPKG SHDP SHP
  SMTS SOLVEFOR SPLTREE STINPROD SUMRF UPMP SUP TEX TEXTFILE
  UNIFACT UPDIVP UPDECOMP UPSQFREE VIEWDEF TS WEIER EQTBL GSTBL
  STBL STRTBL DOMCTOR DOMAIN SYMTAB SYMS IOBCON
)


# ---------------------------------------------------------------------------
# Layer 15 -- Power series, plotting, 2D graphics
# ---------------------------------------------------------------------------

set(oa_algebra_layer_15
  UPXSCONS UPXS TEX1 SUP2 ODPOL NSUP2 UP2 SUBSPACE SPACE3
  DROPT DROPT1 DROPT0 GRDEF CLIP VIEW2D VIEW PLOTTOOL GRIMAGE
  PLOT RMCAT2 ROIRC SDPOL FRAC2 TUBETOOL JVMBCODE MESH
)


# ---------------------------------------------------------------------------
# Layer 16 -- Integer factorization, 3D graphics, p-adic
# ---------------------------------------------------------------------------

set(oa_algebra_layer_16
  DPMM UTS2 ULS2 CARTEN2 COMPLEX2 LMDICT INTFACT DEGRED PMPRED
  UPXS2 NUMTUBE NFINTBAS PMASS PADE PADEPAC MKFLCFN PLOT1
  PTFUNC2 VIEW3D DRAWCX DRAWPT
)


# ---------------------------------------------------------------------------
# Layer 17 -- Galois factoring, character classes, MathML
# ---------------------------------------------------------------------------

set(oa_algebra_layer_17
  CCLASS FSAGG2 GALFACT TOPSP BPADICRT IBACHIN MMLFORM NORMMA
  OMSAGG OPSIG PRIMES WFFINTBS PWFFINTB RDIST RPOLCAT
)


# ---------------------------------------------------------------------------
# Layer 18 -- Keyed access files, polynomial set categories
# ---------------------------------------------------------------------------

set(oa_algebra_layer_18
  KAFILE IPRNTPK TBCMPPK PSETCAT
)


# ---------------------------------------------------------------------------
# Layer 19 -- Float, functions, patterns, parsers
# ---------------------------------------------------------------------------

set(oa_algebra_layer_19
  ACPLOT ANTISYM DRAWCFUN DRAW EP FLOAT FPARFRAC FUNCTION HACKPI
  ISUPS LIB INEP NREP NUMFMT OC PATTERN PMKERNEL PMSYM
  QALGSET2 RECLOS REP1 QUATCAT ROMAN RULECOLD SPECOUT
  SPADPRSR PARSER TSETCAT
)


# ---------------------------------------------------------------------------
# Layer 20 -- Numeric, quaternions, solve, special functions
# ---------------------------------------------------------------------------

set(oa_algebra_layer_20
  ALGMFACT ALGPKG ALGSC CRFP CTRIGMNP DERHAM DFSFUN DRAWCURV
  ELFUTS EXPRODE EXPRTUBE EXPR2 FLOATCP FRNAAF2 GAUSSFAC GCNAALG
  GENUPS GTSET GPOLSET INFPROD0 INPRODFF INPRODPF JORDAN NLINSOL
  ODERTRIC KOVACIC LIE LODOF LSQM NCEP NSMP NUMERIC QUAT OCT
  OCTCT2 PAN2EXPR PFOQ PICOERCE PMFS PSETPK QUATCT2 RSETCAT
  RULESET SIMPAN DRAWHACK SOLVESER SUMFS SUTS WUTSET
)


# ---------------------------------------------------------------------------
# Layer 21 -- Definite integration, Laplace, ODE, generalized series
# ---------------------------------------------------------------------------

set(oa_algebra_layer_21
  DFINTTLS DEFINTEF IRRF2F DEFINTRF GSERIES EXPR2UPS INVLAPLA
  LAPLACE ODEINT ODECONST LODEEF ODEEF NODE1 SOLVERAD REP SULS
  SUPXS
)


# ---------------------------------------------------------------------------
# Layer 22 -- Regular sets, triangular decomposition
# ---------------------------------------------------------------------------

set(oa_algebra_layer_22
  NTSCAT QCMPACK RSETGCD RSDCMPK REGSET RGCHAIN SFRTCAT SNTSCAT
  SOLVETRA SFQCMPK SFRGCD SRDCMPK SREGSET NORMPK LEXTRIPK IRURPK
  ZDSOLVE
)


# ---------------------------------------------------------------------------
# Layer 23 -- Rational univariate, interval arithmetic
# ---------------------------------------------------------------------------

set(oa_algebra_layer_23
  LAZM3PK RURPK INTRVL CATEGORY
)


# ---------------------------------------------------------------------------
# Layer 24 -- AST nodes, JVM targets, compiler frontend
# ---------------------------------------------------------------------------
# The final layer contains the OpenAxiom compiler's AST representation,
# JVM bytecode generation targets, and the compiler frontend itself.

set(oa_algebra_layer_24
  RINTERP ASTCAT SASTCAT PARAMAST HEADAST LITERAL TYPEAST
  IMPTAST MAPPAST ATTRAST JOINAST IFAST RPTAST WHILEAST INAST
  CLLCTAST LSTAST EXITAST RETAST SEGAST PRTDAST CRCEAST LETAST
  RDUCEAST COLONAST ADDAST CAPSLAST SUCHTAST CASEAST HASAST ISAST
  CATAST WHEREAST COMMAAST QQUTAST DEFAST MACROAST SPADXPT SPADAST
  SIGAST INBFILE OUTBFILE IOBFILE RGBCMDL RGBCSPC STEPAST IP4ADDR
  NETCLT INETCLTS ITFORM RSTRCAST SEQAST IRFORM ELABOR COMPILER
  MSYSCMD PRINT TALGOP YDIAGRAM LINFORM LINBASIS DBASIS LINELT
  JVMOP JVMCFACC JVMFDACC JVMMDACC JVMCSTTG
)


# ===========================================================================
# Known default-package producers
# ===========================================================================
#
# The constructors listed below are known to produce a companion
# "default package" (FOO-) when compiled.  This list is transcribed
# from src/algebra/Makefile.am, which lists FOO- alongside FOO in
# its layer variables.
#
# The build system uses this list for verification: after compiling
# a constructor in this list, it asserts that FOO-.NRLIB/code.<FASLEXT>
# was actually produced.  For constructors NOT in this list, the
# default-package copy is best-effort (CopyIfExists).
#
# When adding a new category that defines default implementations,
# add its abbreviation here so the build verifies the output.
# ===========================================================================

set(OA_ALGEBRA_DEFAULT_PACKAGES
  A1AGG ABELGRP ABELMON ABELSG ACF ACFS AGG ALGEBRA AMR ARR2CAT
  ASTCAT ATRIG BASTYPE BGAGG BOOLE BRAGG BTAGG BTCAT CLAGG COMPCAT
  CTORCAT DIAGG DIFFDOM DIFFSPC DIOPS DIRPCAT DIVRING DPOLCAT DSEXT
  DVARCAT ELAGG ELEMFUN ELTAGG ENTIRER ES EUCDOM EVALAB FAMR FAXF
  FDIVCAT FEVALAB FFCAT FFIELDC FIELD FINAALG FINAGG FINITE FINRALG
  FLAGG FLINEXP FPC FPS FRAMALG FRETRCT FRNAALG FS FSAGG GCDDOM GRALG
  GRMOD GROUP HOAGG HYPCAT IEVALAB INBCON INS INTDOM IXAGG KDAGG LALG
  LIECAT LNAGG LODOCAT LOGIC LSAGG LZSTAGG MATCAT MODULE MONAD MONADWU
  MONOGEN MONOID NAALG NARNG NASRING OAGROUP OAMON OC OPERCAT ORDTYPE
  OREPCAT OUTBCON PDDOM PDSPC PFECAT POLYCAT PSCAT PSETCAT QFCAT QUATCAT
  RADCAT RCAGG RCFIELD RETRACT RING RMATCAT RNG RNS RPOLCAT RRCC RSETCAT
  SETAGG SETCAT SGROUP SMATCAT SRAGG STAGG TBAGG TRANFUN TRIGCAT TSETCAT
  UFD ULSCCAT UPOLYC UPSCAT UPXSCCA URAGG UTSCAT VECTCAT VSPACE XF
)


# -- Total layer count and indices: derived from the data -------------------
# These are computed AFTER the catch-all layer logic below, so they must
# appear at the end of this file.  See the bottom of this file.


# ===========================================================================
# Catch-all layer and dynamic layer count
# ===========================================================================
#
# STRATEGY
# --------
# The explicit layers above (0-24) encode a battle-tested compilation
# ordering that has been stable for years.  But as the algebra evolves,
# new constructors will be added to existing pamphlets or entirely new
# .spad/.spad.pamphlet files.  Requiring a developer to immediately
# figure out which layer a new constructor belongs to is friction that
# slows down development.
#
# CATCH-ALL LAYER
# ---------------
# After all explicit layers are processed, we compute the set difference:
#
#   catch-all = OA_ALGEBRA_CONSTRUCTORS \ (layer_0 U layer_1 U ... U layer_24)
#
# Any constructor not assigned to an explicit layer lands in the catch-all
# layer.  Because the catch-all depends on the last explicit layer, every
# explicit constructor is guaranteed to be compiled before the catch-all
# starts.  This is always correct (possibly slow, but correct).
#
# A developer who wants better parallel build performance can later move
# a constructor from the catch-all into an earlier explicit layer once
# its actual dependencies are understood.
#
# DYNAMIC LAYER COUNT
# -------------------
# OA_ALGEBRA_NUM_LAYERS and OA_ALGEBRA_LAYER_INDICES are derived from
# the data, not hardcoded.  If the catch-all is empty (all constructors
# are in explicit layers), the count stays at 25.  If there are unassigned
# constructors, the count becomes 26 (layers 0-25).
#
# SPADFILES LIST
# --------------
# The old hardcoded oa_spadfiles list is gone.  The scanner module
# (OpenAxiomScanAlgebra.cmake) now populates OA_ALGEBRA_PAMPHLET_BASES
# and OA_ALGEBRA_PLAIN_SPADFILES directly from the SOURCES manifest.
# The combined list is available as OA_ALGEBRA_ALL_SPADFILES.  Adding a
# new pamphlet or .spad file to src/algebra/ requires adding its basename
# to src/algebra/SOURCES; the scanner warns about unlisted files.
# ===========================================================================


# The number of explicit (hand-curated) layers.
set(OA_ALGEBRA_NUM_EXPLICIT_LAYERS 25)

# ---------------------------------------------------------------------------
# oa_algebra_build_catchall()
#
# Must be called AFTER include(OpenAxiomScanAlgebra) + oa_scan_algebra_sources()
# has populated OA_ALGEBRA_CONSTRUCTORS.  Computes the catch-all layer.
#
# This is a function (not executed at include time) because the scanner
# results are not yet available when this file is first included.
# ---------------------------------------------------------------------------

function(oa_algebra_build_catchall)
  # Collect every constructor assigned to an explicit layer.
  set(_assigned "")
  math(EXPR _last_explicit "${OA_ALGEBRA_NUM_EXPLICIT_LAYERS} - 1")
  foreach(_i RANGE 0 ${_last_explicit})
    list(APPEND _assigned ${oa_algebra_layer_${_i}})
  endforeach()
  list(REMOVE_DUPLICATES _assigned)

  # Set difference: all constructors minus assigned ones.
  set(_catchall ${OA_ALGEBRA_CONSTRUCTORS})
  foreach(_a ${_assigned})
    list(REMOVE_ITEM _catchall "${_a}")
  endforeach()

  # If there are unassigned constructors, create the catch-all layer.
  list(LENGTH _catchall _ncatch)
  if(_ncatch GREATER 0)
    list(SORT _catchall)
    set(oa_algebra_layer_${OA_ALGEBRA_NUM_EXPLICIT_LAYERS}
      ${_catchall} PARENT_SCOPE)

    math(EXPR _total "${OA_ALGEBRA_NUM_EXPLICIT_LAYERS} + 1")
    message(STATUS
      "Algebra catch-all layer ${OA_ALGEBRA_NUM_EXPLICIT_LAYERS}: "
      "${_ncatch} constructors not in explicit layers")
  else()
    set(_total ${OA_ALGEBRA_NUM_EXPLICIT_LAYERS})
    message(STATUS "Algebra: all constructors assigned to explicit layers")
  endif()

  set(OA_ALGEBRA_NUM_LAYERS ${_total} PARENT_SCOPE)

  # Build the convenience index list (0..N-1).
  set(_indices "")
  math(EXPR _last "${_total} - 1")
  foreach(_i RANGE 0 ${_last})
    list(APPEND _indices ${_i})
  endforeach()
  set(OA_ALGEBRA_LAYER_INDICES ${_indices} PARENT_SCOPE)
endfunction()
