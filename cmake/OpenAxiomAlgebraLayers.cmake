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
# Layer 0 -- Constructors compilable with L0..L-1 FASLs staged
# ---------------------------------------------------------------------------
# The bedrock of the algebra: Type itself, booleans, integers, strings.
# These have minimal or no algebra dependencies (they depend only on
# strap-2 artifacts from the bootstrap stages).

set(oa_algebra_layer_0
  A1AGG ABELGRP ABELMON ABELSG ACF AGG AHYP ALAGG
  ALGEBRA ALIST AMR ANY ARITY ARR2CAT ARRAY1 ARRAY2
  ATRIG ATTREG AUTOMOR BALFACT BASTYPE BEZOUT BGAGG BINOPC
  BITS BMODULE BOOLE BOOLEAN BOP1 BRAGG BTAGG BYTE
  BYTEORD CABMON CACHSET CARD CCLASS CDEN CFCAT CHAR
  CHARNZ CHARPOL CHARZ CLAGG COLOR COMBOPC COMMONOP COMMUPC
  COMPLEX2 COMPPROP COMRING CONDUIT CTORKIND CVMP CYCLOTOM DATAARY
  DEGRED DFLOAT DIAGG DIFEXT DIFFDOM DIFFMOD DIFFSPC DIFRING
  DIOID DIOPS DIOSP DISPLAY DIVRING DLAGG DLIST DLP
  DSEXT DVARCAT EAB ELAGG ELEMFUN ELTAB ELTAGG ENTIRER
  EQ2 EQTBL ES ES1 ES2 ESTOOLS1 ESTOOLS2 EUCDOM
  EVALAB EXIT FACTFUNC FACUTIL FAMONC FAMR FCOMP FEVALAB
  FIELD FILECAT FINAGG FINITE FINRALG FLAGG FLAGG2 FLASORT
  FLINEXP FLOATRP FMCAT FMONCAT FNCAT FPATMAB FPC FPS
  FR2 FRETRCT FRUTIL FSAGG FST FUNCTION FUNCTOR GBEUCLID
  GBINTERN GCDDOM GENPGCD GRAY GRDEF GRMOD GROUP GSTBL
  HACKPI HASHTBL HB HOAGG HOMOTOP HOSTNAME HYPCAT IARRAY1
  IARRAY2 IBITS IBPTOOLS IDENT IDPT IEVALAB IFARRAY IMATLIN
  INBFF INCRMAPS INFORM INPSIGN INS INT INTABL INTBIT
  INTCAT INTDOM INTRET INTRVL INTSLPE IOMODE IPRNTPK IROOT
  IXAGG KDAGG KERNEL2 KOERCE KONVERT KRCFROM KTVLOGIC KVTFROM
  LALG LEADCDET LIECAT LINDEP LINEXP LINSET LIST LIST2MAP
  LIST3 LLINSET LMODULE LMOPS LNAGG LO LOGIC LPEFRAC
  LSAGG LSMP LSMP1 LSPP LZSTAGG MAPHACK1 MAPHACK2 MAPHACK3
  MATCAT MATCAT2 MATSTOR MAYBE MDAGG MKFUNC MKRECORD MLO
  MMAP MMLFORM MODEPVAR MODMONOM MODRING MODULE MONAD MONOID
  MONOTOOL MPC2 MPC3 MSYSCMD MTHING MTSCAT NNI NONE
  NONE1 NPCOEF NTPOLFN NUMFMT OAGROUP OAMON OAMONS OASGP
  OC OCAMON ODR OINTDOM ONECOMP OPERCAT ORDCOMP ORDFIN
  ORDFUNS ORDRING ORDSET ORDSTRCT ORDTYPE OREPCAT ORTHPOL OSGROUP
  OSI OUTFORM OVAR PADICCT PAIR PARPCURV PARSCURV PARSURF
  PATAB PATLRES PATMAB PATRES2 PATTERN2 PCOMP PDDOM PDECOMP
  PDMOD PDRING PDSPC PERMCAT PFECAT PFR PI PID
  PINTERPA PMKERNEL PMSYM POLUTIL POLY2 POLYCAT POLYCATQ POLYLIFT
  PORTNUM PPCURVE PR PRIMARR PRIMCAT PRINT PRODUCT PROPERTY
  PROPLOG PRQAGG PRS PSCAT PSCURVE PSQFR PTCAT PTRANFN
  PUSHVAR QEQUAT QFCAT QFCAT2 QUAGG QUATCAT RADCAT RANDSRC
  RATFACT RATRET RCAGG RCFIELD REAL REAL0 REF REPDB
  REPSQ RETRACT RFFACT RGBCMDL RING RLINSET RMODULE RNG
  RNGBIND RNS ROMAN RRCC RTVALUE RULECOLD SAOS SCACHE
  SCPKG SEG SEGCAT SEGXCAT SETAGG SETCAT SEX SEXCAT
  SEXOF SGROUP SINT SKAGG SMAGG SMITH SOLVEFOR SORTPAK
  SPACEC SPECOUT SPFCAT SPLNODE SRAGG SRING STAGG STEP
  STRING STRTBL SUCH SUP2 SUPFRACF SYMBOL SYMFUNC SYNTAX
  SYSINT SYSNNI SYSPTR TABLE TABLEAU TBAGG TEX TRANFUN
  TREE TRIGCAT TRIMAT TUPLE TYPE UDPO UDVO UFD
  ULSCAT ULSCCAT UPDIVP UPMP UPOLYC2 UPSQFREE UPXSCAT UPXSCCA
  URAGG UTYPE VARIABLE VECTCAT VECTOR VOID VSPACE WP
  XALG ZMOD
)


# ---------------------------------------------------------------------------
# Layer 1 -- Constructors compilable with L0..L0 FASLs staged
# ---------------------------------------------------------------------------
# The largest layer.  Contains the mathematical category hierarchy
# (SETCAT, RING, FIELD, ...), aggregate categories (HOAGG, FLAGG,
# VECTCAT, ...), and fundamental domains (OUTFORM, STREAM, ...).
#
# Default packages (FOO-) are NOT listed here; they are produced
# automatically when the parent category (FOO) is compiled.

set(oa_algebra_layer_1
  ANON ANTISYM ANY1 APPLYORE ARRAY12 ASTACK ASTCAT BINDING
  BINOP BOUNDZRO BTCAT BYTEBUF COMBINAT COMM COMOPC CSTTOOLS
  DBASE DIRPCAT DMEXT DOMTMPLT DPOLCAT DQAGG EFUPXS ELABEXPR
  EMR EQ ERROR FARRAY FGROUP FLOAT FM FM1
  FMONOID FNAME FRAC2 FRAMALG FSERIES FT GB GBF
  GRALG HEAP ICDEN IDEMOPC IDPAG IDPAM IDPC IDPO
  IDPOAM IFAMON INFINITY INT16 INT32 INT64 INT8 INTHEORY
  INTHERTR IP4ADDR ITFORM JVMBCODE JVMCSTTG KERNEL LA LAUPOL
  LFCAT LINBASIS LINFORM LIST2 LMDICT LODOCAT MAPPKG1 MAPPKG2
  MAPPKG3 MKBCFUNC MKUCFUNC MLIFT MODFIELD MODMON MOEBIUS MONADWU
  MPCPF MSETAGG NARNG NCODIV NSUP OCTCT2 ODVAR OMLO
  ONECOMP2 ORDCOMP2 ORDMON OREPCTO OWP PALETTE PARPC2 PARSC2
  PARSER PARSU2 PENDTREE PFBR PFBRU PMINS PMLSAGG POINT
  POLYROOT PRIMARR2 PROPFRML PSEUDLIN QUAT QUATCT2 RDETR RDETRS
  REAL0Q RESLATC RF RFDIST RGBCSPC RIDIST ROIRC RPOLCAT
  SDVAR SEG2 SEGBIND SETMN SGPOPC SIG SIGNRF SMP
  SPADPRSR SPLTREE STACK STBL STREAM SUBRESP SYSPRED SYSTEM
  TALGOP TANEXP TEX1 UINT16 UINT32 UINT64 UINT8 UP
  UPCDEN UPDECOMP UPSCAT VECTOR2 XF ZLINDEP
)


# ---------------------------------------------------------------------------
# Layer 2 -- Constructors compilable with L0..L1 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_2
  ADDAST ATTRAST BTREE CALLAST CAPSLAST CASEAST CATAST CHVAR
  CINTSLPE CLLCTAST CMPCTXT COLONAST COMMAAST COMOP COMPLEX COMPLPAT
  CONTFRAC CONTOUR COORDSYS CPIMA CRCEAST CRFP DBASIS DDFACT
  DFSFUN DHMATRIX DIRPROD DSMP EXITAST EXPUPXS FAGROUP FAMONOID
  FAXF FILE FRAC FRIDEAL FSAGG2 FUNDESC GALUTIL GAUSSFAC
  GDMP GHENSEL GMODPOL HASAST IALGFACT IDPOAMS IFAST IMATQF
  IMPTAST INAST INBCON INEP INFORM1 INMODGCD INNMFACT INTFACT
  IPADIC IR IRFORM ISAST ISUMP ITUPLE JOINAST JVMCFACC
  JVMFDACC JVMMDACC JVMOP KAFILE LETAST LIMITRF LINELT LITERAL
  LSTAST MAPPAST MCDEN MDDFACT MKFLCFN MONOPC MPOLY MRATFAC
  MSET MULTSQFR NAALG NASRING NORMMA NSUP2 NUMERIC OCT
  ODEPRIM ODERED ODESYS ODETOOLS OFMONOID OMSAGG OPSIG ORESUP
  OUT OUTBCON PARAMAST PATMATCH PATTERN1 PFOTOOLS PFRPAC PGCD
  PINTERP PMDOWN PNTHEORY POLY POLY2UP PREASSOC PROPFUN2 PRTDAST
  PTFUNC2 PTPACK QQUTAST QUEUE RADIX RDUCEAST REALSOLV RECLOS
  REDORDER RESRING RETAST RMATCAT RPTAST RSTRCAST SASTCAT SEGAST
  SEGBIND2 SEQAST SET SGPOP SHP SIGAST SPADAST SPADXPT
  STEPAST STREAM1 STREAM2 STREAM3 SUBSPACE SUCHTAST SYMTAB TABLBUMP
  TUBE TYPEAST UNISEG UNISEG2 UP2 UPOLYC UTS2 UTSODETL
  VIEWDEF WHEREAST WHILEAST XPR YSTREAM
)


# ---------------------------------------------------------------------------
# Layer 3 -- Constructors compilable with L0..L2 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_3
  ACPLOT ASSOCEQ BBTREE BINARY BOP BPADIC BSTREE BTOURN
  CLIP CPMATCH DECIMAL DEQUEUE DIRPROD2 DMP DPMO DRAWHACK
  FFIELDC FFPOLY FFSLPE FINAALG FMAGMA FNLA FR FRIDEAL2
  FS GALFACTU GALPOLYU GENEEZ GOSPER HDP HEADAST HEUGCD
  HEXADEC ICARD IDEAL INBFILE INDE INTTR IOBCON IPF
  IR2 IRREDFFX ISUPS ITFUN2 ITFUN3 LIB LODOOPS MATLIN
  MHROWRED MODOP MONOGEN MONOP MRING NCNTFRAC NORMRETR NSMP
  NUMODE NUMQUAD ODEPRRIC ODP ODPOL OREUP OUTBFILE OVERSET
  PADE PADIC PADICRC PARTPERM PATRES PATTERN PLOT PLOT1
  PLOT3D PMQFCAT PMTOOLS PRIMES PROPFUN1 PSETCAT QALGSET RADUTIL
  RDIST REP2 RINTERP RMATRIX RMCAT2 SAE SAEFACT SAERFFC
  SCOPE SDPOL SHDP SMATCAT SPACE3 STTAYLOR TBCMPPK TEXTFILE
  TUBETOOL UNIFACT XFALG
)


# ---------------------------------------------------------------------------
# Layer 4 -- Constructors compilable with L0..L3 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_4
  ACFS AF ALGFACT ALGMANIP BPADICRT BRILL COMBF COMPCAT
  COMPFACT CRAPACK CTORCAT DEFAST DERHAM DPMM DROPT EF
  ELFUTS ENV EXPRTUBE FFP FFPOLY2 FPARFRAC FRMOD FRNAALG
  FS2 FSPECF FSRED GPOLSET HDMP IBACHIN IBATOOL IDECOMP
  INTRAT INTTOOLS INVLAPLA IOBFILE ITAYLOR LF LODO LWORD
  MACROAST MATRIX MRF2 NETCLT NUMTUBE OP OPQUERY PADEPAC
  PADICRAT PAN2EXPR PF PICOERCE PMASSFS PMFS PMPLCAT PMPREDFS
  PRTITION SGCF SQMATRIX STTF SUMRF SUTS TOOLSIGN TOPSP
  TSETCAT UPXSSING UTSODE XPOLYC
)


# ---------------------------------------------------------------------------
# Layer 5 -- Constructors compilable with L0..L4 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_5
  ALGMFACT ALGPKG ALGSC CARTEN CMPLXRT CTOR CTORCALL DROPT1
  EFSTRUC EFULS ELABOR EXPEXPAN EXPR EXPR2 EXPRODE FF
  FFF FFHOM FFINTBAS FFX FRNAAF2 FSUPFACT GALFACT GENUFACT
  GTSET IAN INETCLTS INPRODFF INTRF ITRIGMNP JORDAN LGROBP
  LIE LODO1 LODO2 NCEP NFINTBAS PBWLB PERM PERMAN
  PLEQN PMASS PMPRED POLTOPOL QALGSET2 QFORM RULE SMTS
  STINPROD STTFNC SYMPOLY WFFINTBS XDPOLY XEXPPKG YDIAGRAM
)


# ---------------------------------------------------------------------------
# Layer 6 -- Constructors compilable with L0..L5 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_6
  AN APPRULE CARTEN2 CATCTOR CLIF COMPILER CTRIGMNP CYCLES
  DROPT0 EVALCYC FFCGP FFNBP FGLMICPK FLOATCP FS2UPS GCNAALG
  GROEBSOL IFF INFPROD0 INPRODPF IR2F IRSN LSQM MULTFACT
  NREP ODERAT PERMGRP PGROEB PSETPK PWFFINTB REP1 RULESET
  SULS SUMFS TRIGMNIP TRMANIP TS TWOFACT UTSCAT XRPOLY
)


# ---------------------------------------------------------------------------
# Layer 7 -- Constructors compilable with L0..L6 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_7
  CATEGORY FCTRDATA FFCAT FFCG FFCGX FFNB FFNBX FLALG
  INFSP INTG0 INTPM IRRF2F MESH MFINFACT PGE PRIMELT
  RSETCAT SIMPAN SOLVESER SUP SYSSOLP VIEW2D WEIER WUTSET
  XPOLY
)


# ---------------------------------------------------------------------------
# Layer 8 -- Constructors compilable with L0..L7 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_8
  ALGFF DBLRESP DOMCTOR EP FDIVCAT FFCAT2 FSPRMELT GENMFACT
  INTHERAL LPOLY MPRFF NTSCAT ODEPAL QCMPACK RADFF RETSOL
  RFFACTOR SFRTCAT SOLVERAD VIEW VIEW3D
)


# ---------------------------------------------------------------------------
# Layer 9 -- Constructors compilable with L0..L8 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_9
  DOMAIN DRAWCFUN DRAWCURV DRAWCX DRAWPT HELLFDIV NLINSOL PLOTTOOL
  REP RSETGCD SFQCMPK SNTSCAT SOLVETRA XPBWPOLY
)


# ---------------------------------------------------------------------------
# Layer 10 -- Constructors compilable with L0..L9 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_10
  DRAW FDIV GRIMAGE LEXP ODERTRIC RSDCMPK SFRGCD SYMS
)


# ---------------------------------------------------------------------------
# Layer 11 -- Constructors compilable with L0..L10 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_11
  FDIV2 FORDER KOVACIC LODOF NORMPK REGSET SRDCMPK
)


# ---------------------------------------------------------------------------
# Layer 12 -- Constructors compilable with L0..L11 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_12
  IRURPK LAZM3PK RDIV RGCHAIN SREGSET
)


# ---------------------------------------------------------------------------
# Layer 13 -- Constructors compilable with L0..L12 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_13
  LEXTRIPK PFO PFOQ RURPK
)


# ---------------------------------------------------------------------------
# Layer 14 -- Constructors compilable with L0..L13 FASLs staged
# ---------------------------------------------------------------------------
# This is the largest layer and contains the bulk of the classical
# algebra: fractions, polynomial utilities, integration, algebraic
# function fields, power series, Groebner bases, etc.

set(oa_algebra_layer_14
  INTALG ZDSOLVE
)


# ---------------------------------------------------------------------------
# Layer 15 -- Constructors compilable with L0..L14 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_15
  INTAF INTPAF
)


# ---------------------------------------------------------------------------
# Layer 16 -- Constructors compilable with L0..L15 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_16
  RDEEF
)


# ---------------------------------------------------------------------------
# Layer 17 -- Constructors compilable with L0..L16 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_17
  RDEEFS
)


# ---------------------------------------------------------------------------
# Layer 18 -- Constructors compilable with L0..L17 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_18
  INTEF
)


# ---------------------------------------------------------------------------
# Layer 19 -- Constructors compilable with L0..L18 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_19
  FSCINT
)


# ---------------------------------------------------------------------------
# Layer 20 -- Constructors compilable with L0..L19 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_20
  FSINT
)


# ---------------------------------------------------------------------------
# Layer 21 -- Constructors compilable with L0..L20 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_21
  GSERIES ODEINT ULSCONS UPXSCONS UTS
)


# ---------------------------------------------------------------------------
# Layer 22 -- Constructors compilable with L0..L21 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_22
  ODECONST SUPXS ULS
)


# ---------------------------------------------------------------------------
# Layer 23 -- Constructors compilable with L0..L22 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_23
  EXPR2UPS FS2EXPXP GENUPS LODEEF ULS2 UPXS
)


# ---------------------------------------------------------------------------
# Layer 24 -- Constructors compilable with L0..L23 FASLs staged
# ---------------------------------------------------------------------------
# The final layer contains the OpenAxiom compiler's AST representation,
# JVM bytecode generation targets, and the compiler frontend itself.

set(oa_algebra_layer_24
  LIMITPS ODEEF UPXS2
)




# ---------------------------------------------------------------------------
# Layer 25 -- Constructors compilable with L0..L24 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_25
  DFINTTLS LAPLACE NODE1 SIGNEF
)



# ---------------------------------------------------------------------------
# Layer 26 -- Constructors compilable with L0..L25 FASLs staged
# ---------------------------------------------------------------------------

set(oa_algebra_layer_26
  DEFINTEF DEFINTRF
)

# ===========================================================================
# Excluded constructors
# ===========================================================================
#
# The following constructors cannot be compiled and are excluded from
# the algebra build.  They are not assigned to any layer.
#
#   ESTOOLS  -- References MachineFloat, a domain that was never
#               implemented.  The function df2mf attempts to coerce
#               DoubleFloat to MachineFloat, which does not exist in
#               the algebra sources, SOURCES manifest, or system database.
#
#   NAGS     -- NAG numerical library bindings.  Compilation fails because
#               the invokeNagman foreign-function interface requires the
#               external NAG runtime libraries, which are not available.
#
set(OA_ALGEBRA_EXCLUDED
  ESTOOLS
  NAGS
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
set(OA_ALGEBRA_NUM_EXPLICIT_LAYERS 27)

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

  # Set difference: all constructors minus assigned ones and excluded ones.
  set(_catchall ${OA_ALGEBRA_CONSTRUCTORS})
  foreach(_a ${_assigned})
    list(REMOVE_ITEM _catchall "${_a}")
  endforeach()
  foreach(_x ${OA_ALGEBRA_EXCLUDED})
    list(REMOVE_ITEM _catchall "${_x}")
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
