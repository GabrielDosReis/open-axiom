AC_DEFUN([OPENAXIOM_MAKEFILE],
[AC_CONFIG_FILES([$1:config/var-def.mk:$1.in:config/setup-dep.mk])])

dnl --------------------------------------
dnl -- OPENAXIOM_STANDARD_INTEGER_TYPES --
dnl --------------------------------------
dnl Check for availability of standard sized integer types.
AC_DEFUN([OPENAXIOM_STANDARD_INTEGER_TYPES], [
AC_TYPE_INT8_T
AC_TYPE_UINT8_T
AC_TYPE_INT16_T
AC_TYPE_UINT16_T
AC_TYPE_INT32_T
AC_TYPE_UINT32_T
AC_TYPE_INT64_T
AC_TYPE_UINT64_T
AC_TYPE_INTPTR_T
AC_TYPE_UINTPTR_T
])


dnl ----------------------------------
dnl -- OPENAXIOM_REJECT_ROTTED_LISP --
dnl ----------------------------------
dnl Check for Lisp systems we know are just too buggy for use.
AC_DEFUN([OPENAXIOM_REJECT_ROTTED_LISP],[
case $1 in
   *gcl*)
      AC_MSG_CHECKING([$1 version])
      v=`$1 -batch -eval "(format t \"~S\" (lisp-implementation-version))"`
      AC_MSG_RESULT([$v])
      case $v in
	*2.6.7*|*2.6.8*) ;;         # OK
	*)
	  AC_MSG_WARN([$v is not supported by this version of OpenAxiom.  $1 will be ignored.])
	  AXIOM_LISP=
	  ;;
      esac
      ;;
   # SBCL-1.0.29 has a nasty regression that prevents OpenAxiom build
   *sbcl*)
      AC_MSG_CHECKING([$1 version])
      v=`$1 --version`
      AC_MSG_RESULT([$v])
      case $v in
	*1.0.29)
	   AC_MSG_ERROR([This version of SBCL has a bug that breaks OpenAxiom build.  Consider SBCL-1.0.30 or higher.])
	   ;;
      esac
      ;;
esac
])

dnl -------------------------
dnl -- OPENAXIOM_PROG_LISP --
dnl -------------------------
dnl Find the host Lisp compiler to use
AC_DEFUN([OPENAXIOM_PROG_LISP],[
## Was a host Lisp system specified?
axiom_lisp=
AC_ARG_WITH([lisp], [ --with-lisp=L         use L as Lisp platform],
              [axiom_lisp=$withval])

## For all values of L, except gcl, the assumption is that the Lisp
## image L is available in the build environment.  For  gcl,
## we make an exception: if no GCL image is available, or if
## the option --enable-gcl is specified then OpenAxiom builds its 
## own version from the source tree.
## If --enable-gcl is specified, we need to check for coonsistency
axiom_include_gcl=
if test -z $axiom_lisp; then
    AC_ARG_ENABLE([gcl], [  --enable-gcl   build GCL from OpenAxiom source],
                  [case $enableval in
                       yes|no) axiom_include_gcl=$enableval ;;
                       *) AC_MSG_ERROR([erroneous value for --enable-gcl]) ;;
                   esac])
fi

## We need to build our own GCL if none is avalaible.
if test -z $axiom_lisp; then
    AC_CHECK_PROGS([AXIOM_LISP], [sbcl gcl ecl clisp ccl ccl64])
    ## A lisp may not be available AND the GCL source may also
    ## be missing.  Instruct user to either build one or get
    ## the dependencies from our website.
    if test -z $AXIOM_LISP && test ! -d ${srcdir}/gcl; then
       AC_MSG_ERROR([OpenAxiom requires a Lisp system.  Either separately build one (GCL-2.6.7, GCL-2.6.8, SBCL, ECL, CLisp, Clozure CL), or get the dependency tarball from OpenAxiom download website.])
    fi
    axiom_lisp=$AXIOM_LISP
else
    ## Honor use of Lisp image specified on command line
    AXIOM_LISP=$axiom_lisp
    AC_SUBST(AXIOM_LISP)
    :
fi
])

dnl -----------------------------------
dnl -- OPENAXIOM_CHECK_GCL_INCLUSION --
dnl -----------------------------------
dnl Check for consistency of configure options when GCL is requested.
AC_DEFUN([OPENAXIOM_CHECK_GCL_INCLUSION],[
AC_SUBST(axiom_include_gcl)

case $axiom_include_gcl,$1 in
    ,|no,|yes*)
       ## It doesn't make sense not to include GCL when no Lisp image
       ## is available.  Give up.
       if test $axiom_include_gcl,$AXIOM_LISP = no,; then
	   AC_MSG_ERROR([--disable-gcl specified but no GCL image found])
       fi

       ## No Lisp image was specified and none was available from
       ## the build environment; build GCL from OpenAxiom source.
       ## User may explicilty specify --enable-gcl, but may be missing
       ## the dependency tarball.
       if test ! -d ${srcdir}/gcl; then
          AC_MSG_ERROR([The OpenAxiom dependency tarball is missing; please get it from our website.])
       fi
       AXIOM_LISP='$(axiom_build_bindir)/gcl'
       axiom_all_prerequisites="$axiom_all_prerequisites all-gcl"
       axiom_include_gcl=yes
       ;;
    yes,*)
       AC_MSG_ERROR([--with-lisp=$1 conflicts with --enable-gcl])
       ;;
esac
])

dnl ---------------------------
dnl -- OPENAXIOM_LISP_FLAVOR --
dnl ---------------------------
dnl Determine the flavor of the host Lisp system.
AC_DEFUN([OPENAXIOM_LISP_FLAVOR],[
OPENAXIOM_CHECK_GCL_INCLUSION($1)

axiom_lisp_flavor=unknown
AC_SUBST(axiom_lisp_flavor)
 
## Most Lisp systems don't use conventional methods for building programs.
oa_standard_linking=no
AC_SUBST(oa_standard_linking)

## The pipe below is OK, for as of this writting, the free Lisp systems
##  ECL, GCL, SBCL, CLisp, and Clozure CL all exit at end of standard input.
AC_MSG_CHECKING([which flavor of Lisp])
if test x"$axiom_include_gcl,$axiom_lisp" = xyes; then
   axiom_lisp_flavor=gcl
else
   case `echo '(lisp-implementation-type)' | $1` in
       *GCL*) 
	   axiom_lisp_flavor=gcl
	   ;;
       *"ECL"*) 
	   axiom_lisp_flavor=ecl 
	   oa_standard_linking=yes
	   ;;
       *"SBCL"*) 
	   axiom_lisp_flavor=sbcl 
	   ;;
       *"CLISP"*)
	   ## Not all variants of CLisp have FFI support.  FFI is used
	   ## internally used by OpenAxiom in essential way.
	   if ! $1 -q -x '*features*' | grep ':FFI' > /dev/null
	   then
	     AC_MSG_ERROR([$1 does not support Foreign Function Interface.  Please upgrade to a better version of CLisp or install SBCL.])
	   fi
	   axiom_lisp_flavor=clisp
	   ;;
       *"Armed Bear Common Lisp"*) 
	   axiom_lisp_flavor=abcl 
	   ;;
       *"Clozure Common Lisp"*)
	   axiom_lisp_flavor=clozure
	   ;;
   esac
fi
AC_MSG_RESULT([$axiom_lisp_flavor])

AC_DEFINE_UNQUOTED([OPENAXIOM_BASE_RTS],
                   [openaxiom_${axiom_lisp_flavor}_runtime],
                   [The kind of base runtime system for this build.])
])

dnl ------------------------------
dnl -- OPENAXIOM_HOST_COMPILERS --
dnl ------------------------------
dnl Check for the host C, C++, and Lisp compilers
AC_DEFUN([OPENAXIOM_HOST_COMPILERS],[
OPENAXIOM_PROG_LISP
OPENAXIOM_LISP_FLAVOR($axiom_lisp)
OPENAXIOM_REJECT_ROTTED_LISP($AXIOM_LISP)
AC_PROG_CC
AC_PROG_CXX
OPENAXIOM_SATISFY_GCL_NEEDS
AC_PROG_CPP
])

dnl -------------------------
dnl -- OPENAXIOM_GCL_HACKS --
dnl -------------------------
dnl Some auxiliary programs generated by GCL need to be at the
dnl right place when compiling under mingw32.
AC_DEFUN([OPENAXIOM_GCL_HACKS],[
## The following is a horrible hack to arrange for GCL to successfully
## rebuild symbol tables with "rsym" on Windows platform.  It should
## go away as soon as GCL upstream is fixed.
AC_SUBST(axiom_gcl_rsym_hack)
case $axiom_lisp_flavor,$target in
    gcl,*mingw*)
        axiom_gcl_rsym_hack='d=`echo "(format nil \"~a\" si::*system-directory*)" | $(AXIOM_LISP) | grep "/gcl.*/" | sed -e "s,\",,g"`; cp $$d/rsym$(EXEEXT) .'
	;;
    *) 
        ## Breath.
        axiom_gcl_rsym_hack=':'
	;;
esac
])

dnl ---------------------------------
dnl -- OPENAXIOM_SATISFY_GCL_NEEDS --
dnl ---------------------------------
dnl GCL assumes that the C compiler is from GNU.
AC_DEFUN([OPENAXIOM_SATISFY_GCL_NEEDS],[
## If we are using GCL as the base runtime system, then we do really need
# a C compiler from GNU.  Well, at least for the moment.
case $axiom_lisp_flavor,$GCC in
   gcl,yes)
       axiom_cflags="-O2 -Wall -D_GNU_SOURCE"
       ;;
   
   gcl,*)
       AC_MSG_ERROR([We need a GNU C compiler])
       ;;
esac
AC_SUBST(axiom_cflags)
])

dnl --------------------------
dnl -- OPENAXIOM_LISP_FLAGS --
dnl --------------------------
dnl Determine how to invoke the host Lisp systemin batch mode.
dnl We also take the opportunity to determine whether we can use
dnl dynamically loaded modules.
AC_DEFUN([OPENAXIOM_LISP_FLAGS],[
AC_SUBST(axiom_quiet_flags)
AC_SUBST(axiom_eval_flags)

## Can we use dynamically linked libraries?  
## Tentatively answer `yes' -- this is modern time.
oa_use_dynamic_lib=yes
AC_SUBST(oa_use_dynamic_lib)

## How are we supposed to tell the Lisp system to eval an expression
## in batch mode?  What is the extension of a compiled Lisp file?
case $axiom_lisp_flavor in
    gcl) 
       axiom_quiet_flags='-batch'
       axiom_eval_flags='-eval'
       oa_use_dynamic_lib=no  
       ;;
    ecl) 
       axiom_quiet_flags=
       axiom_eval_flags='-norc -eval'
       oa_use_dynamic_lib=no  
       ;;
    sbcl) 
       axiom_quiet_flags='--noinform --noprint'
       axiom_eval_flags='--eval'
       ;;
    clisp) 
       axiom_quiet_flags='--quiet'
       axiom_eval_flags='-norc -x'
       ;;
    clozure)
       axiom_quiet_flags='--quiet --no-init'
       axiom_eval_flags='--eval'
       ;;
    *) AC_MSG_ERROR([We do not know how to build OpenAxiom this $axiom_lisp]) ;;
esac
])


dnl -------------------------------
dnl -- OPENAXIOM_FILE_EXTENSIONS --
dnl -------------------------------
dnl Compute various file extensions used by the build system.
AC_DEFUN([OPENAXIOM_FILE_EXTENSIONS],[
AC_SUBST(axiom_fasl_type)
AC_MSG_CHECKING([compiled Lisp file extension])
if test x"$axiom_include_gcl" = xyes; then
   axiom_fasl_type=o
else
   ## We set the IFS to <space> as we don't want automatic
   ## replacement of <newline> by <space>.
   openaxiom_save_IFS=$IFS
   IFS=' '
   axiom_fasl_type=`$1 $axiom_quiet_flags $axiom_eval_flags '(progn (format t "axiom_fasl_type=~a" (pathname-type (compile-file-pathname "foo.lisp"))) (quit))'`

   ## Now pull out the fasl type.  ECL has the habit of spitting noise
   ## about internal loading.  Therefore, we must look only for a line that
   ## begins with axiom_fasl_type.
   axiom_fasl_type=`echo $axiom_fasl_type | grep '^axiom_fasl_type'`
   IFS=$openaxiom_save_IFS
   axiom_fasl_type=`echo $axiom_fasl_type | sed -e 's/axiom_fasl_type=//'`
   if test -z $axiom_fasl_type; then
       AC_MSG_ERROR([Could not determine extension for compiled Lisp files])
   fi
fi
AC_MSG_RESULT([$axiom_fasl_type])

## What is the extension of object and executable files on this platform?
AC_OBJEXT
AC_DEFINE_UNQUOTED([OPENAXIOM_EXEEXT], ["$ac_cv_exeext"], 
                   [Extension of executable file.])
])

dnl ------------------------------
dnl -- OPENAXIOM_FFI_TYPE_TABLE --
dnl ------------------------------
dnl Build FFI type translation table used by
dnl the Boot translator and the Spad compiler
AC_DEFUN([OPENAXIOM_FFI_TYPE_TABLE],[
AC_SUBST(void_type)
AC_SUBST(char_type)
AC_SUBST(int_type)
AC_SUBST(float_type)
AC_SUBST(double_type)
AC_SUBST(string_type)

case $axiom_lisp_flavor in
   gcl)
      void_type='void'
      char_type='char'
      int_type='int'
      float_type='float'
      double_type='double'
      string_type='string'
      ;;
   sbcl)
      void_type='void'
      char_type='char'
      int_type='int'
      float_type='float'
      double_type='double'
      string_type='c-string'
      ;;
   clisp)
      void_type='nil'
      char_type='character'
      int_type='int'
      float_type='single-float'
      double_type='double-float'
      string_type='c-string'
      ;;
   ecl)
      void_type=':void'
      char_type=':char'
      int_type=':int'
      float_type=':float'
      double_type=':double'
      string_type=':cstring'
      ;;
   clozure)
      void_type=':void'
      # FIXME: this is not really what we want, but good enough for now.
      char_type=':unsigned-byte'
      int_type=':signed-fullword'
      float_type=':single-float'
      double_type=':double-float'
      # Clozure CL wants you to deal with your own mess
      string_type=':address'
      ;;
   *)
      AC_MSG_ERROR([We do not know how to translate native types for this Lisp])
      ;;
esac
])


dnl ---------------------------------------
dnl -- OPENAXIOM_HOST_LISP_CPU_PRECISION --
dnl ---------------------------------------
dnl Determine the register precision as seen by the host Lisp system, and
dnl set the global variable openaxiom_host_lisp_precision.
AC_DEFUN([OPENAXIOM_HOST_LISP_CPU_PRECISION], [
if test x"$axiom_include_gcl" != xyes; then
   AC_MSG_CHECKING([CPU precision as seen by $AXIOM_LISP])
   case `echo '*features*' | $AXIOM_LISP` in
     *X86-64*|*X86_64*|*WORD-SIZE=64*|*64-BIT-HOST*)
	# PORTME: the pattern above covers only the supported free Lisps, i.e.
	# GCL, SBCL, CLisp, ECL and Clozure CL.
	openaxiom_host_lisp_precision=64
	;;
     *)
	# assume everything else is 32-bit
	# FIXME: this is bold assumption.
	openaxiom_host_lisp_precision=32
	;;
   esac
   AC_MSG_RESULT([$openaxiom_host_lisp_precision])
fi
])


dnl ------------------------------------
dnl -- OPENAXIOM_HOST_DATA_PROPERTIES --
dnl ------------------------------------
AC_DEFUN([OPENAXIOM_HOST_DATA_PROPERTIES],[
OPENAXIOM_HOST_LISP_CPU_PRECISION
## Byte order of the host.
AC_C_BIGENDIAN
AC_CHECK_HEADERS([stdint.h inttypes.h])
OPENAXIOM_STANDARD_INTEGER_TYPES
AC_CHECK_SIZEOF([void*])
if test x"$axiom_include_gcl" = xyes; then
   openaxiom_host_lisp_precision=`expr "$ac_cv_sizeof_voidp * 8"`
fi

## Now that we have full knowledge of the host Lisp to use, tell
## the rest of the runtime about the host natural integer precision.
AC_DEFINE_UNQUOTED([OPENAXIOM_HOST_LISP_PRECISION],
                   [$openaxiom_host_lisp_precision],
                   [The width of the host Lisp and CPU registers.])

## Augment compiler flags with ABI directives as appropriate.
case $GCC in
  yes)
    CFLAGS="$CFLAGS -m$openaxiom_host_lisp_precision"
    CXXFLAGS="$CXXFLAGS -m$openaxiom_host_lisp_precision"
    ;;
  no)
    # cross fingers and pray.
    ;;
esac
])
