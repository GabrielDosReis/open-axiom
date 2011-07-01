AC_DEFUN([OPENAXIOM_MAKEFILE],
[AC_CONFIG_FILES([$1:config/var-def.mk:$1.in:config/setup-dep.mk])])

dnl --------------------------------------
dnl -- OPENAXIOM_STANDARD_INTEGER_TYPES --
dnl --------------------------------------
dnl Check for availability of standard sized integer types.
AC_DEFUN([OPENAXIOM_STANDARD_INTEGER_TYPES], [
AC_CHECK_TYPES([int8_t],[],[AC_TYPE_INT8_T])
AC_CHECK_TYPES([uint8_t],[],[AC_TYPE_UINT8_T])
AC_CHECK_TYPES([int16_t],[],[AC_TYPE_INT16_T])
AC_CHECK_TYPES([uint16_t],[][AC_TYPE_UINT16_T])
AC_CHECK_TYPES([int32_t],[],[AC_TYPE_INT32_T])
AC_CHECK_TYPES([uint32_t],[],[AC_TYPE_UINT32_T])
AC_CHECK_TYPES([int64_t],[],[AC_TYPE_INT64_T])
AC_CHECK_TYPES([uint64_t],[],[AC_TYPE_UINT64_T])
AC_CHECK_TYPES([intptr_t],[],[AC_TYPE_INTPTR_T])
AC_CHECK_TYPES([uintptr_t],[],[AC_TYPE_UINTPTR_T])
AC_CHECK_TYPES([intmax_t],[],[AC_TYPE_INTMAX_T])
AC_CHECK_TYPES([uintmax_t],[],[AC_TYPE_UINTMAX_T])
AC_TYPE_UID_T
])


dnl ----------------------------------
dnl -- OPENAXIOM_REJECT_ROTTED_LISP --
dnl ----------------------------------
dnl Check for Lisp systems we know are just too buggy for use.
AC_DEFUN([OPENAXIOM_REJECT_ROTTED_LISP],[
if test x"$oa_include_gcl" != xyes; then
   case $AXIOM_LISP in
      *gcl*)
	 AC_MSG_CHECKING([$AXIOM_LISP version])
	 v=`$AXIOM_LISP -batch -eval "(format t \"~S\" (lisp-implementation-version))"`
	 AC_MSG_RESULT([$v])
	 case $v in
	   *2.6.7*|*2.6.8*) ;;         # OK
	   *)
	     AC_MSG_WARN([$v is not supported by this version of OpenAxiom.  $AXIOM_LISP will be ignored.])
	     AXIOM_LISP=
	     ;;
	 esac
	 ;;
      # SBCL-1.0.29 has a nasty regression that prevents OpenAxiom build
      *sbcl*)
	 AC_MSG_CHECKING([$AXIOM_LISP version])
	 v=`$AXIOM_LISP --version`
	 AC_MSG_RESULT([$v])
	 case $v in
	   *1.0.29)
	      AC_MSG_ERROR([This version of SBCL has a bug that breaks OpenAxiom build.  Consider SBCL-1.0.30 or higher.])
	      ;;
	 esac
	 ;;
   esac
fi
])

dnl -------------------------
dnl -- OPENAXIOM_PROG_LISP --
dnl -------------------------
dnl Find the host Lisp compiler to use
AC_DEFUN([OPENAXIOM_PROG_LISP],[
## Was a host Lisp system specified?
oa_user_lisp=
AC_ARG_WITH([lisp], [ --with-lisp=L         use L as Lisp platform],
              [oa_user_lisp=$withval])

## For all values of L, except gcl, the assumption is that the Lisp
## image L is available in the build environment.  For  gcl,
## we make an exception: if no GCL image is available, or if
## the option --enable-gcl is specified then OpenAxiom builds its 
## own version from the source tree.
## If --enable-gcl is specified, we need to check for coonsistency
oa_include_gcl=
AC_ARG_ENABLE([gcl], [  --enable-gcl            build GCL from OpenAxiom source],
	      [case $enableval in
		   yes|no) oa_include_gcl=$enableval ;;
		   *) AC_MSG_ERROR([erroneous value for --enable-gcl]) ;;
	       esac])

## If nothing was said about preferred Lisp, guess one.
AC_SUBST(AXIOM_LISP)
if test -n "$oa_user_lisp"; then
    ## Honor use of Lisp image specified on command line
    AXIOM_LISP=$oa_user_lisp
elif test -z "$oa_include_gcl"; then
    AC_CHECK_PROGS([AXIOM_LISP], [sbcl gcl ecl clisp ccl64 ccl32 ccl])
fi
])

dnl -----------------------------------
dnl -- OPENAXIOM_CHECK_GCL_INCLUSION --
dnl -----------------------------------
dnl Check for consistency of configure options when GCL is requested.
AC_DEFUN([OPENAXIOM_CHECK_GCL_INCLUSION],[
case $oa_include_gcl,$AXIOM_LISP in
    no,)
       ## It doesn't make sense not to include GCL when no Lisp image
       ## is available.  Give up.
       AC_MSG_ERROR([--disable-gcl specified but no Lisp system found])
       ;;

    ,|yes,)
       ## No Lisp image was specified and none was available from
       ## the build environment; build GCL from OpenAxiom source.
       ## User may explicilty specify --enable-gcl, but may be missing
       ## the dependency tarball.
       if test -d ${srcdir}/gcl; then
	  AXIOM_LISP='$(axiom_build_bindir)/gcl'
	  oa_include_gcl=yes
       elif test -z "$oa_include_gcl"; then
	  AC_MSG_ERROR([OpenAxiom requires a Lisp system.  Either separately build one (GCL-2.6.7, GCL-2.6.8, SBCL, ECL, CLisp, Clozure CL), or get the dependency tarball from OpenAxiom download website.])
       else
          AC_MSG_ERROR([The OpenAxiom dependency tarball is missing; please get it from our website.])
       fi
       ;;
    yes,*)
       AC_MSG_ERROR([--with-lisp=$AXIOM_LISP conflicts with --enable-gcl])
       ;;
esac
])

dnl ---------------------------
dnl -- OPENAXIOM_LISP_FLAVOR --
dnl ---------------------------
dnl Determine the flavor of the host Lisp system.
AC_DEFUN([OPENAXIOM_LISP_FLAVOR],[
OPENAXIOM_CHECK_GCL_INCLUSION

axiom_lisp_flavor=unknown
AC_SUBST(axiom_lisp_flavor)
 
## Most Lisp systems don't use conventional methods for building programs.
oa_standard_linking=no
AC_SUBST(oa_standard_linking)

## The pipe below is OK, for as of this writting, the free Lisp systems
##  ECL, GCL, SBCL, CLisp, and Clozure CL all exit at end of standard input.
AC_MSG_CHECKING([which flavor of Lisp])
if test x"$oa_include_gcl" = xyes; then
   axiom_lisp_flavor=gcl
else
   case `echo '(lisp-implementation-type)' | $AXIOM_LISP` in
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
	   if ! $AXIOM_LISP -q -x '*features*' | grep ':FFI' > /dev/null
	   then
	     AC_MSG_ERROR([$AXIOM_LISP does not support Foreign Function Interface.  Please upgrade to a better version of CLisp or install SBCL.])
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
                   [${axiom_lisp_flavor}_runtime],
                   [The kind of base runtime system for this build.])
])

dnl --------------------------------------------
dnl -- OPENAXIOM_CPPFLAGS_FOR_VENDOR_LOCK_INS --
dnl --------------------------------------------
dnl Adjust CPPFLAGS before detecting several vendor lock-ins (or not.)
AC_DEFUN([OPENAXIOM_CPPFLAGS_FOR_VENDOR_LOCK_INS],[
case $host in
  *linux*)
     CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
     ;;
  *bsd*|*dragonfly*)
     CPPFLAGS="$CPPFLAGS -D_BSD_SOURCE"
     ;;
  *mingw*)
     CPPFLAGS="$CPPFLAGS -DOPENAXIOM_MS_WINDOWS_HOST"
     ;;
  *solaris*)
     ## FIXME: This should disappear
     CPPFLAGS="$CPPFLAGS -DSUNplatform"
     ;;
esac
])

dnl ------------------------------
dnl -- OPENAXIOM_HOST_COMPILERS --
dnl ------------------------------
dnl Check for the host C, C++, and Lisp compilers
AC_DEFUN([OPENAXIOM_HOST_COMPILERS],[
OPENAXIOM_PROG_LISP
OPENAXIOM_LISP_FLAVOR
OPENAXIOM_REJECT_ROTTED_LISP
OPENAXIOM_HOST_LISP_CPU_PRECISION
## Are we using compilers from GNU?
oa_gnu_compiler=no
AC_PROG_CC
AC_PROG_CXX
if test x$GCC = xyes || test x$GXX = xyes; then
  oa_gnu_compiler=yes
fi
## Augment C and C++ compiler flags with ABI directives as appropriate
## before we proceed to infer other host datatype properties.
if test -n "$openaxiom_host_lisp_precision"; then
   if test x$oa_gnu_compiler = xyes; then
     CPPFLAGS="$CPPFLAGS -m$openaxiom_host_lisp_precision"
     LDFLAGS="$LDFLAGS -m$openaxiom_host_lisp_precision"
   ## else, cross fingers and pray.
   fi
fi
OPENAXIOM_SATISFY_GCL_NEEDS
AC_PROG_CPP
AC_PROG_CXXCPP
OPENAXIOM_CPPFLAGS_FOR_VENDOR_LOCK_INS
])

dnl ---------------------------------
dnl -- OPENAXIOM_SATISFY_GCL_NEEDS --
dnl ---------------------------------
dnl GCL assumes that the C compiler is from GNU.
AC_DEFUN([OPENAXIOM_SATISFY_GCL_NEEDS],[
## If we are using GCL as the base runtime system, then we do really need
## a C compiler from GNU.  Well, at least for the moment.
case $axiom_lisp_flavor,$oa_gnu_compiler in
   gcl,yes)
       axiom_cflags="-O2 -Wall -D_GNU_SOURCE"
       ;;
   
   gcl,*)
       AC_MSG_ERROR([We need a GNU C compiler])
       ;;
esac
AC_SUBST(axiom_cflags)
])

dnl ---------------------------------
dnl -- OPENAXIOM_GCL_BUILD_OPTIONS --
dnl ---------------------------------
dnl Determine options needed to build included GCL, if need be.
AC_DEFUN([OPENAXIOM_GCL_BUILD_OPTIONS],[
oa_host_has_libbfd=
## Check for these only if we are going to build GCL from source.
if test x"$oa_include_gcl" = xyes; then
    AC_CHECK_HEADER([bfd.h])
    AC_HAVE_LIBRARY([bfd], [oa_host_has_libbfd=yes])

    oa_gcl_bfd_option=
    if test x"$ac_cv_header_bfd_h" = xyes \
	&& test x"$oa_host_has_libbfd" = xyes; then
	oa_gcl_bfd_option="--disable-dynsysbfd"
    else
	oa_gcl_bfd_option="--disable-statsysbfd"
    fi
fi

case $host in
   powerpc*darwin*)
      axiom_gcl_bfd_option="--disable-statsysbfd \
                                --enable-machine=powerpc-macosx"
      axiom_gcl_mm_option="--enable-vssize=65536*2"
      ;;
esac

## We don't need GCL to build support for X Window system or TCL/TK:
oa_gcl_x_option="--disable-tkconfig --disable-x --disable-xgcl --disable-tcltk"

GCLOPTS="$oa_gcl_bfd_option $oa_gcl_mm_option $oa_gcl_x_option"
AC_SUBST(GCLOPTS)
])

dnl --------------------------
dnl -- OPENAXIOM_LISP_FLAGS --
dnl --------------------------
dnl Determine how to invoke the host Lisp system in batch mode.
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
    *) AC_MSG_ERROR([We do not know how to build OpenAxiom this $AXIOM_LISP]) ;;
esac
])


dnl -------------------------------
dnl -- OPENAXIOM_FILE_EXTENSIONS --
dnl -------------------------------
dnl Compute various file extensions used by the build system.
AC_DEFUN([OPENAXIOM_FILE_EXTENSIONS],[
AC_SUBST(axiom_fasl_type)
AC_MSG_CHECKING([compiled Lisp file extension])
if test x"$oa_include_gcl" = xyes; then
   axiom_fasl_type=o
else
   ## We set the IFS to <space> as we don't want automatic
   ## replacement of <newline> by <space>.
   openaxiom_save_IFS=$IFS
   IFS=' '
   axiom_fasl_type=`$AXIOM_LISP $axiom_quiet_flags $axiom_eval_flags '(progn (format t "axiom_fasl_type=~a" (pathname-type (compile-file-pathname "foo.lisp"))) (quit))'`

   ## Now pull out the fasl type.  ECL has the habit of spitting noise
   ## about internal loading.  Therefore, we must look only for a line that
   ## begins with axiom_fasl_type.
   axiom_fasl_type=`echo $axiom_fasl_type | grep '^axiom_fasl_type'`
   IFS=$openaxiom_save_IFS
   axiom_fasl_type=`echo $axiom_fasl_type | sed -e 's/axiom_fasl_type=//'`
   if test -z "$axiom_fasl_type"; then
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
if test x"$oa_include_gcl" != xyes; then
   AC_MSG_CHECKING([CPU precision as seen by $AXIOM_LISP])
   # Almost all Lisp systems we support put the relevant information
   # on the *FEATURES* list.  Except ECL.
   case $axiom_lisp_flavor in
     ecl)
       features_expr='(progn (require :cmp) (eval (find-symbol "*COMPILER-FEATURES*" :c)))'
       ;;
      *)
       features_expr='*features*'
       ;;
   esac
   case `echo $features_expr | $AXIOM_LISP` in
     *X86-64*|*X86_64*|*WORD-SIZE=64*|*64-BIT*)
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
## Byte order of the host.
AC_C_BIGENDIAN
OPENAXIOM_STANDARD_INTEGER_TYPES
AC_CHECK_SIZEOF([void*])
if test x"$oa_include_gcl" = xyes; then
   ## PORTME: does GCL really care about system where CHAR_BITS is not 8?
   openaxiom_host_lisp_precision=`expr "$ac_cv_sizeof_voidp * 8"`
fi

## Now that we have full knowledge of the host Lisp to use, tell
## the rest of the runtime about the host natural integer precision.
AC_DEFINE_UNQUOTED([OPENAXIOM_HOST_LISP_PRECISION],
                   [$openaxiom_host_lisp_precision],
                   [The width of the host Lisp and CPU registers.])
])

dnl --------------------------------------
dnl -- OPENAXIOM_DYNAMIC_MODULE_SUPPORT --
dnl --------------------------------------
dnl Infer compiler flags and file extensions associated
dnl with dynamic module support.
dnl We need to link some C object files into in the Lisp images we
dnl use.  Some Lisps (e.g. GCL, ECL) support inclusion of ``ordinary''
dnl object files.  Other Lisps (e.g. SBCL, Clozure CL) support only dynamic
dnl or shared libraries.  However, the exact minutia of  portably
dnl building shared libraries are known to be fraught with all kinds
dnl of traps.  Consequently, we sought to use dedicated tools such
dnl Libtool.  Unfortunately, Libtool has been steadily improved over the years
dnl to become nearly useless when mixed with non-libtool components.
AC_DEFUN([OPENAXIOM_DYNAMIC_MODULE_SUPPORT],[
AC_SUBST(oa_use_libtool_for_shared_lib)
AC_SUBST(oa_shrobj_flags)
AC_SUBST(oa_shrlib_flags)
oa_use_libtool_for_shared_lib=no
oa_shrobj_flags=
oa_shrlib_flags=
## Tell Libtool to assume `dlopen' so that it does not have to
## emulate it.
LT_PREREQ([2.2.6])
LT_INIT([pic-only dlopen win32-dll shared])
AC_SUBST([LIBTOOL_DEPS])
# Give me extension of libraries
AC_SUBST(shared_ext)
AC_SUBST(libext)
module=yes
eval shared_ext=\"$shrext_cmds\"
case $host in
    *mingw*|*cygwin*)
       oa_shrobj_flags='-prefer-pic'
       oa_shrlib_flags="-shared --export-all-symbols"
       ;;
    *darwin*)
       oa_shrobj_flags='-dynamic'
       oa_shrlib_flags='-bundle -undefined suppress -flat_namespace'
       ;;
    *)
       oa_shrobj_flags='-prefer-pic'
       oa_shrlib_flags='-shared'
       ;;
esac
])


dnl ---------------------------
dnl -- OPENAXIOM_BUILD_TOOLS --
dnl ---------------------------
dnl Check for utilities we need for building the system.
AC_DEFUN([OPENAXIOM_BUILD_TOOLS],[
AC_CHECK_PROG([TOUCH], [touch],
              [touch], [AC_MSG_ERROR(['touch' program is missing.])])
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_SED
AC_CHECK_PROGS([MKTEMP], [mktemp])
AC_PROG_AWK

## Find GNU Make
case $build in
    *linux*)
	# GNU/Linux systems come equipped with GNU Make, called `make'
        AC_CHECK_PROGS([MAKE], [make],
                       [AC_MSG_ERROR([Make utility missing.])])
	;;
    *)
        # Other systems tend to spell it `gmake' and such
        AC_CHECK_PROGS([MAKE], [gmake make],
                       [AC_MSG_ERROR([Make utility missing.])])
	if ! $MAKE --version | grep 'GNU' 2>/dev/null; then
	    AC_MSG_ERROR([OpenAxiom build system needs GNU Make.])
	fi
	;;
esac
])

dnl ---------------------------
dnl -- OPENAXIOM_HOST_EDITOR --
dnl ---------------------------
dnl Check for a text editor for use when
dnl the system is up and running.
AC_DEFUN([OPENAXIOM_HOST_EDITOR],[
AC_SUBST(oa_editor)
## On Windows system, we prefer the default installation
## location to be 'C:/Program Files/OpenAxiom', following Windows 
## convention.  We cannot use AC_PREFIX_DEFAULT directly as it seems 
## to operate unconditionally.  Therefore, we resort to this dirty
## trick stepping over Autoconf's internals.
case $host in
    *mingw*)
        ac_default_prefix="C:/Program Files/OpenAxiom"
        AC_PATH_PROGS([oa_editor],[notepad.exe])
	;;
    *)  
        AC_PATH_PROGS([oa_editor],[vi])
        ;;
esac
])

dnl --------------------------
dnl -- OPENAXIOM_HOST_PROGS --
dnl --------------------------
dnl Check for programs we need in the host environment.
AC_DEFUN([OPENAXIOM_HOST_PROGS],[
OPENAXIOM_HOST_EDITOR
AC_PATH_PROG([PDFLATEX], [pdflatex])
if test -z "$PDFLATEX"; then
   AC_PATH_PROG([LATEX], [latex],
                [AC_MSG_NOTICE([Documentation is disabled.])])
fi
AC_CHECK_PROGS([MAKEINDEX], [makeindex])
])


dnl -----------------------------
dnl -- OPENAXIOM_BUILD_OPTIONS --
dnl -----------------------------
dnl Process build options specified on the command line.
AC_DEFUN([OPENAXIOM_BUILD_OPTIONS],[
## Does it make sense to pretend that we support multithreading?
oa_enable_threads=no
AC_ARG_ENABLE([threads], [  --enable-threads   turn on threads support],
              [case $enableval in
                  yes|no) oa_enable_threads=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-threads]) ;;
               esac])
# GNU compilers want hints about multithreading.
case $oa_gnu_compiler,$oa_enable_threads in
   yes,yes)
     axiom_cflags="$axiom_cflags -pthread"
esac
AC_SUBST(oa_enable_threads)

## Occaionally, we may want to keep intermediary files.
oa_keep_files=
AC_ARG_ENABLE([int-file-retention], 
              [  --enable-int-file-retention   keep intermediary files],
              [case $enableval in
                  yes|no) oa_keep_files=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-int-file-retention]) ;;
               esac])
AC_SUBST(oa_keep_files)

## Parse args for profiling-enabled build.
oa_enable_profiling=no
AC_ARG_ENABLE([profiling], [  --enable-profiling  turn profiling on],
              [case $enableval in
                  yes|no) oa_enable_profiling=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-profiling]) ;;
               esac])
AC_SUBST(oa_enable_profiling)

## Lisp optimization settings
oa_optimize_options=speed
## Shall we proclaim safety?
oa_enable_checking=no          # don't turn on checking by default.
AC_ARG_ENABLE([checking], [  --enable-checking  turn runtime checking on],
              [case $enableval in
                  yes|no) oa_enable_checking=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-checking]) ;;
               esac])
if test x"$oa_enable_checking" = xyes; then
   case $axiom_lisp_flavor in
     gcl) # GCL-2.6.x does not understand debug.
        oa_optimize_options="$oa_optimize_options safety" 
        ;;
     *) oa_optimize_options="$oa_optimize_options safety debug" 
        ;;
   esac
   AC_MSG_NOTICE([runtime checking may increase compilation time])
fi
AC_SUBST(oa_enable_checking)
AC_SUBST(oa_optimize_options)
])


dnl -----------------------------
dnl -- OPENAXIOM_CHECK_SIGNALS --
dnl -----------------------------
dnl The host platform must be able to handle signals.  Although, this is 
dnl not strictly necessary, that is the way OpenAxiom source code
dnl is currently written.  We ask for a POSIX or ISO C semantics, though
dnl we have a strong preference for POSIX-conformant semantics.
AC_DEFUN([OPENAXIOM_CHECK_SIGNALS],[
AC_CHECK_HEADERS([signal.h],
                 [],
                 [AC_MSG_ERROR([OpenAxiom needs signal support.])])
AC_CHECK_DECLS([sigaction], [], [], 
               [#include <signal.h>])
AC_CHECK_DECLS([kill], [], [],
               [#include <signal.h>])
])


dnl -----------------------------
dnl -- OPENAXIOM_CHECK_SOCKETS --
dnl -----------------------------
dnl The host environment must be capable of handling communication through
dnl sockets.  This is required for interfacing AXIOMsys
dnl and Superman.  Notice that ideally, we should decouple
dnl that interface in such a way that we can still build OpenAxiom
dnl when Superman is not needed or a socket library is not
dnl available.
AC_DEFUN([OPENAXIOM_CHECK_SOCKETS],[
case $host in
    *mingw*)
	AC_CHECK_HEADERS([winsock2.h],
	                [axiom_host_has_socket=yes],
			[])
	oa_c_runtime_extra="-lwsock32"
	;;
    *)
        AC_CHECK_HEADERS([sys/socket.h], 
                         [axiom_host_has_socket=yes],
		         [])
	;;
esac
if test x$axiom_host_has_socket != xyes; then \
    AC_MSG_ERROR([OpenAxiom needs suport for sockets.])
fi
## solaris-based systems tend to hide the socket library.
case $host in
    *solaris*)
       AC_SEARCH_LIBS([accept], [socket],
	   [], [AC_MSG_ERROR([socket library not found])])
       AC_SEARCH_LIBS([gethostbyname], [nsl])
       ;;
    *) ;;
esac

AC_EGREP_CPP([has_af_local],
             [#if HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#else
#  include <winsock2.h>
#endif
#ifdef AF_LOCAL
   has_af_local
#endif
             ],
             [AC_DEFINE([HAVE_AF_LOCAL], [1], [Host has AF_LOCAL])])


AC_EGREP_CPP([has_af_unix],
             [#if HAVE_SYS_SOCKET_H
#  include <sys/socket.h>
#else
#  include <winsock2.h>
#endif
#ifdef AF_UNIX
   has_af_unix
#endif
             ],
             [AC_DEFINE([HAVE_AF_UNIX], [1], [Host has AF_UNIX])])
])

dnl --------------------------------
dnl -- OPENAXIOM_CHECK_FILESYSTEM --
dnl --------------------------------
dnl Some parts of OpenAxiom manipulate files and directories.  They
dnl more or less directly reflect the underlying platform semantics.
dnl For the moment, we require POSIX semantics, though that does not
dnl seem necessary.  That restriction should be removed as soon as possible.
AC_DEFUN([OPENAXIOM_CHECK_FILESYSTEM],[
AC_CHECK_HEADERS([sys/stat.h],
                 [],
		 [AC_MSG_ERROR([OpenAxiom needs <sys/stat.h>])])
case $host in
    *mingw*)
        ;;
    *)
	AC_CHECK_HEADERS([dirent.h],
			 [],
			 [AC_MSG_ERROR([OpenAxiom needs <dirent.h>])])
        ;;
esac

AC_CHECK_HEADERS([unistd.h], [],
                 [AC_MSG_ERROR([OpenAxiom needs <unistd.h>])])
])

dnl -----------------------------
dnl -- OPENAXIOM_CHECK_PROCESS --
dnl -----------------------------
AC_DEFUN([OPENAXIOM_CHECK_PROCESS],[
AC_CHECK_DECLS([getuid, geteuid, getgid, getegid], [], [],
               [#include <unistd.h>])
AC_CHECK_HEADERS([sys/wait.h])
if test x"$ac_cv_header_sys_wait_h" = xyes; then \
    AC_CHECK_DECLS([wait], 
                   [], 
                   [], 
                   [#include <sys/wait.h>])
fi
AC_CHECK_DECLS([fork],
               [],
               [],
               [#include <unistd.h>])
])

dnl ----------------------------------
dnl -- OPENAXIOM_CHECK_CORE_SUPPORT --
dnl ----------------------------------
AC_DEFUN([OPENAXIOM_CHECK_CORE_SUPPORT],[
oa_c_runtime=
AC_SUBST(oa_c_runtime)

oa_c_runtime_extra=
AC_SUBST(oa_c_runtime_extra)

OPENAXIOM_CHECK_FILESYSTEM
OPENAXIOM_CHECK_SIGNALS
OPENAXIOM_CHECK_SOCKETS
OPENAXIOM_CHECK_PROCESS
OPENAXIOM_CHECK_GMP
])

dnl ------------------------
dnl -- OPENAXIOM_CHECK_IO --
dnl ------------------------
AC_DEFUN([OPENAXIOM_CHECK_IO],[
# Honor support for large files
AC_SYS_LARGEFILE
# What about long file names?
AC_SYS_LONG_FILE_NAMES
# Posix terminal IO?
AC_SYS_POSIX_TERMIOS
## Does this system have openpty or shall we emulate?
AC_CHECK_HEADERS([sys/ioctl.h pty.h util.h libutil.h termios.h])
AC_CHECK_DECLS([openpty],[],[],
   [#if HAVE_PTY_H
#  include <pty.h>
#endif
#if HAVE_UTIL_H
#  include <util.h>
#endif
#if HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
#endif
#if HAVE_TERMIOS_H
#  include <termios.h>
#endif
#if HAVE_LIBUTIL_H
#  include <sys/types.h>
#  include <libutil.h>
#endif
   ])
if test x"$ac_cv_have_decl_openpty" = xyes; then \
   AC_SEARCH_LIBS([openpty],[util])
fi

oa_use_sman=1
if test x"$ac_cv_have_decl_fork" = xyes \
     -a x"$ac_cv_have_decl_wait" = xyes; then \
    oa_c_runtime="$oa_c_runtime terminal_io"
else
    oa_use_sman=0
    AC_MSG_NOTICE([Superman component is disabled.])
fi

AC_DEFINE_UNQUOTED([OPENAXIOM_USE_SMAN], [$oa_use_sman],
                   [Whether to use the session manager as driver.])
])


dnl -------------------------
dnl -- OPENAXIOM_CHECK_X11 --
dnl -------------------------
dnl One of the thorniest issues with programs that use the X Window System
dnl is portability.  There exist many implementations of the X11
dnl specification, each with its own variations, extensions, and what
dnl not.  Designing hand-written makefiles for such programs can be a
dnl daunting task, fraut with all kinds of traps.  Fortunately, Autoconf
dnl provides us with some help, namely the macro [[AC_PATH_X]] and 
dnl [[AC_PATH_XTRA]].  The former searches the directories where the
dnl X11 include files and the library files reside.  The latter is an
dnl enhanced version that:
dnl  1. computes the C compiler flags required by X11;
dnl  2. computes the linker flags required by X11;
dnl  3. checks for special libraries that some systems need in order to
dnl     compile X11 programs;
dnl  4. checks for special X11R6 libraries that need to be linked before
dnl      the flag [[-lX11]].
AC_DEFUN([OPENAXIOM_CHECK_X11],[
AC_PATH_XTRA
## Output directives for the C compiler
AC_SUBST(X_CLFAGS)
## Output directives for the linker
AC_SUBST(X_LIBS)
## Output any extra libraries required by X11
AC_SUBST(X_EXTRA_LIBS)

## Finally, output the list of libraries that need to appear before -lX11
## Some part of OpenAxiom depends on Xpm.  That library has kind uncertain
## future.  At some point in the past, it was deprecated, to be
## replaced by xpm-nox; then came back again.  So, its support may
## vary from system to system.  For the moment, we assume that if X11
## is found then, Xpm is already present.  Though, clearly that is a
## very optimistic assumption.  Long term, OpenAxiom should get rid of
## dependence on Xpm.  A nearly fool-proof test would be probably
## inspired by AC_PATH_XTRA.  I don't have time to get to that 
## complication right now.  Will fix later.
X_PRE_LIBS="-lXpm $X_PRE_LIBS"
AC_SUBST(X_PRE_LIBS)

## If the system supports X11, then build graphics
axiom_use_x=no
if test -z "$no_x"; then
    axiom_use_x=yes
    oa_c_runtime="$oa_c_runtime graphics"
else
    AC_MSG_NOTICE([The Garphics component is disabled.])
fi
AC_SUBST(axiom_use_x)
])

dnl ------------------------
dnl -- OPENAXIOM_CHECK_QT --
dnl ------------------------
AC_DEFUN([OPENAXIOM_CHECK_QT],[
# Check for Qt utilities.
oa_has_qt=no
AC_CHECK_PROGS([OA_QT_MOC], [moc])
AC_CHECK_PROGS([OA_QT_QMAKE], [qmake])
if test -n "$OA_QT_MOC"; then
  AC_MSG_CHECKING([Qt version])
  oa_qt_version=`"$OA_QT_MOC" -v 2>&1 | sed -e 's/^.*(\(.*\))$/\1/'`
  AC_MSG_RESULT([$oa_qt_version])
  case $oa_qt_version in
    *[1-3]\.[0-9]+\.[0-9]+)
       AC_MSG_WARN([This version of Qt is too old for OpenAxiom.])
       ;;
    *) 
       oa_has_qt=yes
       ;;
  esac
fi
AC_DEFINE_UNQUOTED([OPENAXIOM_USE_GUI], [`expr x$oa_has_qt = xyes`],
                   [Whether to use the QT-based GUI interface as driver.])
])

dnl -------------------------------------
dnl -- OPENAXIOM_CHECK_BROWSER_SUPPORT --
dnl -------------------------------------
dnl The HyperDoc component needs string pattern matching.  
dnl We require [[<regex.h>]], with POSIX-conformant definition.  We used
dnl to key build of HyperDoc component on the availability of X11
dnl functionalities.  That, however, is a severe restriction.  Not all
dnl of the HyperDoc components need X11.  Some, such as [[htadd]], don't
dnl need X11 at all.  Therefore we have lifted part of the restrictions.
dnl See \File{src/hyper/Makefile} for more details.  Note that is we don't
dnl build the HyperDoc component, the compilation of algebra files are
dnl drawn in [[Unexpected HT command]] noise.
AC_DEFUN([OPENAXIOM_CHECK_BROWSER_SUPPORT],[
openaxiom_host_has_regex=
AC_CHECK_HEADER([regex.h], 
		[openaxiom_host_has_regex=yes],
		[openaxiom_host_has_regex=no])
AC_SUBST(openaxiom_host_has_regex)
])

dnl ------------------------------
dnl -- OPENAXIOM_CHECK_GRAPHICS --
dnl ------------------------------
AC_DEFUN([OPENAXIOM_CHECK_GRAPHICS],[
OPENAXIOM_CHECK_X11
OPENAXIOM_CHECK_QT
OPENAXIOM_CHECK_BROWSER_SUPPORT
])


dnl ------------------------
dnl -- OPENAXIOM_CHECK_MM --
dnl ------------------------
dnl Check for host capability of memory mapping.
AC_DEFUN([OPENAXIOM_CHECK_MM],[
AC_CHECK_HEADERS([sys/mman.h fcntl.h])
## We want anonymous mapping for memory allocation.  Unfortunately,
## the flag for anonymous mapping is not standardized.  Popular names
##  are MAP_ANONYMOUS and MAP_ANON.
if test x"$ac_cv_header_sys_mman_h" = xyes; then
   AC_MSG_CHECKING([for flag to request anonymous memory mapping])
   AC_EGREP_CPP([MAP_ANONYMOUS],
                [#include <sys/mman.h>
#ifdef MAP_ANONYMOUS
   "MAP_ANONYMOUS"
#endif],
                [openaxiom_mm_anonymous_flag=MAP_ANONYMOUS])
   if test -z "$openaxiom_mm_anonymous_flag"; then
      AC_EGREP_CPP([MAP_ANON],
                   [#include <sys/mman.h>
#ifdef MAP_ANON
   "MAP_ANON"
#endif],
                   [openaxiom_mm_anonymous_flag=MAP_ANON])
   fi
   ## It would be curious that we don't have an anonymous mapping
   ## capability.  Let that be known loudly.
   if test -n "$openaxiom_mm_anonymous_flag"; then
      AC_MSG_RESULT([$openaxiom_mm_anonymous_flag])
   else
      AC_MSG_ERROR([Could not find flag for anonymous map])
   fi
   AC_DEFINE_UNQUOTED([OPENAXIOM_MM_ANONYMOUS_MAP_FLAG],
                      [$openaxiom_mm_anonymous_flag],
                      [mmap anonymous flag])
fi
])

dnl ----------------------------------
dnl -- OPENAXIOM_ALIGNMENT_OPERATOR --
dnl ----------------------------------
dnl Check that the C/C++ compiler understand
dnl alignment operator, i.e. either `alignof',
dnl or vendor lock-ins such as `__alignof'.
AC_DEFUN([OPENAXIOM_ALIGNMENT_OPERATOR],[
AC_MSG_CHECKING([name of alignment query operator])
oa_alignment=
AC_COMPILE_IFELSE([int a = __alignof(int);],
                  [oa_alignment="__alignof"],
                  [AC_COMPILE_IFELSE([int a = alignof(int);],
                                     [oa_alignment="alignof"],
                                     [AC_MSG_ERROR([C/C++ compiler does not support alignment query operator])])])
AC_DEFINE_UNQUOTED([openaxiom_alignment],[$oa_alignment],
                   [Alignment query operator])
AC_MSG_RESULT([$oa_alignment])
])

dnl -------------------------
dnl -- OPENAXIOM_CHECK_GMP --
dnl -------------------------
AC_DEFUN([OPENAXIOM_CHECK_GMP],[
AC_CHECK_HEADERS([gmp.h], [AC_CHECK_LIB([gmp],[__gmpz_init])])
AM_CONDITIONAL([OA_HAS_GMP], [test -n $ac_cv_header_gmp_h])
])

dnl --------------------------
dnl -- OPENAXIOM_CHECK_MISC --
dnl --------------------------
AC_DEFUN([OPENAXIOM_CHECK_MISC],[
OPENAXIOM_ALIGNMENT_OPERATOR
case $oa_gnu_compiler in
  yes)
     CFLAGS="$CFLAGS -O2 -Wall"
     CXXFLAGS="$CXXFLAGS -O2 -Wall"
     ;;
esac
])
