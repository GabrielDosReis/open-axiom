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
## host Lisp system flavor
axiom_lisp_flavor=unknown
AC_SUBST(axiom_lisp_flavor)
 
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
