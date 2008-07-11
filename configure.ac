sinclude(config/open-axiom.m4)
sinclude(config/aclocal.m4)
AC_INIT([OpenAxiom], [1.2.0-2008-07-10], 
        [open-axiom-bugs@lists.sf.net])

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
AC_CONFIG_HEADERS([config/openaxiom-c-macros.h])
# AM_INIT_AUTOMAKE([foreign])
AC_PREREQ([2.60])

AC_CONFIG_SRCDIR(src/Makefile.pamphlet)

oa_use_libtool_for_shared_lib=yes
oa_shrobj_flags=
oa_shrlib_flags=
# Tell Libtool to assume `dlopen' so that it does not have to
# emulate it.
AC_LIBTOOL_DLOPEN
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEPS)
# Give me extension of libraries
eval shared_ext=\"$shrext_cmds\"
AC_SUBST(shared_ext)
AC_SUBST(libext)
# Don't use Libtool for building actual DLLs on MinGW and Cygwin
case $host in
    *mingw*|*cygwin*)
       oa_use_libtool_for_shared_lib=no
       oa_shrobj_flags='-prefer-pic'
       oa_shrlib_flags="-shared --export-all-symbols"
       ;;
    *darwin*)
       oa_shrobj_flags='-dynamic'
       oa_shrlib_flags='-dynamiclib -undefined suppress -flat_namespace'
       ;;
    *)
       oa_shrobj_flags='-prefer-pic'
       oa_shrlib_flags='-shared -module'
       ;;
esac
AC_SUBST(oa_use_libtool_for_shared_lib)
AC_SUBST(oa_shrobj_flags)
AC_SUBST(oa_shrlib_flags)

axiom_optimize_options=speed
## Shall we proclaim safety?
axiom_enable_checking=no          # don't turn on checking by default.
AC_ARG_ENABLE([checking], [  --enable-checking  turn runtime checking on],
              [case $enableval in
                  yes|no) axiom_enable_checking=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-checking]) ;;
               esac])
if test x"$axiom_enable_checking" = xyes; then
   axiom_optimize_options="$axiom_optimize_options safety"
   AC_MSG_NOTICE([runtime checking may increase compilation time])
fi
AC_SUBST(axiom_enable_checking)
AC_SUBST(axiom_optimize_options)

axiom_src_subdirs="lib hyper lisp boot interp share algebra input etc doc"
AC_SUBST(axiom_src_subdirs)

AC_CANONICAL_SYSTEM

open_axiom_installdir=$libdir/open-axiom/$target/$PACKAGE_VERSION
AC_SUBST(open_axiom_installdir)

if test $build != $target; then
   AC_MSG_WARN([Cross build is not supported.])
   AC_MSG_WARN([Please notify open-axiom-devel@open-axiom.org if you succeed.])
fi
## Where tools for the build platform are sequestered
axiom_build_sharedir=$axiom_builddir/share

## On Windows system, we prefer the default installation
## location to be 'C:/Program Files/OpenAxiom', following Windows 
## convention.  We cannot use AC_PREFIX_DEFAULT directly as it seems 
## to operate unconditionally.  Therefore, we resort to this dirty
## trick stepping over Autoconf's internals.
case $host in
    *mingw*)
        ac_default_prefix="C:/Program Files/OpenAxiom"
        ;;
    *)  ;;
esac

## Accumulate list of utils needed for the build platform
## It is vital that noweb is present in the build environement.
axiom_all_prerequisites=

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

## Make sure the C compiler is from GCC
AC_PROG_CC
if test x$GCC != xyes; then
    AC_MSG_ERROR([We need a C compiler from GCC])
fi
axiom_cflags="-O2 -Wall -D_GNU_SOURCE"
AC_SUBST(axiom_cflags)

## What is the extension of object files on this platform?
AC_OBJEXT

AC_DEFINE_UNQUOTED([OPENAXIOM_EXEEXT], ["$ac_cv_exeext"], 
                   [Extension of executable file.])


AC_PROG_INSTALL
# AC_PROG_LN_S
AC_CHECK_PROG([TOUCH], [touch],
              [touch], [AC_MSG_ERROR(['touch' program is missing.])])
AC_CHECK_PROGS([MKTEMP], [mktemp])

case $build in
     *-solaris9)
        AC_CHECK_PROG([AWK], [gawk], 
                      [gawk], [AC_MSG_ERROR([OpenAxiom needs gawk])])

        AC_CHECK_PROG([PATCH], [gpatch],
                      [gptach], [AC_MSG_ERROR([OpenAxiom needs gpatch])]) 
        ;;

      *)
        AC_PROG_AWK

        AC_CHECK_PROGS([PATCH], [gpatch patch], 
                       [AC_MSG_ERROR([OpenAxiom needs a patch program])])
        ;;
esac

AC_CHECK_PROG([AR], [ar], [ar], [AC_MSG_ERROR([program 'ar' is missing])])

AC_PATH_PROG([LATEX], [latex])
AC_CHECK_PROGS([MAKEINDEX], [makeindex])
if test -n "$LATEX"; then \
   :
else
    AC_MSG_NOTICE([Documentation is disabled.])
fi

## ---------------------------------------
## Make sure noweb executable is available
## ---------------------------------------
AC_CHECK_PROGS([NOTANGLE], [notangle])
AC_CHECK_PROGS([NOWEAVE], [noweave])

## In case noweb is missing we need to build our own.
if test -z $NOTANGLE -o -z $NOWEAVE ; then
    ## Yes, but do we have the source files to build from?
    if test ! -d ${srcdir}/noweb; then
       AC_MSG_NOTICE([OpenAxiom requires noweb utilties])
       AC_MSG_ERROR([Please get the tarball of dependencies and reconfigure])
    fi
    NOTANGLE='$(axiom_build_bindir)/notangle'
    NOWEAVE='$(axiom_build_bindir)/noweave'
    axiom_all_prerequisites="$axiom_all_prerequisites all-noweb"
fi

## ------------------------
## -- Which Lisp to use? --
## ------------------------
##
## We will default to GCL later, if no lisp implementation is specified.
axiom_lisp=
axiom_lisp_flavor=unknown
AC_ARG_WITH([lisp], [ --with-lisp=L         use L as Lisp platform],
              [axiom_lisp=$withval])
## If --enable-gcl is specified, we need to check for coonsistency
axiom_include_gcl=
if test -z $axiom_lisp; then
    AC_ARG_ENABLE([gcl], [  --enable-gcl   build GCL from OpenAxiom source],
                  [case $enableval in
                       yes|no) axiom_include_gcl=$enableval ;;
                       *) AC_MSG_ERROR([erroneous value for --enable-gcl]) ;;
                   esac])
fi
## We need to build our own GCL if none is avalaible, or the existing
## one is too old.
if test -z $axiom_lisp; then
    AC_CHECK_PROGS([AXIOM_LISP], [gcl sbcl clisp])
    ## A lisp may not be available AND the GCL source may also
    ## be missing.  Instruct user to either build one or get
    ## the dependencies from our website.
    if test x$AXIOM_LISP = xgcl; then
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
    fi
    if test -z $AXIOM_LISP && test ! -d ${srcdir}/gcl; then
       AC_MSG_ERROR([OpenAxiom requires a Lisp system.  Either separately build one (GCL-2.6.7, GCL-2.6.8, SBCL, CLisp), or get the dependency tarball from OpenAxiom download website.])
    fi
    axiom_lisp=$AXIOM_LISP
else
    ## Honor use of Lisp image specified on command line
    AXIOM_LISP=$axiom_lisp
    AC_SUBST(AXIOM_LISP)
    :
fi
## Coherence check for GCL inclusion.
case $axiom_include_gcl,$axiom_lisp in
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
       axiom_lisp_flavor=gcl
       axiom_fasl_type=o
       ;;
    yes,*)
       AC_MSG_ERROR([--with-lisp=$axiom_lisp conflicts with --enable-gcl])
       ;;
    *)
       ## As of this writting, the Lisp systems ECL, GCL, SBCL, and CLisp all
       ## understands the command line option --help, though they do
       ## behave very differently.  Some of them just print out the
       ## help information and exits.  Others, such as GCL, think they
       ## must enter a read-eval-print loop (which isn't very helpful).
       AC_MSG_CHECKING([which flavor of Lisp])
       what=`echo '(quit)' | $axiom_lisp --help`
       case $what in
           *GCL*) 
               axiom_lisp_flavor=gcl
               ;;
           *ecl*) 
               axiom_lisp_flavor=ecl 
               ;;
           *sbcl*) 
               axiom_lisp_flavor=sbcl 
               ;;
           *CLISP*)
               ## Not all variants of CLisp have FFI support.  FFI is used
               ## internally used by OpenAxiom in essential way.
               if ! $axiom_lisp -q -x '*features*' | grep ':FFI' 2>/dev/null
               then
                 AC_MSG_ERROR([$axiom_lisp does not support Foreign Function Interface.  Please upgrade to a better version of CLisp or install SBCL.])
               fi
               axiom_lisp_flavor=clisp
               ;;
       esac
       AC_MSG_RESULT([$axiom_lisp_flavor])
esac
AC_SUBST(axiom_include_gcl)
AC_SUBST(axiom_lisp_flavor)
AC_DEFINE_UNQUOTED([OPENAXIOM_BASE_RTS],
                   [openaxiom_${axiom_lisp_flavor}_runtime],
                   [The kind of base runtime system for this build.])

## The following is a horrible hack to arrange for GCL to successfully
## rebuild symbol tables with "rsym" on Windows platform.  It should
## go away as soon as GCL upstream is fixed.
case $axiom_lisp_flavor,$target in
    gcl,*mingw*)
        axiom_gcl_rsym_hack='d=`echo "(format nil \"~a\" si::*system-directory*)" | $(AXIOM_LISP) | grep "/gcl.*/" | sed -e "s,\",,g"`; cp $$d/rsym$(EXEEXT) .'
        ;;
    *) 
        ## Breath.
        axiom_gcl_rsym_hack=':'
        ;;
esac
AC_SUBST(axiom_gcl_rsym_hack)
## How are we supposed to tell the Lisp system to eval an expression
## in batch mode?  What is the extension of a compiled Lisp file?
case $axiom_lisp_flavor in
    gcl) 
       axiom_quiet_flags='-batch'
       axiom_eval_flags='-eval'
       ;;
    ecl) 
       axiom_quiet_flags=
       axiom_eval_flags='-eval'
       ;;
    sbcl) 
       axiom_quiet_flags='--noinform --noprint'
       axiom_eval_flags='--eval'
       ;;
    clisp) 
       axiom_quiet_flags='--quiet'
       axiom_eval_flags='-x'
       ;;
    *) AC_MSG_ERROR([We do not know how to build OpenAxiom this $axiom_lisp]) ;;
esac
AC_SUBST(axiom_quiet_flags)
AC_SUBST(axiom_eval_flags)
if test -z $axiom_fasl_type; then
    AC_MSG_CHECKING([compiled Lisp file extension])
    ## We set the IFS to <space> as we don't want automatic
    ## replacement of <newline> by <space>.
    axiom_save_IFS=$IFS
    IFS=' '
    axiom_fasl_type=`$axiom_lisp $axiom_quiet_flags $axiom_eval_flags '(progn (format t "axiom_fasl_type=~a" (pathname-type (compile-file-pathname "foo.lisp" #+:ecl :type #+:ecl :object))) (quit))'`

    ## Now pull out the fasl type.  ECL has the habit of spitting noise
    ## about internal loading.  Therefore, we must look only for a line that
    ## begins with axiom_fasl_type.
    axiom_fasl_type=`echo $axiom_fasl_type | grep '^axiom_fasl_type'`
    IFS=$axiom_save_IFS
    axiom_fasl_type=`echo $axiom_fasl_type | sed -e 's/axiom_fasl_type=//'`
    if test -z $axiom_fasl_type; then
        AC_MSG_ERROR([Could not determine extension for compiled Lisp files])
    fi
    AC_MSG_RESULT([$axiom_fasl_type])
fi
AC_SUBST(axiom_fasl_type)

AC_SUBST(axiom_all_prerequisites)

# FIXME: Move this out of here.  
axiom_c_runtime=
AC_SUBST(axiom_c_runtime)

AC_CHECK_HEADERS([stdint.h inttypes.h])
AC_TYPE_UINT8_T
AC_CHECK_HEADERS([signal.h],
                 [],
                 [AC_MSG_ERROR([OpenAxiom needs signal support.])])
AC_CHECK_DECLS([sigaction], [], [], 
               [#include <signal.h>])
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
AC_CHECK_DECLS([getuid, geteuid, getgid, getegid], [], [],
               [#include <unistd.h>])

AC_CHECK_DECLS([kill], [], [],
               [#include <signal.h>])
case $host in
    *mingw*)
        AC_CHECK_HEADERS([winsock2.h],
                        [axiom_host_has_socket=yes],
                        [])
        axiom_c_runtime_extra="-lwsock32"
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
           [axiom_c_runtime_extra="-lsocket"],
           [AC_MSG_ERROR([socket library not found])])
       ;;
    *) ;;
esac

AC_SUBST(axiom_c_runtime_extra)

AC_EGREP_HEADER([has_af_local],
                [#if HAVE_SYS_SOCKET_H
                 #  include <sys/socket.h>
                 #else
                 #  include <winsock2.h>
                 #endif
                 #ifdef AF_LOCAL
                   has_af_local
                 #endif],
                 [AC_DEFINE([HAVE_AF_LOCAL], [1], [Host has AF_LOCAL])])


AC_EGREP_HEADER([has_af_unix],
                [#if HAVE_SYS_SOCKET_H
                 #  include <sys/socket.h>
                 #else
                 #  include <winsock2.h>
                 #endif
                 #ifdef AF_UNIX
                   has_af_unix
                 #endif],
                 [AC_DEFINE([HAVE_AF_UNIX], [1], [Host has AF_UNIX])])

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

axiom_use_sman=1
if test x"$ac_cv_have_decl_fork" = xyes \
     -a x"$ac_cv_have_decl_wait" = xyes; then \
    axiom_c_runtime="$axiom_c_runtime terminal_io"
    axiom_src_all="$axiom_src_all all-sman all-clef"
    axiom_src_subdirs="$axiom_src_subdirs clef sman"
    AXIOM_MAKEFILE([src/clef/Makefile])
    AXIOM_MAKEFILE([src/sman/Makefile])
else
    axiom_use_sman=0
    AC_MSG_NOTICE([Superman component is disabled.])
fi

AC_DEFINE_UNQUOTED([OPENAXIOM_USE_SMAN], [$axiom_use_sman],
                   [Whether to use the session manager as driver.])

axiom_src_all="all-input $axiom_src_all"

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
if test -z $no_x; then
    axiom_use_x=yes
    axiom_c_runtime="$axiom_c_runtime graphics"
    axiom_src_all="$axiom_src_all all-graph"
    axiom_src_subdirs="$axiom_src_subdirs graph"
    AXIOM_MAKEFILE([src/graph/Makefile])
    AXIOM_MAKEFILE([src/graph/Gdraws/Makefile])
    AXIOM_MAKEFILE([src/graph/view2D/Makefile])
    AXIOM_MAKEFILE([src/graph/view3D/Makefile])
    AXIOM_MAKEFILE([src/graph/viewAlone/Makefile])
    AXIOM_MAKEFILE([src/graph/viewman/Makefile])
else
    AC_MSG_NOTICE([The Garphics component is disabled.])
fi
AC_SUBST(axiom_src_all)
AC_SUBST(axiom_use_x)
openaxiom_host_has_regex=
AC_CHECK_HEADER([regex.h], 
                [openaxiom_host_has_regex=yes],
                [openaxiom_host_has_regex=no])
AC_SUBST(openaxiom_host_has_regex)

axiom_host_has_libbfd=
## Check for these only if we are going to build GCL from source.
case $axiom_all_prerequisites in
    *all-gcl*)
        AC_CHECK_HEADER([bfd.h])
        AC_HAVE_LIBRARY([bfd], [axiom_host_has_libbfd=yes])

        axiom_gcl_bfd_option=
        if test x"$ac_cv_header_bfd_h" = xyes \
            && test x"$axiom_host_has_libbfd" = xyes; then
            axiom_gcl_bfd_option="--disable-dynsysbfd"
        else
            axiom_gcl_bfd_option="--disable-statsysbfd --enable-locbfd"
        fi
        ;;
    *)    
        # Nothing to worry about
        ;;
esac
# axiom_gcl_mm_option="--enable-maxpage=256*1024"
axiom_gcl_x_option="--disable-tkconfig --disable-x --disable-xgcl"
axiom_gcl_emacs="--enable-emacs=correct"

PFL=
CCF="-O2 -Wall -D_GNU_SOURCE"
LDF=
LISP=lsp

case $target in
    *bsd*)
        AC_DEFINE([BSDplatform], [], [BSD flavour])
        CCF="-O2 -Wall -D_GNU_SOURCE -I/usr/local/include"
        LDF="-L/usr/local/lib"
        ;;
    windows)
        AC_DEFINE([MSYSplatform], [], [MS])
        SRCDIRS=bootdir interpdir sharedir algebradir etcdir docdir inputdir
        ;;
    *linux*)
        AC_DEFINE([LINUXplatform], [], [Linux flavour])
        ;;
    *solaris*)
        AC_DEFINE([SUNplatform], [], [SunOS flavour])
        ;;
    powerpc*darwin*)
        AC_DEFINE([MACOSXplatform], [], [MACOX flavour])
        CCF="-O2 -Wall -D_GNU_SOURCE \
            -I/usr/include -I/usr/include/sys"
        axiom_gcl_bfd_option="--disable-statsysbfd \
                                --enable-machine=powerpc-macosx"
        axiom_gcl_mm_option="--enable-vssize=65536*2"
        ;;
esac

GCLOPTS="$axiom_gcl_emacs $axiom_gcl_bfd_option $axiom_gcl_mm_option $axiom_gcl_x_option"

AC_SUBST(PLF)
AC_SUBST(CCF)
AC_SUBST(LDF)
AC_SUBST(LISP)
AC_SUBST(GCLOPTS)
case $axiom_lisp_flavor in
   gcl)
      void_type='void'
      char_type='char'
      int_type='int'
      float_type='float'
      double_type='double'
      string_type='string'
      pointer_type='object'
      ;;
   sbcl)
      void_type='sb-alien:void'
      char_type='sb-alien:char'
      int_type='sb-alien:int'
      float_type='sb-alien:float'
      double_type='sb-alien:double'
      string_type='sb-alien:c-string'
      pointer_type='sb-alien:system-area-pointer'
      ;;
   clisp)
      void_type='nil'
      char_type='ffi:character'
      int_type='ffi:int'
      float_type='ffi:single-float'
      double_type='ffi:double-float'
      string_type='ffi:c-string'
      pointer_type='ffi:c-pointer'
      ;;
   ecl)
      void_type=':void'
      char_type=':char'
      int_type=':int'
      float_type=':float'
      double_type=':double'
      string_type=':cstring'
      pointer_type=':pointer-void'
      ;;
   *)
      AC_MSG_ERROR([We do not know how to translate native types for this Lisp])
      ;;
esac
AC_SUBST(void_type)
AC_SUBST(char_type)
AC_SUBST(int_type)
AC_SUBST(float_type)
AC_SUBST(double_type)
AC_SUBST(string_type)
AC_SUBST(pointer_type)

AXIOM_MAKEFILE([Makefile])
AXIOM_MAKEFILE([src/Makefile])
AXIOM_MAKEFILE([src/lib/Makefile])
AXIOM_MAKEFILE([src/hyper/Makefile])
AXIOM_MAKEFILE([src/driver/Makefile])
AXIOM_MAKEFILE([src/lisp/Makefile])
AXIOM_MAKEFILE([src/boot/Makefile])
AXIOM_MAKEFILE([src/interp/Makefile])
AXIOM_MAKEFILE([src/share/Makefile])
AXIOM_MAKEFILE([src/algebra/Makefile])
AXIOM_MAKEFILE([src/input/Makefile])
AXIOM_MAKEFILE([src/etc/Makefile])
AXIOM_MAKEFILE([src/doc/Makefile])


## We now generate the "document" script and support files at configure time.
## We put them in the build directory because they are intended to be 
## build support utils only.
AC_CONFIG_FILES(build/scripts/document:$srcdir/src/scripts/document.in, \
                [chmod +x build/scripts/document])

AC_OUTPUT

## Generate rules for Algebra bootstrap files.
echo -n "building list of Algebra bootstrap files..."
egrep ".*BOOTSTRAP>>=" $srcdir/src/algebra/*.spad.pamphlet \
    | sort | uniq | \
  while IFS=':' read spad_file chunk_desc; do
      chunk_desc=`echo $chunk_desc | sed -e 's,<<,,' -e 's,>>=,,'`
      set $chunk_desc; bootstrap_file=$1
      cat >> src/algebra/tmp-extract-lisp-files.mk <<EOF
$bootstrap_file: \$(srcdir)/`basename $spad_file`; \
    \$(EXTRACT_BOOTSTRAP_FILE)
EOF
  done 
echo done
$srcdir/config/move-if-change \
    src/algebra/tmp-extract-lisp-files.mk \
    src/algebra/extract-lisp-files.mk

## Generate rules to extrad SPAD type definitions from pamphlets.
echo -n "extracting list of SPAD type definitions..."
egrep '<<(category|domain|package) .*>>=' \
    $srcdir/src/algebra/*.spad.pamphlet \
    | sort | uniq | \
    while IFS=':' read spad_file chunk_desc; do 
        chunk_desc=`echo $chunk_desc | sed -e 's,<<,,' -e 's,>>=,,'`
        set $chunk_desc; spad_abbrev=$2
        cat >> src/algebra/tmp-extract-spad.mk <<EOF
$spad_abbrev.spad: \$(srcdir)/`basename $spad_file` ; \
    @\$(axiom_build_document) --output=\$@ --tangle="$chunk_desc" \$<
EOF
    done 
echo done
$srcdir/config/move-if-change \
    src/algebra/tmp-extract-spad.mk src/algebra/extract-spad.mk

echo "Type '${MAKE}' (without quotes) to build OpenAxiom"
