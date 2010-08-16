sinclude(config/open-axiom.m4)
sinclude(config/aclocal.m4)
AC_INIT([OpenAxiom], [1.4.0-2010-08-07], 
        [open-axiom-bugs@lists.sf.net])

AC_CONFIG_AUX_DIR(config)
AC_CONFIG_MACRO_DIR(config)
AC_CONFIG_HEADERS([config/openaxiom-c-macros.h])
# AM_INIT_AUTOMAKE([foreign])
AC_PREREQ([2.62])
AC_CONFIG_SRCDIR(src/Makefile.pamphlet)
AC_CANONICAL_SYSTEM

open_axiom_installdir=$libdir/open-axiom/$target/$PACKAGE_VERSION
AC_SUBST(open_axiom_installdir)

if test $build != $target; then
   AC_MSG_WARN([Cross build is not supported.])
   AC_MSG_WARN([Please notify open-axiom-devel@open-axiom.org if you succeed.])
fi
## Where tools for the build platform are sequestered
axiom_build_sharedir=$axiom_builddir/share


## Accumulate list of utils needed for the build platform
## It is vital that noweb is present in the build environement.
oa_all_prerequisites=
AC_SUBST(oa_all_prerequisites)

OPENAXIOM_HOST_COMPILERS
OPENAXIOM_GCL_HACKS
OPENAXIOM_HOST_DATA_PROPERTIES
OPENAXIOM_DYNAMIC_MODULE_SUPPORT

OPENAXIOM_BUILD_TOOLS

axiom_src_subdirs="lib hyper lisp boot interp share algebra input etc doc"
AC_SUBST(axiom_src_subdirs)

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
AC_SUBST(oa_editor)

AC_PATH_PROGS([HOST_AWK],[awk nawk gawk mawk])


AC_CHECK_PROG([AR], [ar], [ar], [AC_MSG_ERROR([program 'ar' is missing])])

AC_PATH_PROG([PDFLATEX], [pdflatex])
if test -z "$PDFLATEX"; then
   AC_PATH_PROG([LATEX], [latex],
                [AC_MSG_NOTICE([Documentation is disabled.])])
fi

AC_CHECK_PROGS([MAKEINDEX], [makeindex])

OPENAXIOM_LISP_FLAGS

OPENAXIOM_FILE_EXTENSIONS

## Occaionally, we may want to keep intermediary files.
oa_keep_files=
AC_ARG_ENABLE([int-file-retention], 
              [  --enable-int-file-retention   keep intermediary files],
              [case $enableval in
                  yes|no) oa_keep_files=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-int-file-retention]) ;;
               esac])
AC_SUBST(oa_keep_files)

## Does it make sense to pretend that we support multithreading?
oa_enable_threads=no
AC_ARG_ENABLE([threads], [  --enable-threads   turn on threads support],
              [case $enableval in
                  yes|no) oa_enable_threads=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-threads]) ;;
               esac])

# GNU compiler wants to have a hint about multithreading.
case $GCC,$oa_enable_threads in
   yes,yes)
     axiom_cflags="$axiom_cflags -pthread"
esac

AC_SUBST(oa_enable_threads)

axiom_optimize_options=speed
## Shall we proclaim safety?
axiom_enable_checking=no          # don't turn on checking by default.
AC_ARG_ENABLE([checking], [  --enable-checking  turn runtime checking on],
              [case $enableval in
                  yes|no) axiom_enable_checking=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-checking]) ;;
               esac])
if test x"$axiom_enable_checking" = xyes; then
   case $axiom_lisp_flavor in
     gcl) # GCL-2.6.x does not understand debug.
        axiom_optimize_options="$axiom_optimize_options safety" 
        ;;
     *) axiom_optimize_options="$axiom_optimize_options safety debug" 
        ;;
   esac
   AC_MSG_NOTICE([runtime checking may increase compilation time])
fi

AC_SUBST(axiom_enable_checking)
AC_SUBST(axiom_optimize_options)

## Parse args for profiling-enabled build.
oa_enable_profiling=no
AC_ARG_ENABLE([profiling], [  --enable-profiling  turn profiling on],
              [case $enableval in
                  yes|no) oa_enable_profiling=$enableval ;;
                  *) AC_MSG_ERROR([erroneous value for --enable-profiling]) ;;
               esac])

AC_SUBST(oa_enable_profiling)

# FIXME: Move this out of here.  
axiom_c_runtime=
AC_SUBST(axiom_c_runtime)

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
           [], [AC_MSG_ERROR([socket library not found])])
       AC_SEARCH_LIBS([gethostbyname], [nsl])
       ;;
    *) ;;
esac

AC_SUBST(axiom_c_runtime_extra)

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

axiom_use_sman=1
if test x"$ac_cv_have_decl_fork" = xyes \
     -a x"$ac_cv_have_decl_wait" = xyes; then \
    axiom_c_runtime="$axiom_c_runtime terminal_io"
    axiom_src_all="$axiom_src_all all-sman all-clef"
    axiom_src_subdirs="$axiom_src_subdirs clef sman"
    OPENAXIOM_MAKEFILE([src/clef/Makefile])
    OPENAXIOM_MAKEFILE([src/sman/Makefile])
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

# Check for Qt utilities.
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
  esac
fi

axiom_use_x=no
if test -z $no_x; then
    axiom_use_x=yes
    axiom_c_runtime="$axiom_c_runtime graphics"
    axiom_src_all="$axiom_src_all all-graph"
    axiom_src_subdirs="$axiom_src_subdirs graph"
    OPENAXIOM_MAKEFILE([src/graph/Makefile])
    OPENAXIOM_MAKEFILE([src/graph/Gdraws/Makefile])
    OPENAXIOM_MAKEFILE([src/graph/view2D/Makefile])
    OPENAXIOM_MAKEFILE([src/graph/view3D/Makefile])
    OPENAXIOM_MAKEFILE([src/graph/viewAlone/Makefile])
    OPENAXIOM_MAKEFILE([src/graph/viewman/Makefile])
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
case $oa_all_prerequisites in
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

case $GCC in
  yes)
     CCF="-O2 -Wall -D_GNU_SOURCE"
     ;;
esac

case $target in
    *bsd*|*dragonfly*)
        CCF="-O2 -Wall"
        ;;
    windows)
        SRCDIRS=bootdir interpdir sharedir algebradir etcdir docdir inputdir
        ;;
    *solaris*)
        AC_DEFINE([SUNplatform], [], [SunOS flavour])
        ;;
    powerpc*darwin*)
        CCF="-O2 -Wall -D_GNU_SOURCE \
            -I/usr/include -I/usr/include/sys"
        axiom_gcl_bfd_option="--disable-statsysbfd \
                                --enable-machine=powerpc-macosx"
        axiom_gcl_mm_option="--enable-vssize=65536*2"
        ;;
esac

GCLOPTS="$axiom_gcl_emacs $axiom_gcl_bfd_option $axiom_gcl_mm_option $axiom_gcl_x_option"

AC_SUBST(CCF)
AC_SUBST(GCLOPTS)
OPENAXIOM_FFI_TYPE_TABLE
OPENAXIOM_MAKEFILE([Makefile])
OPENAXIOM_MAKEFILE([src/Makefile])
OPENAXIOM_MAKEFILE([src/lib/Makefile])
OPENAXIOM_MAKEFILE([src/hyper/Makefile])
OPENAXIOM_MAKEFILE([src/driver/Makefile])
OPENAXIOM_MAKEFILE([src/lisp/Makefile])
OPENAXIOM_MAKEFILE([src/boot/Makefile])
OPENAXIOM_MAKEFILE([src/interp/Makefile])
OPENAXIOM_MAKEFILE([src/share/Makefile])
OPENAXIOM_MAKEFILE([src/algebra/Makefile])
OPENAXIOM_MAKEFILE([src/input/Makefile])
OPENAXIOM_MAKEFILE([src/etc/Makefile])
OPENAXIOM_MAKEFILE([src/doc/Makefile])

AC_CONFIG_FILES([src/hyper/presea], [chmod +x src/hyper/presea])


## We now generate the "document" script and support files at configure time.
## We put them in the build directory because they are intended to be 
## build support utils only.
AC_CONFIG_FILES(build/scripts/document:$srcdir/src/scripts/document.in, \
                [chmod +x build/scripts/document])

AC_OUTPUT

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
    @\$(axiom_build_document) --output=\$@.tmp --tangle="$chunk_desc" \$< && \
    \$(top_confdir)/move-if-change \$@.tmp \$@
EOF
    done 
echo done
$srcdir/config/move-if-change \
    src/algebra/tmp-extract-spad.mk src/algebra/extract-spad.mk

echo "Type '${MAKE}' (without quotes) to build OpenAxiom"
