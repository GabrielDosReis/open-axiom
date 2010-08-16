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
OPENAXIOM_LISP_FLAGS
OPENAXIOM_FILE_EXTENSIONS

OPENAXIOM_BUILD_OPTIONS
OPENAXIOM_HOST_PROGS

axiom_src_subdirs="lib hyper lisp boot interp share algebra input etc doc"
AC_SUBST(axiom_src_subdirs)

OPENAXIOM_CHECK_CORE_SUPPORT
OPENAXIOM_CHECK_IO
OPENAXIOM_CHECK_GRAPHICS

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
