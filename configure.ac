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

OPENAXIOM_FFI_TYPE_TABLE
OPENAXIOM_GCL_BUILD_OPTIONS
OPENAXIOM_CHECK_MISC

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
