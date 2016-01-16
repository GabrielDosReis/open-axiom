dnl Copyright (C) 2006-2016, Gabriel Dos Reis.
dnl All rights reserved.
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions are
dnl met:
dnl
dnl     - Redistributions of source code must retain the above copyright
dnl       notice, this list of conditions and the following disclaimer.
dnl
dnl     - Redistributions in binary form must reproduce the above copyright
dnl       notice, this list of conditions and the following disclaimer in
dnl       the documentation and/or other materials provided with the
dnl       distribution.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
dnl IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
dnl TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
dnl PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
dnl OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
dnl EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
dnl PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
dnl PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
dnl LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
dnl NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
dnl SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

dnl
dnl  This configure template details the configuration process of setting up 
dnl  OpenAxiom for build from source codes.  The  process scrutinizes the 
dnl  build, host,  and target environments, and finally instantiates 
dnl  Makefiles for building OpenAxiom interpreter, compiler, libraries, and
dnl  auxiliary tools where appropriate.
dnl

AC_INIT([OpenAxiom], [1.5.0-2016-01-16], 
        [open-axiom-bugs@lists.sf.net])

dnl Most of the macros used in this configure.ac are defined in files
dnl located in the subdirectory config/
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config])

AH_TOP([#ifndef OPENAXIOM_CONFIG_included]
[#define OPENAXIOM_CONFIG_included])
AH_BOTTOM([#endif // OPENAXIOM_CONFIG_included])

dnl Put all configuration macros here
AC_CONFIG_HEADERS([config/openaxiom-c-macros.h])

OPENAXIOM_CANONICAL_SYSTEM

dnl Page Automake
AM_INIT_AUTOMAKE([foreign subdir-objects])
AC_PREREQ([2.62])

dnl Simple sanity check.
AC_CONFIG_SRCDIR(src/Makefile.in)


open_axiom_installdir=$libdir/open-axiom/$target/$PACKAGE_VERSION
AC_SUBST(open_axiom_installdir)

dnl For the moment, the OpenAxiom base source code is written
dnl in a way that does not support cross build.  However, we do
dnl want to make cross build possible; consequently we issue
dnl a warning when we detect attempt at cross build.
dnl Note that we do not stop the configuration process because we do seek
dnl contributions for cross build support.
if test $build != $target; then
   AC_MSG_WARN([Cross build is not supported yet.])
   AC_MSG_WARN([Please notify open-axiom-devel@lists.sf.net if you succeed.])
fi

## Determine interpreters and shells.
OPENAXIOM_SHELLS

## Although OpenAxiom does not support cross build yet, let
## alone Canadian cross, we want to make sure that we do not write
## the build machinery in a way that actively unsupports 
## cross build.  Consequently, in the build tree, we sequester
## tools that we build and use on the build platform,
## in  sub-directories different from others.
oa_build_sharedir=$oa_builddir/share

## We have now almost switched entirely to C++.
AC_LANG([C++])

OPENAXIOM_HOST_COMPILERS
AM_CONDITIONAL([OA_USE_LLVM], [test -n $oa_use_llvm])
OPENAXIOM_REQUIRE_CXX11
OPENAXIOM_EXTRA_CXX_OPTIONS
AM_CONDITIONAL([OA_BUILD_GCL], [test x$oa_include_gcl = xyes])
AM_CONDITIONAL([OA_ECL_RT], [test x$oa_lisp_flavor = xecl])
AM_CONDITIONAL([OA_DELAYED_FFI], [test x$oa_delay_ffi = xyes])

OPENAXIOM_HOST_DATA_PROPERTIES

OPENAXIOM_DYNAMIC_MODULE_SUPPORT
OPENAXIOM_BUILD_TOOLS
OPENAXIOM_LISP_FLAGS
OPENAXIOM_FILE_EXTENSIONS

OPENAXIOM_BUILD_OPTIONS
OPENAXIOM_HOST_PROGS

OPENAXIOM_CHECK_CORE_SUPPORT
OPENAXIOM_CHECK_IO
AM_CONDITIONAL([OA_BUILD_SMAN], [test $oa_use_sman = 1])
OPENAXIOM_CHECK_GRAPHICS
AM_CONDITIONAL([OA_BUILD_GRAPHICS], [test x$oa_use_x = xyes])
AM_CONDITIONAL([OA_BUILD_GUI], [test x$oa_has_qt = xyes])

OPENAXIOM_FFI_TYPE_TABLE
OPENAXIOM_GCL_BUILD_OPTIONS
OPENAXIOM_CHECK_MM
OPENAXIOM_CHECK_MISC

## We are ready to instantiate makefiles.
AC_CONFIG_FILES([
  Makefile
  src/Makefile
  src/utils/Makefile
  src/rt/Makefile
  src/boot/Makefile
  src/algebra/Makefile])
OPENAXIOM_MAKEFILE([src/lib/Makefile])
OPENAXIOM_MAKEFILE([src/hyper/Makefile])
OPENAXIOM_MAKEFILE([src/driver/Makefile])
OPENAXIOM_MAKEFILE([src/lisp/Makefile])
OPENAXIOM_MAKEFILE([src/interp/Makefile])
OPENAXIOM_MAKEFILE([src/share/Makefile])
OPENAXIOM_MAKEFILE([src/input/Makefile])
OPENAXIOM_MAKEFILE([src/etc/Makefile])
OPENAXIOM_MAKEFILE([src/doc/Makefile])
OPENAXIOM_MAKEFILE([src/clef/Makefile])
OPENAXIOM_MAKEFILE([src/sman/Makefile])
OPENAXIOM_MAKEFILE([src/graph/Makefile])
OPENAXIOM_MAKEFILE([src/graph/Gdraws/Makefile])
OPENAXIOM_MAKEFILE([src/graph/view2D/Makefile])
OPENAXIOM_MAKEFILE([src/graph/view3D/Makefile])
OPENAXIOM_MAKEFILE([src/graph/viewAlone/Makefile])
OPENAXIOM_MAKEFILE([src/graph/viewman/Makefile])
AC_CONFIG_FILES([src/gui/gui.pro])

AC_CONFIG_LINKS([${target}/include/open-axiom/config:config/openaxiom-c-macros.h],
  [],
  [target=$target])

AC_OUTPUT

## Set up Makefile for QT-based gui
if test x$oa_has_qt = xyes; then
  oa_qmake_spec=
  case $host,$GXX in
     *darwin*,yes)
         oa_qmake_spec="-spec macx-g++"
         ;;
  esac
  (cd src/gui && $OA_QT_QMAKE -makefile $oa_qmake_spec gui.pro)
fi

## Generate rules to extrad SPAD type definitions from pamphlets.
echo -n "extracting list of SPAD type definitions..."
egrep '<<(category|domain|package) .*>>=' \
    $srcdir/src/algebra/*.spad.pamphlet \
    | sort | uniq | \
    while IFS=':' read spad_file chunk_desc; do 
        chunk_desc=`echo $chunk_desc | sed -e 's,<<,,' -e 's,>>=,,'`
        set $chunk_desc; spad_abbrev=$2
        cat >> src/algebra/tmp-extract.mk <<EOF
$spad_abbrev.spad: \$(srcdir)/`basename $spad_file` ; \
    @\$(oa_hammer) --output=\$@.tmp --tangle="$chunk_desc" \$< && \
    \$(top_confdir)/move-if-change \$@.tmp \$@
EOF
    done 
echo done
$srcdir/config/move-if-change \
    src/algebra/tmp-extract.mk src/algebra/extract.mk


echo "Type '${MAKE}' (without quotes) to build OpenAxiom"

dnl That is all folks.
