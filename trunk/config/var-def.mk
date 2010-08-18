## ---------------------------------
## -- Standard Autoconf variables --
## ---------------------------------

SHELL = @SHELL@
VPATH = @srcdir@

prefix = @prefix@
exec_prefix = @exec_prefix@
bindir = @bindir@
sbindir = @sbindir@
libexecdir = @libexecdir@
datarootdir = @datarootdir@
datadir = @datadir@
sysconfdir = @sysconfdir@
sharedstatedir = @sharedstatedir@
localstatedir = @localstatedir@
includedir = @includedir@
oldincludedir = @oldincludedir@
docdir = @docdir@
infodir = @infodir@
htmldir = @htmldir@
dvidir = @dvidir@
pdfdir = @pdfdir@
psdir = @psdir@
libdir = @libdir@
lispdir = @lispdir@
localedir = @localedir@

host = @host@
host_alias = @host_alias@
build = @build@
build_alias = @build_alias@
target = @target@
target_alias = @target_alias@

srcdir = @srcdir@
abs_srcdir = @abs_srcdir@
top_srcdir = @top_srcdir@
abs_top_srcdir = @abs_top_srcdir@
top_confdir = $(top_srcdir)/config

# Notice that there is a bug in Autoconf 2.59 whereby the variable
# top_builddir is not defined.  So avoid to use it directly.  Use
# abs_top_builddir.
builddir = @builddir@
abs_builddir = @abs_builddir@
top_builddir = @top_builddir@
abs_top_builddir = @abs_top_builddir@
datadir = @datadir@

PACKAGE_VERSION = @PACKAGE_VERSION@

AR = @AR@
CC = @CC@
CPPFLAGS = @CPPFLAGS@
CFLAGS = @CFLAGS@
CXXFLAGS = @CXXFLAGS@
LDFLAGS = @LDFLAGS@
OBJEXT = @OBJEXT@
EXEEXT = @EXEEXT@
# this includes leading period
SHREXT = @shared_ext@
# extension of the archive library; this does not include period
LIBEXT = @libext@

PACKAGE_STRING = @PACKAGE_STRING@

LIBTOOL_DEPS = @LIBTOOL_DEPS@
LIBTOOL = $(top_builddir)/libtool

## Command used to compile a C program 
COMPILE = $(LIBTOOL) --tag=CC --mode=compile $(CC) -c $(CPPFLAGS)
CXXCOMPILE = $(LIBTOOL) --tag=CXX --mode=compile $(CXX) -c $(CPPFLAGS)

## Sadly, at the moment, the C parts of the OpenAxiom system is not
## well structured enough to allow for clean dynamic libraries
## and dynamic linking.  So, we build static programs.
## This situation is to be fixed when I have time.
LINK = $(LIBTOOL) --tag=CC --mode=link $(CC) -static $(LDFLAGS)
CXXLINK = $(LIBTOOL) --tag=CXX --mode=link $(CXX) -static $(LDFLAGS)

## Libtool is a disaster for building DLLs on Cygwin, and insists
## on adding silly extensions where it should not on MinGW, so we have
## to be very selective about when and where to use.  Sadly, that ends
## up negating the whole point of having Libtool in the first place.
ifeq (@oa_use_libtool_for_shared_lib@,no)
LINK_SHRLIB = $(CC) $(LDFLAGS)
CXXLINK_SHRLIB = $(CXX) $(LDFLAGS)
else
LINK_SHRLIB = $(LIBTOOL) --tag=CC --mode=link $(CC) $(LDFLAGS)
CXXLINK_SHRLIB = $(LIBTOOL) --tag=CXX --mode=link $(CC) $(LDFLAGS)
endif


oa_shrobj_flags = @oa_shrobj_flags@
oa_shrlib_flags = @oa_shrlib_flags@

AUTOCONF = autoconf
AWK = @AWK@
INSTALL = @INSTALL@
install_sh = @install_sh@
install_sh_DATA = $(install_sh) -c -m 644
install_sh_PROGRAM = $(install_sh) -c
install_sh_script = $(install_sh) -c
INSTALL_DATA = @INSTALL_DATA@
INSTALL_PROGRAM = @INSTALL_PROGRAM@
INSTALL_SCRIPT = @INSTALL_SCRIPT@
## FIXME:  The test done with Autoconf-2.60 and later concludes
##         that "ln -s" is unusable on msys, and therefore defaults to
##         "cp -p", but that default is unusable for us.  For our
##         purpose "ln -s" is just fine on that platform.  Consequently
##         we are explicitly overrding that value here.
LN_S = ln -s
mkinstalldirs = $(top_srcdir)/config/mkinstalldirs
PATCH = @PATCH@
RANLIB = @RANLIB@
TOUCH = @TOUCH@

# The list of make targets made recursively, by walking sub-directories
# Normally, this is a template generated by Automake, but we don't
# use Automake yet; so, we must do it the hard way.  
# See the corresponding rules in setup-dep.mk
RECURSIVE_TARGETS = all-recursive check-recursive dvi-recursive \
		html-recursive info-recursive install-data-recursive \
		install-exec-recursive install-info-recursive \
		install-recursive installcheck-recursive \
		installdirs-recursive pdf-recursive ps-recursive \
		uninstall-info-recursive uninstall-recursive


## Normally, Automake will substitute the value for this variable.
## But, we don't have Automake yet.  So we punt on it.
mkdir_p = mkdir -p

STAMP = echo timestamp >

## -------------------------
## -- OpenAxiom variables --
## -------------------------

quiet_flags = @axiom_quiet_flags@
eval_flags = @axiom_eval_flags@
oa_standard_linking = @oa_standard_linking@


## Absolute path to the toplevel source directory for OpenAxiom.  This is
## almost like Autoconf-standard abs_top_srcdir except that, it retains 
## the same syntactic values in subdirectories.

## Where The OpenAxiom distribution main source files are kept.
## Notice, this is the src/ directory within the toplevel source
## directory 

axiom_src_srcdir = $(top_srcdir)/src
axiom_src_docdir = $(axiom_src_srcdir)/doc
axiom_src_datadir = $(axiom_src_srcdir)/share
axiom_src_algdir = $(axiom_src_srcdir)/algebra
axiom_src_texdir =  $(axiom_src_datadir)/tex

## Where tools for the build machine are built
# Tools that we occasionally build don't know
# much about Autoconf and related infrastructure.  Therefore
# we do lot by "hand". For the moment, things work if we specify
# paths as absolute, as opposed to relative.  Other parts of
# OpenAxiom also expect absolute paths.
axiom_abs_top_builddir = $(abs_top_builddir)
axiom_abs_builddir = $(abs_top_builddir)/build/$(build)
axiom_abs_build_bindir = $(axiom_abs_builddir)/bin
axiom_abs_build_libdir = $(axiom_abs_builddir)/lib
axiom_abs_build_mandir = $(axiom_abs_builddir)/man
axiom_abs_build_datadir = $(axiom_abs_builddir)/share
axiom_abs_build_texdir = $(axiom_abs_build_datadir)/texmf/tex

axiom_top_builddir = $(top_builddir)/build
axiom_builddir = $(axiom_top_builddir)/$(build)
axiom_build_bindir = $(axiom_builddir)/bin
axiom_build_libdir = $(axiom_builddir)/lib
axiom_build_mandir = $(axiom_builddir)/man
axiom_build_docdir = $(axiom_builddir)/doc
axiom_build_datadir = $(axiom_builddir)/share
axiom_build_texdir = $(axiom_build_datadir)/texmf/tex

axiom_configdir = $(top_builddir)/config
axiom_c_macros = $(axiom_configdir)/axiom-c-macros.h

LATEX = @LATEX@

## Staging directory for the target DESTDIR
axiom_targetdir = $(top_builddir)/$(target)
axiom_target_bindir = $(axiom_targetdir)/bin
axiom_target_libdir = $(axiom_targetdir)/lib
axiom_target_srcdir = $(axiom_targetdir)/src
axiom_target_docdir = $(axiom_targetdir)/doc
axiom_target_datadir = $(axiom_targetdir)/share
axiom_target_texdir = $(axiom_target_datadir)/texmf/tex


## Where OpenAxiom keeps the tarballs for optional components
axiom_optional_srcdir = $(abs_top_srcdir)/zips

## The final directory where OpenAxiom is installed.  This is usually
## the directory deduced or specified at configuration time.
open_axiom_installdir = @open_axiom_installdir@

INC=$(top_srcdir)/src/include
oa_c_runtime_extra = @LIBS@ @oa_c_runtime_extra@ -lm
oa_c_libs = -lopen-axiom-core $(oa_c_runtime_extra) -lm

oa_yesno_to_lisp_boolean = $(subst yes,t,$(subst no,nil,$(1)))

oa_enable_profiling = @oa_enable_profiling@
oa_enable_lisp_profiling = \
	$(call oa_yesno_to_lisp_boolean,$(oa_enable_profiling))

oa_enable_threads = @oa_enable_threads@

axiom_use_x = @axiom_use_x@
AXIOM_X11_CFLAGS = @X_CFLAGS@ 
AXIOM_X11_LDFLAGS = @X_LIBS@ @X_PRE_LIBS@ -lX11 @X_EXTRA_LIBS@

axiom_includes = -I$(axiom_src_srcdir)/include -I$(axiom_configdir)

## Where the staging build directory is found
AXIOM = $(top_builddir)/$(target)

## Where to find OpenAxiom data bases.
DAASE = $(axiom_src_datadir)

TMP=$(axiom_builddir)

## Shall we build GCL?
oa_include_gcl = @oa_include_gcl@

## -------------------------------------------
## -- Files generated for the build machine --
## -------------------------------------------
axiom_build_document = $(axiom_top_builddir)/scripts/document
axiom_build_nowebdir = $(axiom_builddir)/noweb

TANGLE = @NOTANGLE@
WEAVE = @NOWEAVE@
## We export TANGLE and WEAVE for use in subshells, such as document.
export TANGLE
export WEAVE

AXIOM_LISP = @AXIOM_LISP@
# Extension of the output file name returned by compile-file
FASLEXT = @axiom_fasl_type@

# Extension of compiled FASLs appropriate for linking into executable
# programs.  For most Lisp systems, it is the same as FASLEXT because
# they build programs by dumping images.
ifeq (@axiom_lisp_flavor@,ecl)
LNKEXT = $(OBJEXT)
else
LNKEXT = $(FASLEXT)
endif


# Qt utilities
OA_QT_MOC = @OA_QT_MOC@
OA_QT_QMAKE = @OA_QT_QMAKE@

##
AXIOMXLROOT=${AXIOM}/compiler

## Lisp command to end a session.
BYE=bye

## Clear suffix-based implicit rule table.
.SUFFIXES:
