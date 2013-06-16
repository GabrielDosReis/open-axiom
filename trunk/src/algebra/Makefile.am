# Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
# All rights reserved.
# Copyright (C) 2007-2013, Gabriel Dos Reis.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     - Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     - Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#
#     - Neither the name of The Numerical Algorithms Group Ltd. nor the
#       names of its contributors may be used to endorse or promote products
#       derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#

# * Adding new algebra
#
# This is a complex process by its very nature. Developers and Maintainers
# who undertake the process need to understand quite a lot of detail. The
# ultimate steps to add algebra are tedious but simple. Note that only
# algebra code that gets shipped with the system needs to undergo this
# process. User code can be compiled once the distributed algebra exists
# and does not need either this Makefile or this installation process.
#
# NOTE: If you add new algebra to this file you must also update
#
# \File{exposed.lsp.pamphlet}
#
# otherwise the new algebra won't be loaded by the interpreter when needed.
#
# The first idea that you need to understand is the overall process
# of adding algebra code. Lets assume that you have a brand new spad
# file, called \File{foo.spad} containing a simple domain `BAR'. The
# steps in the process of adding this file are:
#  1. Find out where the algebra code lives in the lattice.
#     You do this by 
#       a. starting a new interpsys session
#       b. collecting all the names of the algebra files BAR requires
#       c. determining which layer each of the required files resides
#       d. determine the highest layer (e.g. 14) that contains
#          the required files
#
#  2. insert the documentation into the next layer (e.g. 15)
#
#  3. insert the [[\${OUT}/BAR.$(FASLEXT)]] file into the layer's file list
#
# * Rebuilding the algebra from scratch
#
# Compile order is important. Here we try to define the ordered lattice
# of spad file dependencies. However this is, in reality, a graph rather
# than a lattice.
#

oa_targetdir = $(top_builddir)/$(target)
oa_target_bindir = $(oa_targetdir)/bin
oa_target_libdir = $(oa_targetdir)/lib
oa_target_docdir = $(oa_targetdir)/doc
oa_target_srcdir = $(oa_targetdir)/src
oa_target_datadir = $(oa_targetdir)/share

oa_src_srcdir = $(top_srcdir)/src
oa_src_docdir = $(top_srcdir)/doc
oa_src_datadir = $(oa_src_srcdir)/share

oa_builddir = $(top_builddir)/build-$(build)
oa_build_bindir = $(oa_builddir)/bin
oa_build_texdir = $(oa_builddir)/texmf/tex
oa_build_docdir = $(oa_builddir)/doc

top_confdir = $(top_srcdir)/config

OUT=$(oa_targetdir)/algebra
DOC=$(oa_target_docdir)/src/algebra
OUTSRC=$(oa_target_srcdir)/algebra
INPUT=../input


STAMP = echo timestamp >

# Extension of the output file name returned by compile-file
FASLEXT = @axiom_fasl_type@

oa_hammer = $(top_builddir)/src/utils/hammer$(EXEEXT)

# Name of the driver to launch the compiler
DRIVER = ../driver/open-axiom$(EXEEXT)

INTERPSYS = ../interp/interpsys$(EXEEXT)
BOOTSYS = $(oa_target_bindir)/bootsys$(EXEEXT)

COMPILE_SPAD = $(DRIVER) --execpath=$(INTERPSYS) \
		--system="$(oa_targetdir)" --initial-db=initdb.$(FASLEXT) \
		--sysdb="$(oa_src_datadir)/algebra/" \
		--strap=strap-2 --optimize=3 \
		--system-algebra --compile $<

BOOTSTRAP = $(DRIVER) --execpath=$(INTERPSYS) \
		--system="$(oa_targetdir)" --initial-db=initdb.$(FASLEXT) \
		--sysdb="$(oa_src_datadir)/algebra/" \
		--system-algebra --compile

# Speed up implicit rules matching.
SUFFIXES = .spad .$(FASLEXT) $(EXEEXT) .input .pamphlet .tex .dvi

# Bootstrap layer 0 dependencies.
# This layer comprises mostly categories, and a limited number of domains.
# Categories are compiled for their exports.  Default definitions
# are skipped.  Similarly, domains are compiled, mostly as incomplete types.
# They are needed only for their being known as constructors.
# Consequently, the dependencies listed here are at the categories
# inheritance level; not necessarily at the use level.
strap-0/TYPE.$(FASLEXT): initdb.$(FASLEXT)
strap-0/UTYPE.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/FUNCTOR.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/BASTYPE.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/KOERCE.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/KONVERT.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/RETRACT.$(FASLEXT): strap-0/KRCFROM.$(FASLEXT)
strap-0/FRETRCT.$(FASLEXT): strap-0/RETRACT.$(FASLEXT)
strap-0/SETCAT.$(FASLEXT): strap-0/BASTYPE.$(FASLEXT) \
	strap-0/KOERCE.$(FASLEXT)
strap-0/FINITE.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/ORDTYPE.$(FASLEXT): strap-0/BASTYPE.$(FASLEXT)
strap-0/ORDSET.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/ORDTYPE.$(FASLEXT)
strap-0/ORDFIN.$(FASLEXT): strap-0/ORDSET.$(FASLEXT) \
	strap-0/FINITE.$(FASLEXT)
strap-0/LOGIC.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/BOOLE.$(FASLEXT): strap-0/LOGIC.$(FASLEXT)
strap-0/PROPLOG.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/BOOLE.$(FASLEXT)
strap-0/SEXCAT.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/LLINSET.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/RLINSET.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/LINSET.$(FASLEXT): strap-0/LLINSET.$(FASLEXT) \
	strap-0/RLINSET.$(FASLEXT)
strap-0/ABELSG.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/ABELMON.$(FASLEXT): strap-0/ABELSG.$(FASLEXT)
strap-0/CABMON.$(FASLEXT): strap-0/ABELMON.$(FASLEXT)
strap-0/OCAMON.$(FASLEXT): strap-0/OAMON.$(FASLEXT) \
	strap-0/CABMON.$(FASLEXT)
strap-0/SGROUP.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/OAMON.$(FASLEXT): strap-0/OASGP.$(FASLEXT) \
	strap-0/ABELMON.$(FASLEXT)
strap-0/OASGP.$(FASLEXT): strap-0/ORDSET.$(FASLEXT)
strap-0/MONOID.$(FASLEXT): strap-0/SGROUP.$(FASLEXT)
strap-0/GROUP.$(FASLEXT): strap-0/MONOID.$(FASLEXT)
strap-0/ABELGRP.$(FASLEXT): strap-0/CABMON.$(FASLEXT) \
	strap-0/LLINSET.$(FASLEXT)
strap-0/OAGROUP.$(FASLEXT): strap-0/OCAMON.$(FASLEXT) \
	strap-0/ABELGRP.$(FASLEXT)
strap-0/RNG.$(FASLEXT): strap-0/ABELGRP.$(FASLEXT) \
	strap-0/SGROUP.$(FASLEXT)
strap-0/LMODULE.$(FASLEXT): strap-0/ABELGRP.$(FASLEXT) \
	strap-0/LLINSET.$(FASLEXT)
strap-0/RMODULE.$(FASLEXT): strap-0/ABELGRP.$(FASLEXT) \
	strap-0/RLINSET.$(FASLEXT)
strap-0/SRING.$(FASLEXT): strap-0/ABELMON.$(FASLEXT) \
	strap-0/MONOID.$(FASLEXT)
strap-0/ORDRING.$(FASLEXT): strap-0/OAGROUP.$(FASLEXT) \
	strap-0/CHARZ.$(FASLEXT) strap-0/MONOID.$(FASLEXT)
strap-0/RING.$(FASLEXT): strap-0/RNG.$(FASLEXT) \
	strap-0/SRING.$(FASLEXT) \
	strap-0/LMODULE.$(FASLEXT) strap-0/KRCFROM.$(FASLEXT)
strap-0/DIOID.$(FASLEXT): strap-0/OAMON.$(FASLEXT) strap-0/SRING.$(FASLEXT)
strap-0/COMRING.$(FASLEXT): strap-0/RING.$(FASLEXT) \
	strap-0/BMODULE.$(FASLEXT)
strap-0/INTDOM.$(FASLEXT): strap-0/COMRING.$(FASLEXT) \
	strap-0/ALGEBRA.$(FASLEXT) strap-0/ENTIRER.$(FASLEXT)
strap-0/GCDDOM.$(FASLEXT): strap-0/INTDOM.$(FASLEXT)
strap-0/BMODULE.$(FASLEXT): strap-0/LMODULE.$(FASLEXT) \
	strap-0/RMODULE.$(FASLEXT)
strap-0/MODULE.$(FASLEXT): strap-0/COMRING.$(FASLEXT) \
	strap-0/BMODULE.$(FASLEXT) strap-0/LINSET.$(FASLEXT)
strap-0/ENTIRER.$(FASLEXT): strap-0/RING.$(FASLEXT) \
	strap-0/BMODULE.$(FASLEXT)
strap-0/ALGEBRA.$(FASLEXT): strap-0/RING.$(FASLEXT) \
	strap-0/MODULE.$(FASLEXT) strap-0/KOERCE.$(FASLEXT)
strap-0/EUCDOM.$(FASLEXT): strap-0/PID.$(FASLEXT)
strap-0/PID.$(FASLEXT): strap-0/GCDDOM.$(FASLEXT)
strap-0/OINTDOM.$(FASLEXT): strap-0/INTDOM.$(FASLEXT) \
	strap-0/ORDRING.$(FASLEXT)
strap-0/FIELD.$(FASLEXT): strap-0/EUCDOM.$(FASLEXT) \
	strap-0/UFD.$(FASLEXT) strap-0/DIVRING.$(FASLEXT)
strap-0/DIVRING.$(FASLEXT): strap-0/ENTIRER.$(FASLEXT) \
	strap-0/ALGEBRA.$(FASLEXT)
strap-0/DIFFSPC.$(FASLEXT): strap-0/DIFFDOM.$(FASLEXT)
strap-0/DIFFDOM.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/DIFRING.$(FASLEXT): strap-0/RING.$(FASLEXT) \
	strap-0/DIFFSPC.$(FASLEXT)
strap-0/LINEXP.$(FASLEXT): strap-0/LMODULE.$(FASLEXT)
strap-0/CFCAT.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/REAL.$(FASLEXT): strap-0/KONVERT.$(FASLEXT)
strap-0/CHARZ.$(FASLEXT): strap-0/RING.$(FASLEXT)
strap-0/STEP.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/PATMAB.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/UFD.$(FASLEXT): strap-0/GCDDOM.$(FASLEXT)
strap-0/PDSPC.$(FASLEXT): strap-0/PDDOM.$(FASLEXT)
strap-0/DSEXT.$(FASLEXT): strap-0/DIFFSPC.$(FASLEXT) \
	strap-0/PDSPC.$(FASLEXT)
strap-0/DIFEXT.$(FASLEXT): strap-0/DSEXT.$(FASLEXT) \
	strap-0/DIFRING.$(FASLEXT) strap-0/PDRING.$(FASLEXT)
strap-0/PFECAT.$(FASLEXT): strap-0/UFD.$(FASLEXT) strap-0/CHARZ.$(FASLEXT)
strap-0/QFCAT.$(FASLEXT): strap-0/FIELD.$(FASLEXT) \
	strap-0/ALGEBRA.$(FASLEXT) strap-0/RETRACT.$(FASLEXT) \
	strap-0/FEVALAB.$(FASLEXT) strap-0/DIFEXT.$(FASLEXT) \
	strap-0/FLINEXP.$(FASLEXT) strap-0/PATAB.$(FASLEXT) \
	strap-0/FPATMAB.$(FASLEXT) strap-0/ORDSET.$(FASLEXT) \
	strap-0/OINTDOM.$(FASLEXT) strap-0/REAL.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT) strap-0/CHARZ.$(FASLEXT) \
	strap-0/CHARNZ.$(FASLEXT) strap-0/EUCDOM.$(FASLEXT) \
	strap-0/PFECAT.$(FASLEXT)
strap-0/FPATMAB.$(FASLEXT): strap-0/PATMAB.$(FASLEXT)
strap-0/PATAB.$(FASLEXT): strap-0/KONVERT.$(FASLEXT)
strap-0/FLINEXP.$(FASLEXT): strap-0/LINEXP.$(FASLEXT)
strap-0/FEVALAB.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/FUNCTOR.$(FASLEXT) strap-0/ELTAB.$(FASLEXT) \
	strap-0/EVALAB.$(FASLEXT) strap-0/IEVALAB.$(FASLEXT)
strap-0/CHARNZ.$(FASLEXT): strap-0/RING.$(FASLEXT)
strap-0/INS.$(FASLEXT): strap-0/UFD.$(FASLEXT) strap-0/EUCDOM.$(FASLEXT) \
	strap-0/OINTDOM.$(FASLEXT) strap-0/DIFRING.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT) strap-0/RETRACT.$(FASLEXT) \
	strap-0/LINEXP.$(FASLEXT) strap-0/CFCAT.$(FASLEXT) \
	strap-0/REAL.$(FASLEXT) strap-0/CHARZ.$(FASLEXT) \
	strap-0/STEP.$(FASLEXT) strap-0/PATMAB.$(FASLEXT)
strap-0/FPS.$(FASLEXT): strap-0/RNS.$(FASLEXT)
strap-0/RNS.$(FASLEXT): strap-0/FIELD.$(FASLEXT) strap-0/ORDRING.$(FASLEXT) \
	strap-0/RETRACT.$(FASLEXT) strap-0/RADCAT.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT) strap-0/PATMAB.$(FASLEXT) \
	strap-0/CHARZ.$(FASLEXT)
strap-0/TRANFUN.$(FASLEXT): strap-0/TRIGCAT.$(FASLEXT) \
	strap-0/ATRIG.$(FASLEXT) strap-0/HYPCAT.$(FASLEXT) \
	strap-0/AHYP.$(FASLEXT) strap-0/ELEMFUN.$(FASLEXT)
strap-0/TRIGCAT.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/ATRIG.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/HYPCAT.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/AHYP.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/ELEMFUN.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/SEGCAT.$(FASLEXT): strap-0/KVTFROM.$(FASLEXT)
strap-0/SEGXCAT.$(FASLEXT): strap-0/SEGCAT.$(FASLEXT)
strap-0/VECTCAT.$(FASLEXT): strap-0/A1AGG.$(FASLEXT) \
	strap-0/ABELMON.$(FASLEXT) strap-0/RING.$(FASLEXT) \
	strap-0/MONOID.$(FASLEXT) strap-0/ABELGRP.$(FASLEXT)
strap-0/ARR2CAT.$(FASLEXT): strap-0/FINAGG.$(FASLEXT) strap-0/SMAGG.$(FASLEXT)
strap-0/MATCAT.$(FASLEXT): strap-0/ARR2CAT.$(FASLEXT) \
	strap-0/EUCDOM.$(FASLEXT) strap-0/INTDOM.$(FASLEXT) \
	strap-0/FIELD.$(FASLEXT) strap-0/FINAGG.$(FASLEXT)
strap-0/IDPC.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) strap-0/FUNCTOR.$(FASLEXT)
strap-0/AMR.$(FASLEXT): strap-0/RING.$(FASLEXT) strap-0/BMODULE.$(FASLEXT) \
	strap-0/ALGEBRA.$(FASLEXT) strap-0/FIELD.$(FASLEXT) \
	strap-0/CHARZ.$(FASLEXT) strap-0/CHARNZ.$(FASLEXT) \
	strap-0/FUNCTOR.$(FASLEXT)
strap-0/FAMR.$(FASLEXT): strap-0/AMR.$(FASLEXT) \
	strap-0/FRETRCT.$(FASLEXT)
strap-0/POLYCAT.$(FASLEXT): strap-0/PFECAT.$(FASLEXT) \
	strap-0/PDRING.$(FASLEXT) \
	strap-0/FAMR.$(FASLEXT) strap-0/EVALAB.$(FASLEXT) \
	strap-0/RETRACT.$(FASLEXT) strap-0/FLINEXP.$(FASLEXT)
strap-0/PDRING.$(FASLEXT): strap-0/RING.$(FASLEXT) \
	strap-0/PDSPC.$(FASLEXT)
strap-0/UPOLYC.$(FASLEXT): strap-0/POLYCAT.$(FASLEXT) \
	strap-0/ELTAB.$(FASLEXT) strap-0/DIFRING.$(FASLEXT) \
	strap-0/DIFEXT.$(FASLEXT) strap-0/STEP.$(FASLEXT) \
	strap-0/COMRING.$(FASLEXT) strap-0/INTDOM.$(FASLEXT) \
	strap-0/FIELD.$(FASLEXT) strap-0/ALGEBRA.$(FASLEXT)
strap-0/TBAGG.$(FASLEXT): strap-0/KDAGG.$(FASLEXT) strap-0/FINAGG.$(FASLEXT)
strap-0/KDAGG.$(FASLEXT): strap-0/DIAGG.$(FASLEXT) strap-0/IXAGG.$(FASLEXT)
strap-0/DIAGG.$(FASLEXT): strap-0/DIOPS.$(FASLEXT)
strap-0/DIOPS.$(FASLEXT): strap-0/BGAGG.$(FASLEXT)
strap-0/BGAGG.$(FASLEXT): strap-0/SMAGG.$(FASLEXT) 
strap-0/SMAGG.$(FASLEXT): strap-0/HOAGG.$(FASLEXT)
strap-0/LSAGG.$(FASLEXT): strap-0/STAGG.$(FASLEXT) \
	strap-0/FLAGG.$(FASLEXT) strap-0/ELAGG.$(FASLEXT)
strap-0/STAGG.$(FASLEXT): strap-0/URAGG.$(FASLEXT)
strap-0/URAGG.$(FASLEXT): strap-0/RCAGG.$(FASLEXT)
strap-0/RCAGG.$(FASLEXT): strap-0/HOAGG.$(FASLEXT)
strap-0/ELAGG.$(FASLEXT): strap-0/LNAGG.$(FASLEXT) strap-0/SMAGG.$(FASLEXT)
strap-0/SRAGG.$(FASLEXT): strap-0/A1AGG.$(FASLEXT)
strap-0/A1AGG.$(FASLEXT): strap-0/FLAGG.$(FASLEXT) strap-0/SMAGG.$(FASLEXT)
strap-0/FLAGG.$(FASLEXT): strap-0/LNAGG.$(FASLEXT) strap-0/FINAGG.$(FASLEXT)
strap-0/FINAGG.$(FASLEXT): strap-0/HOAGG.$(FASLEXT)
strap-0/LNAGG.$(FASLEXT): strap-0/IXAGG.$(FASLEXT) \
	strap-0/CLAGG.$(FASLEXT) strap-0/ELTAB.$(FASLEXT)
strap-0/CLAGG.$(FASLEXT): strap-0/HOAGG.$(FASLEXT)
strap-0/IXAGG.$(FASLEXT): strap-0/HOAGG.$(FASLEXT) \
	strap-0/ELTAGG.$(FASLEXT)
strap-0/HOAGG.$(FASLEXT): strap-0/AGG.$(FASLEXT) strap-0/FUNCTOR.$(FASLEXT) \
	strap-0/KOERCE.$(FASLEXT) strap-0/BASTYPE.$(FASLEXT) \
	strap-0/SETCAT.$(FASLEXT) strap-0/EVALAB.$(FASLEXT)
strap-0/SETAGG.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/AGG.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/ELTAGG.$(FASLEXT): strap-0/AGG.$(FASLEXT) strap-0/ELTAB.$(FASLEXT)
strap-0/ELTAB.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/EVALAB.$(FASLEXT): strap-0/IEVALAB.$(FASLEXT)
strap-0/IEVALAB.$(FASLEXT): strap-0/TYPE.$(FASLEXT)
strap-0/SEXCAT.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/KVTFROM.$(FASLEXT) strap-0/ELTAB.$(FASLEXT)
strap-0/BOOLEAN.$(FASLEXT): strap-0/PROPLOG.$(FASLEXT) \
	strap-0/ORDFIN.$(FASLEXT) strap-0/KONVERT.$(FASLEXT)
strap-0/SINT.$(FASLEXT): strap-0/INS.$(FASLEXT) strap-0/ORDFIN.$(FASLEXT) \
	strap-0/BOOLE.$(FASLEXT) strap-0/INT.$(FASLEXT)
strap-0/INT.$(FASLEXT): strap-0/INS.$(FASLEXT)
strap-0/NNI.$(FASLEXT): strap-0/OAMONS.$(FASLEXT) \
	strap-0/MONOID.$(FASLEXT) strap-0/INT.$(FASLEXT)
strap-0/OAMONS.$(FASLEXT): strap-0/OCAMON.$(FASLEXT)
strap-0/PI.$(FASLEXT): strap-0/OASGP.$(FASLEXT) \
	strap-0/MONOID.$(FASLEXT) strap-0/NNI.$(FASLEXT)
strap-0/STRING.$(FASLEXT): strap-0/SRAGG.$(FASLEXT)
strap-0/LIST.$(FASLEXT): strap-0/LSAGG.$(FASLEXT)
strap-0/VECTOR.$(FASLEXT): strap-0/VECTCAT.$(FASLEXT)
strap-0/OUTFORM.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/INFORM.$(FASLEXT): strap-0/SEXCAT.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT)
strap-0/OVERSET.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/CTORKIND.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/MAYBE.$(FASLEXT): strap-0/UTYPE.$(FASLEXT) \
	strap-0/RETRACT.$(FASLEXT) strap-0/KOERCE.$(FASLEXT)
strap-0/DFLOAT.$(FASLEXT): strap-0/FPS.$(FASLEXT) \
	strap-0/DIFRING.$(FASLEXT) strap-0/TRANFUN.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT)
strap-0/FLOAT.$(FASLEXT): strap-0/FPS.$(FASLEXT) \
	strap-0/DIFRING.$(FASLEXT) strap-0/TRANFUN.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT) strap-0/KVTFROM.$(FASLEXT)
strap-0/FRAC.$(FASLEXT): strap-0/QFCAT.$(FASLEXT)
strap-0/MATRIX.$(FASLEXT): strap-0/MATCAT.$(FASLEXT)
strap-0/SUP.$(FASLEXT): strap-0/UPOLYC.$(FASLEXT) strap-0/PR.$(FASLEXT)
strap-0/PR.$(FASLEXT): strap-0/FAMR.$(FASLEXT)
strap-0/TABLE.$(FASLEXT): strap-0/TBAGG.$(FASLEXT)
strap-0/EQ.$(FASLEXT): strap-0/TYPE.$(FASLEXT) strap-0/FUNCTOR.$(FASLEXT) \
	strap-0/IEVALAB.$(FASLEXT) strap-0/KOERCE.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT) strap-0/EVALAB.$(FASLEXT) \
	strap-0/ABELSG.$(FASLEXT) strap-0/ABELGRP.$(FASLEXT) \
	strap-0/MONOID.$(FASLEXT) strap-0/GROUP.$(FASLEXT) \
	strap-0/RING.$(FASLEXT) strap-0/BMODULE.$(FASLEXT) \
	strap-0/COMRING.$(FASLEXT) strap-0/MODULE.$(FASLEXT) \
	strap-0/INTDOM.$(FASLEXT) strap-0/PDRING.$(FASLEXT) \
	strap-0/FIELD.$(FASLEXT) strap-0/VSPACE.$(FASLEXT) \
	strap-0/ES.$(FASLEXT)
strap-0/ES.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) strap-0/IEVALAB.$(FASLEXT) \
	strap-0/EVALAB.$(FASLEXT)
strap-0/VSPACE.$(FASLEXT): strap-0/MODULE.$(FASLEXT)
strap-0/UNISEG.$(FASLEXT): strap-0/SEGCAT.$(FASLEXT) \
	strap-0/SETCAT.$(FASLEXT) strap-0/SEGXCAT.$(FASLEXT) \
	strap-0/ORDRING.$(FASLEXT)
strap-0/SYNTAX.$(FASLEXT): strap-0/UTYPE.$(FASLEXT) \
	strap-0/SETCAT.$(FASLEXT) strap-0/RETRACT.$(FASLEXT) \
	strap-0/INT.$(FASLEXT) strap-0/DFLOAT.$(FASLEXT) \
	strap-0/IDENT.$(FASLEXT) strap-0/STRING.$(FASLEXT) \
	strap-0/KOERCE.$(FASLEXT) strap-0/INFORM.$(FASLEXT)
strap-0/FR.$(FASLEXT): strap-0/INTDOM.$(FASLEXT) strap-0/FUNCTOR.$(FASLEXT) \
	strap-0/DIFEXT.$(FASLEXT) strap-0/FEVALAB.$(FASLEXT) \
	strap-0/ALGEBRA.$(FASLEXT) strap-0/FRETRCT.$(FASLEXT)

strap-0/PATTERN.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/RETRACT.$(FASLEXT)

strap-0/SYMBOL.$(FASLEXT): strap-0/ORDSET.$(FASLEXT)

strap-0/BOP.$(FASLEXT): strap-0/ORDSET.$(FASLEXT) strap-0/OPERCAT.$(FASLEXT)

strap-0/ALIST.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) strap-0/ALAGG.$(FASLEXT)

strap-0/CHAR.$(FASLEXT): strap-0/ORDFIN.$(FASLEXT)

strap-0/KERNEL.$(FASLEXT): strap-0/CACHSET.$(FASLEXT) \
	strap-0/SETCAT.$(FASLEXT) \
	strap-0/ORDSET.$(FASLEXT) strap-0/PATAB.$(FASLEXT)

# Bootstrap stage 1 dependencies.
# At this stage, we attempt to compile domains essential for completing
# the bootstrap process (stage 2).  Those that were compiled at stage 0
# were (necessarily) incomplete.

strap-1/FUNCTOR.$(FASLEXT): strap-1/TYPE.$(FASLEXT)
strap-1/BASTYPE.$(FASLEXT): strap-1/TYPE.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT)

strap-1/SETCAT.$(FASLEXT): strap-1/BASTYPE.$(FASLEXT) \
	strap-0/SINT.$(FASLEXT) strap-0/STRING.$(FASLEXT) \
	strap-0/OUTFORM.$(FASLEXT)

strap-1/LOGIC.$(FASLEXT): strap-1/TYPE.$(FASLEXT)

strap-1/BOOLE.$(FASLEXT): strap-1/LOGIC.$(FASLEXT)

strap-1/PROPLOG.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/BOOLE.$(FASLEXT)

strap-1/ORDTYPE.$(FASLEXT): strap-1/BASTYPE.$(FASLEXT)

strap-1/ORDSET.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/ORDTYPE.$(FASLEXT)

strap-1/FINITE.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-0/OAMONS.$(FASLEXT) strap-0/PI.$(FASLEXT)

strap-1/ORDFIN.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) \
	strap-1/FINITE.$(FASLEXT)

strap-1/PID.$(FASLEXT): strap-1/GCDDOM.$(FASLEXT) \
	strap-0/MAYBE.$(FASLEXT)

strap-1/GCDDOM.$(FASLEXT): strap-1/INTDOM.$(FASLEXT) \
	strap-0/LIST.$(FASLEXT) strap-0/SUP.$(FASLEXT)

strap-1/EUCDOM.$(FASLEXT): strap-1/PID.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT) strap-0/LSAGG.$(FASLEXT)

strap-1/UFD.$(FASLEXT): strap-1/GCDDOM.$(FASLEXT) \
	strap-1/BOOLEAN.$(FASLEXT) strap-0/FR.$(FASLEXT)

strap-1/LLINSET.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/RLINSET.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/LINSET.$(FASLEXT): strap-1/LLINSET.$(FASLEXT) \
	strap-1/RLINSET.$(FASLEXT)

strap-1/SGROUP.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/ABELSG.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/MONOID.$(FASLEXT): strap-1/SGROUP.$(FASLEXT)

strap-1/ABELMON.$(FASLEXT): strap-1/ABELSG.$(FASLEXT)

strap-1/CABMON.$(FASLEXT): strap-1/ABELMON.$(FASLEXT)

strap-1/OCAMON.$(FASLEXT): strap-1/OAMON.$(FASLEXT) \
	strap-1/CABMON.$(FASLEXT)

strap-1/OAMON.$(FASLEXT): strap-1/OASGP.$(FASLEXT) \
	strap-1/ABELMON.$(FASLEXT)

strap-1/OASGP.$(FASLEXT): strap-1/ORDSET.$(FASLEXT)

strap-1/OAMONS.$(FASLEXT): strap-1/OCAMON.$(FASLEXT)

strap-1/ABELGRP.$(FASLEXT): strap-1/CABMON.$(FASLEXT) \
	strap-1/LLINSET.$(FASLEXT)

strap-1/OAGROUP.$(FASLEXT): strap-1/OCAMON.$(FASLEXT) \
	strap-1/ABELGRP.$(FASLEXT)

strap-1/RNG.$(FASLEXT): strap-1/ABELGRP.$(FASLEXT) \
	strap-1/SGROUP.$(FASLEXT)

strap-1/LMODULE.$(FASLEXT): strap-1/ABELGRP.$(FASLEXT) \
	strap-1/LLINSET.$(FASLEXT)

strap-1/RMODULE.$(FASLEXT): strap-1/ABELGRP.$(FASLEXT) \
	strap-1/RLINSET.$(FASLEXT)

strap-1/BMODULE.$(FASLEXT): strap-1/LMODULE.$(FASLEXT) \
	strap-1/RMODULE.$(FASLEXT)

strap-1/SRING.$(FASLEXT): strap-1/ABELMON.$(FASLEXT) \
	strap-1/MONOID.$(FASLEXT)

strap-1/GROUP.$(FASLEXT): strap-1/MONOID.$(FASLEXT) \
	strap-0/REPSQ.$(FASLEXT) strap-0/INT.$(FASLEXT)

strap-1/RING.$(FASLEXT): strap-1/RNG.$(FASLEXT) \
	strap-1/SRING.$(FASLEXT) \
	strap-1/LMODULE.$(FASLEXT) strap-1/KRCFROM.$(FASLEXT)

strap-1/DIOID.$(FASLEXT): strap-1/OAMON.$(FASLEXT) strap-1/SRING.$(FASLEXT)

strap-1/SRING.$(FASLEXT): strap-1/ABELMON.$(FASLEXT) \
	strap-1/MONOID.$(FASLEXT)

strap-1/ORDRING.$(FASLEXT): strap-1/OAGROUP.$(FASLEXT) \
	strap-1/CHARZ.$(FASLEXT) strap-1/MONOID.$(FASLEXT)

strap-1/CHARZ.$(FASLEXT): strap-1/RING.$(FASLEXT)

strap-1/COMRING.$(FASLEXT): strap-1/RING.$(FASLEXT) \
	strap-1/BMODULE.$(FASLEXT)

strap-1/ALGEBRA.$(FASLEXT): strap-1/RING.$(FASLEXT) \
	strap-1/MODULE.$(FASLEXT) strap-1/KOERCE.$(FASLEXT)

strap-1/MODULE.$(FASLEXT): strap-1/COMRING.$(FASLEXT) \
	strap-1/BMODULE.$(FASLEXT) strap-1/LINSET.$(FASLEXT)

strap-1/INTDOM.$(FASLEXT): strap-1/COMRING.$(FASLEXT) \
	strap-1/ALGEBRA.$(FASLEXT) strap-1/ENTIRER.$(FASLEXT) \
	strap-0/FIELD.$(FASLEXT)

strap-1/ENTIRER.$(FASLEXT): strap-1/RING.$(FASLEXT) \
	strap-0/BMODULE.$(FASLEXT)

strap-1/OINTDOM.$(FASLEXT): strap-1/INTDOM.$(FASLEXT) \
	strap-1/ORDRING.$(FASLEXT)

strap-1/FIELD.$(FASLEXT): strap-1/EUCDOM.$(FASLEXT) \
	strap-1/UFD.$(FASLEXT) strap-1/DIVRING.$(FASLEXT)

strap-1/FPC.$(FASLEXT): strap-1/FIELD.$(FASLEXT) strap-1/CHARNZ.$(FASLEXT)

strap-1/CHARNZ.$(FASLEXT): strap-1/RING.$(FASLEXT)

strap-1/FFIELDC.$(FASLEXT): strap-1/FPC.$(FASLEXT) strap-1/FINITE.$(FASLEXT) \
	strap-1/STEP.$(FASLEXT) strap-1/DIFRING.$(FASLEXT) \
	strap-0/TABLE.$(FASLEXT)

strap-1/STEP.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/DIVRING.$(FASLEXT): strap-1/ENTIRER.$(FASLEXT) \
	strap-1/ALGEBRA.$(FASLEXT) strap-0/FRAC.$(FASLEXT)

strap-1/REAL.$(FASLEXT): strap-1/KONVERT.$(FASLEXT) \
	strap-0/DFLOAT.$(FASLEXT) strap-0/FLOAT.$(FASLEXT)

strap-1/INS.$(FASLEXT): strap-1/UFD.$(FASLEXT) strap-1/EUCDOM.$(FASLEXT) \
	strap-1/OINTDOM.$(FASLEXT) strap-1/DIFRING.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/RETRACT.$(FASLEXT) \
	strap-1/LINEXP.$(FASLEXT) strap-1/CFCAT.$(FASLEXT) \
	strap-1/REAL.$(FASLEXT) strap-1/CHARZ.$(FASLEXT) \
	strap-1/STEP.$(FASLEXT) strap-1/PATMAB.$(FASLEXT) \
	strap-0/PATTERN.$(FASLEXT) strap-0/INFORM.$(FASLEXT) \
	strap-0/NNI.$(FASLEXT) strap-0/FRAC.$(FASLEXT)

strap-1/PATMAB.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/PATTERN.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/RETRACT.$(FASLEXT) strap-0/SYMBOL.$(FASLEXT) \
	strap-0/BOP.$(FASLEXT)

strap-1/LINEXP.$(FASLEXT): strap-1/LMODULE.$(FASLEXT)

strap-1/FPS.$(FASLEXT): strap-1/RNS.$(FASLEXT)

strap-1/RNS.$(FASLEXT): strap-1/FIELD.$(FASLEXT) strap-1/ORDRING.$(FASLEXT) \
	strap-1/RETRACT.$(FASLEXT) strap-1/RADCAT.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/PATMAB.$(FASLEXT) \
	strap-1/CHARZ.$(FASLEXT)

strap-1/DSEXT.$(FASLEXT): strap-1/DIFFSPC.$(FASLEXT) \
	strap-1/PDSPC.$(FASLEXT)

strap-1/DIFEXT.$(FASLEXT): strap-1/DSEXT.$(FASLEXT) \
	strap-1/DIFRING.$(FASLEXT) strap-1/PDRING.$(FASLEXT)

strap-1/DIFRING.$(FASLEXT): strap-1/RING.$(FASLEXT) \
	strap-1/DIFFSPC.$(FASLEXT)

strap-1/DIFFSPC.$(FASLEXT): strap-1/DIFFDOM.$(FASLEXT)

strap-1/PDRING.$(FASLEXT): strap-1/PDSPC.$(FASLEXT)

strap-1/PDSPC.$(FASLEXT): strap-1/PDDOM.$(FASLEXT)

strap-1/PDDOM.$(FASLEXT): strap-1/TYPE.$(FASLEXT)

strap-1/TRANFUN.$(FASLEXT): strap-1/TRIGCAT.$(FASLEXT) \
	strap-1/ATRIG.$(FASLEXT) strap-1/HYPCAT.$(FASLEXT) \
	strap-1/AHYP.$(FASLEXT) strap-1/ELEMFUN.$(FASLEXT)

strap-1/TRIGCAT.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-0/RING.$(FASLEXT)
strap-1/ATRIG.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-0/RING.$(FASLEXT)
strap-1/HYPCAT.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-0/RING.$(FASLEXT)
strap-1/AHYP.$(FASLEXT): strap-1/TYPE.$(FASLEXT)
strap-1/ELEMFUN.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-0/MONOID.$(FASLEXT)

strap-1/IDPC.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) strap-1/FUNCTOR.$(FASLEXT)

strap-1/AMR.$(FASLEXT): strap-1/RING.$(FASLEXT) strap-1/BMODULE.$(FASLEXT) \
	strap-1/ALGEBRA.$(FASLEXT) strap-1/FIELD.$(FASLEXT) \
	strap-1/CHARZ.$(FASLEXT) strap-1/CHARNZ.$(FASLEXT) \
	strap-1/FUNCTOR.$(FASLEXT)

strap-1/FAMR.$(FASLEXT): strap-1/AMR.$(FASLEXT) \
	strap-1/FRETRCT.$(FASLEXT)

strap-1/FRETRCT.$(FASLEXT): strap-1/RETRACT.$(FASLEXT) strap-0/INS.$(FASLEXT)

strap-1/POLYCAT.$(FASLEXT): strap-1/PDRING.$(FASLEXT) \
	strap-1/FAMR.$(FASLEXT) strap-1/EVALAB.$(FASLEXT) \
	strap-1/RETRACT.$(FASLEXT) strap-1/FLINEXP.$(FASLEXT) \
	strap-1/VECTOR.$(FASLEXT) strap-0/MATRIX.$(FASLEXT) \
	strap-0/LIST.$(FASLEXT) strap-0/SUP.$(FASLEXT) \
	strap-0/INFORM.$(FASLEXT)

strap-1/UPOLYC.$(FASLEXT): strap-1/POLYCAT.$(FASLEXT) \
	strap-1/ELTAB.$(FASLEXT) strap-1/DIFRING.$(FASLEXT) \
	strap-1/DIFEXT.$(FASLEXT) strap-1/STEP.$(FASLEXT) \
	strap-1/COMRING.$(FASLEXT) strap-1/INTDOM.$(FASLEXT) \
	strap-1/FIELD.$(FASLEXT) strap-1/ALGEBRA.$(FASLEXT) \
	strap-1/SAOS.$(FASLEXT)

strap-1/PFECAT.$(FASLEXT): strap-1/UFD.$(FASLEXT) strap-1/CHARZ.$(FASLEXT)

strap-1/QFCAT.$(FASLEXT): strap-1/FIELD.$(FASLEXT) \
	strap-1/ALGEBRA.$(FASLEXT) strap-1/RETRACT.$(FASLEXT) \
	strap-1/FEVALAB.$(FASLEXT) strap-1/DIFEXT.$(FASLEXT) \
	strap-1/FLINEXP.$(FASLEXT) strap-1/PATAB.$(FASLEXT) \
	strap-1/FPATMAB.$(FASLEXT) strap-1/ORDSET.$(FASLEXT) \
	strap-1/OINTDOM.$(FASLEXT) strap-1/REAL.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/CHARZ.$(FASLEXT) \
	strap-1/CHARNZ.$(FASLEXT) strap-1/EUCDOM.$(FASLEXT) \
	strap-1/PFECAT.$(FASLEXT)

strap-1/FEVALAB.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/FUNCTOR.$(FASLEXT) strap-1/EVALAB.$(FASLEXT) \
	strap-1/IEVALAB.$(FASLEXT) strap-1/ELTAB.$(FASLEXT)

strap-1/FPATMAB.$(FASLEXT): strap-1/PATMAB.$(FASLEXT)

strap-1/PATAB.$(FASLEXT): strap-1/KONVERT.$(FASLEXT)

strap-1/FLINEXP.$(FASLEXT): strap-1/LINEXP.$(FASLEXT) \
	strap-0/MATRIX.$(FASLEXT) strap-0/INT.$(FASLEXT) \
	strap-0/VECTOR.$(FASLEXT)

strap-1/OPERCAT.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-0/OUTFORM.$(FASLEXT) strap-0/BOOLEAN.$(FASLEXT)

strap-1/ARR2CAT.$(FASLEXT): strap-1/FINAGG.$(FASLEXT) strap-1/SMAGG.$(FASLEXT)

strap-1/SMAGG.$(FASLEXT): strap-1/AGG.$(FASLEXT)

strap-1/BTAGG.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) \
	strap-1/BOOLE.$(FASLEXT) strap-1/LOGIC.$(FASLEXT) \
	strap-1/A1AGG.$(FASLEXT)

strap-1/FSAGG.$(FASLEXT): strap-1/KDAGG.$(FASLEXT) \
	strap-1/SETAGG.$(FASLEXT) strap-1/FINAGG.$(FASLEXT)

strap-1/KDAGG.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)

strap-1/LSAGG.$(FASLEXT): strap-1/STAGG.$(FASLEXT) \
	strap-1/FLAGG.$(FASLEXT) strap-1/ELAGG.$(FASLEXT)

strap-1/STAGG.$(FASLEXT): strap-1/URAGG.$(FASLEXT)

strap-1/URAGG.$(FASLEXT): strap-1/RCAGG.$(FASLEXT) \
	strap-0/PI.$(FASLEXT)

strap-1/RCAGG.$(FASLEXT): strap-1/HOAGG.$(FASLEXT)

strap-1/ELAGG.$(FASLEXT): strap-1/LNAGG.$(FASLEXT) strap-1/SMAGG.$(FASLEXT)

strap-1/SRAGG.$(FASLEXT): strap-1/A1AGG.$(FASLEXT) \
	strap-0/CHAR.$(FASLEXT) strap-0/CCLASS.$(FASLEXT) \
	strap-0/STRING.$(FASLEXT)

strap-0/CCLASS.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT) strap-0/FSAGG.$(FASLEXT)

strap-0/FSAGG.$(FASLEXT): strap-0/DIAGG.$(FASLEXT) \
	strap-0/SETAGG.$(FASLEXT) strap-0/FINAGG.$(FASLEXT)

strap-1/A1AGG.$(FASLEXT): strap-1/FLAGG.$(FASLEXT) strap-1/SMAGG.$(FASLEXT)

strap-1/FLAGG.$(FASLEXT): strap-1/LNAGG.$(FASLEXT) strap-1/FINAGG.$(FASLEXT)

strap-1/FINAGG.$(FASLEXT): strap-1/HOAGG.$(FASLEXT)

strap-1/LNAGG.$(FASLEXT): strap-1/IXAGG.$(FASLEXT) \
	strap-1/CLAGG.$(FASLEXT) strap-1/ELTAB.$(FASLEXT) \
	strap-0/UNISEG.$(FASLEXT)

strap-1/CLAGG.$(FASLEXT): strap-1/HOAGG.$(FASLEXT)
strap-1/IXAGG.$(FASLEXT): strap-1/HOAGG.$(FASLEXT) \
	strap-1/ELTAGG.$(FASLEXT)

strap-1/HOAGG.$(FASLEXT): strap-1/AGG.$(FASLEXT) strap-1/FUNCTOR.$(FASLEXT) \
	strap-1/KOERCE.$(FASLEXT) strap-1/BASTYPE.$(FASLEXT) \
	strap-1/SETCAT.$(FASLEXT) strap-1/EVALAB.$(FASLEXT) \
	strap-0/EQ.$(FASLEXT)

strap-1/SETAGG.$(FASLEXT): strap-1/CLAGG.$(FASLEXT) strap-1/SETCAT.$(FASLEXT)

strap-1/AGG.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-0/INS.$(FASLEXT)

strap-1/ELTAGG.$(FASLEXT): strap-1/AGG.$(FASLEXT) strap-1/ELTAB.$(FASLEXT)

strap-1/ELTAB.$(FASLEXT): strap-1/TYPE.$(FASLEXT)

strap-1/EVALAB.$(FASLEXT): strap-1/IEVALAB.$(FASLEXT)

strap-1/IEVALAB.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-0/EQ.$(FASLEXT) strap-0/LIST.$(FASLEXT)

strap-1/SEXCAT.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/KVTFROM.$(FASLEXT) strap-1/ELTAB.$(FASLEXT)

strap-1/CTORCAT.$(FASLEXT): strap-1/OPERCAT.$(FASLEXT) \
	strap-0/CTORKIND.$(FASLEXT) strap-0/LIST.$(FASLEXT) \
	strap-0/OVERSET.$(FASLEXT) strap-1/ARITY.$(FASLEXT)

strap-1/VSPACE.$(FASLEXT): strap-1/FIELD.$(FASLEXT) \
	strap-1/MODULE.$(FASLEXT)

strap-1/MATCAT.$(FASLEXT): strap-1/ARR2CAT.$(FASLEXT) \
	strap-1/INTDOM.$(FASLEXT) strap-1/FIELD.$(FASLEXT) \
	strap-1/EUCDOM.$(FASLEXT) strap-1/FINAGG.$(FASLEXT)

strap-1/VECTCAT.$(FASLEXT): strap-1/A1AGG.$(FASLEXT) \
	strap-1/ABELSG.$(FASLEXT) strap-1/ABELMON.$(FASLEXT) \
	strap-1/ABELGRP.$(FASLEXT) strap-1/MONOID.$(FASLEXT) \
	strap-1/RING.$(FASLEXT) strap-1/RADCAT.$(FASLEXT)

strap-1/RADCAT.$(FASLEXT): strap-0/INS.$(FASLEXT)

strap-1/ARITY.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/RETRACT.$(FASLEXT) strap-0/NNI.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT) strap-0/SINT.$(FASLEXT) \
	strap-0/OUTFORM.$(FASLEXT)

strap-1/CTORCALL.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/CTORCAT.$(FASLEXT) strap-0/LIST.$(FASLEXT) \
	strap-0/SYNTAX.$(FASLEXT)

strap-1/OUTFORM.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-0/VOID.$(FASLEXT) strap-0/STRING.$(FASLEXT) \
	strap-0/NNI.$(FASLEXT) strap-0/BOOLEAN.$(FASLEXT) \
	strap-0/NUMFMT.$(FASLEXT) strap-0/IDENT.$(FASLEXT) \
	strap-0/LIST.$(FASLEXT) strap-0/CHAR.$(FASLEXT)

strap-1/BOOLEAN.$(FASLEXT): strap-1/PROPLOG.$(FASLEXT) \
	strap-1/ORDFIN.$(FASLEXT) strap-1/KONVERT.$(FASLEXT) \
	strap-0/PI.$(FASLEXT)

strap-1/SINT.$(FASLEXT): strap-1/INS.$(FASLEXT) strap-1/INT.$(FASLEXT)

strap-1/PI.$(FASLEXT): strap-1/OASGP.$(FASLEXT) \
	strap-1/MONOID.$(FASLEXT) strap-1/NNI.$(FASLEXT)

strap-1/NNI.$(FASLEXT): strap-1/OAMONS.$(FASLEXT) strap-1/INT.$(FASLEXT)

strap-1/INT.$(FASLEXT): strap-1/INS.$(FASLEXT) \
	strap-1/SUP.$(FASLEXT) strap-0/MATRIX.$(FASLEXT) \
	strap-1/VECTOR.$(FASLEXT)

strap-1/MAYBE.$(FASLEXT): strap-1/UTYPE.$(FASLEXT) \
	strap-1/RETRACT.$(FASLEXT) strap-1/KOERCE.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT)

strap-1/DFLOAT.$(FASLEXT): strap-1/FPS.$(FASLEXT) \
	strap-1/DIFRING.$(FASLEXT) strap-1/TRANFUN.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT)

strap-1/FLOAT.$(FASLEXT): strap-1/FPS.$(FASLEXT) \
	strap-1/DIFRING.$(FASLEXT) strap-1/TRANFUN.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/KVTFROM.$(FASLEXT)

strap-1/SUP.$(FASLEXT): strap-1/UPOLYC.$(FASLEXT) strap-1/PR.$(FASLEXT) \
	strap-1/FPC.$(FASLEXT) strap-1/FFIELDC.$(FASLEXT)

strap-1/PR.$(FASLEXT): strap-1/FAMR.$(FASLEXT) strap-1/FM.$(FASLEXT)

strap-1/FM.$(FASLEXT): strap-1/IDPAG.$(FASLEXT)

strap-1/IDPAG.$(FASLEXT): strap-1/ABELGRP.$(FASLEXT) \
	strap-1/IDPAM.$(FASLEXT)

strap-1/IDPAM.$(FASLEXT): strap-1/ABELMON.$(FASLEXT)  \
	strap-1/IDPC.$(FASLEXT) strap-1/IDPO.$(FASLEXT)

strap-1/IDPT.$(FASLEXT): strap-1/PAIR.$(FASLEXT)
strap-1/IDPO.$(FASLEXT): strap-1/PAIR.$(FASLEXT)

strap-1/PAIR.$(FASLEXT): strap-1/KOERCE.$(FASLEXT) \
	strap-1/SETCAT.$(FASLEXT) strap-0/OUTFORM.$(FASLEXT)

strap-1/MATRIX.$(FASLEXT): strap-1/MATCAT.$(FASLEXT) \
	strap-1/IARRAY2.$(FASLEXT)

strap-1/IARRAY2.$(FASLEXT): strap-1/ARR2CAT.$(FASLEXT) \
	strap-1/PRIMARR.$(FASLEXT) strap-1/STRING.$(FASLEXT)

strap-1/PRIMARR.$(FASLEXT): strap-1/A1AGG.$(FASLEXT)

strap-1/LIST.$(FASLEXT): strap-1/KOERCE.$(FASLEXT) \
	strap-1/SETCAT.$(FASLEXT) strap-1/KONVERT.$(FASLEXT) \
	strap-1/LSAGG.$(FASLEXT)

strap-1/VECTOR.$(FASLEXT): strap-1/VECTCAT.$(FASLEXT) \
	strap-1/ARRAY1.$(FASLEXT)

strap-1/ARRAY1.$(FASLEXT): strap-1/A1AGG.$(FASLEXT)

strap-1/IARRAY1.$(FASLEXT): strap-1/A1AGG.$(FASLEXT) \
	strap-0/PRIMARR.$(FASLEXT)

strap-1/FRAC.$(FASLEXT): strap-1/QFCAT.$(FASLEXT)

strap-1/STRING.$(FASLEXT): strap-1/SRAGG.$(FASLEXT) strap-1/CHAR.$(FASLEXT)

strap-1/CHAR.$(FASLEXT): strap-1/ORDFIN.$(FASLEXT) strap-1/CCLASS.$(FASLEXT)

strap-1/CCLASS.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/KOERCE.$(FASLEXT) \
	strap-1/FSAGG.$(FASLEXT) strap-1/KONVERT.$(FASLEXT) \
	strap-1/IBITS.$(FASLEXT)

strap-1/TABLE.$(FASLEXT): strap-1/TBAGG.$(FASLEXT)

strap-1/IBITS.$(FASLEXT): strap-1/BTAGG.$(FASLEXT)

strap-1/CTOR.$(FASLEXT): strap-1/CTORCAT.$(FASLEXT) \
	strap-1/IDENT.$(FASLEXT) strap-1/MAYBE.$(FASLEXT)

strap-1/CATCTOR.$(FASLEXT): strap-1/CTORCAT.$(FASLEXT) \
	strap-1/KOERCE.$(FASLEXT) strap-1/CTOR.$(FASLEXT)

strap-1/SAOS.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-0/SYMBOL.$(FASLEXT)

strap-1/STREAM.$(FASLEXT): strap-1/LZSTAGG.$(FASLEXT) strap-1/SMAGG.$(FASLEXT)

strap-1/LZSTAGG.$(FASLEXT): strap-1/STAGG.$(FASLEXT)

strap-1/BOP.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) strap-1/OPERCAT.$(FASLEXT) \
	strap-0/SYMBOL.$(FASLEXT) strap-0/NNI.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT) strap-0/MAYBE.$(FASLEXT) \
	strap-0/LIST.$(FASLEXT) strap-0/ALIST.$(FASLEXT) \
	strap-0/OUTFORM.$(FASLEXT) strap-0/INFORM.$(FASLEXT)

strap-1/ES.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) strap-1/RETRACT.$(FASLEXT) \
	strap-1/IEVALAB.$(FASLEXT) strap-0/BOP.$(FASLEXT) \
	strap-0/SYMBOL.$(FASLEXT) strap-0/KERNEL.$(FASLEXT)

strap-1/EQ.$(FASLEXT): strap-1/FUNCTOR.$(FASLEXT) strap-1/IEVALAB.$(FASLEXT) \
	strap-1/SETCAT.$(FASLEXT) strap-1/KOERCE.$(FASLEXT) \
	strap-1/ABELSG.$(FASLEXT) strap-1/ABELGRP.$(FASLEXT) \
	strap-1/MONOID.$(FASLEXT) strap-1/GROUP.$(FASLEXT) \
	strap-1/BMODULE.$(FASLEXT) strap-1/COMRING.$(FASLEXT) \
	strap-1/VSPACE.$(FASLEXT) strap-1/ES.$(FASLEXT)

# Bootstrap stage 2 dependencies.
# At this stage we recompile everything from stage 1, including
# a few more domains.

strap-2/FUNCTOR.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/KOERCE.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/KONVERT.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/RETRACT.$(FASLEXT): strap-2/KRCFROM.$(FASLEXT)
strap-2/BASTYPE.$(FASLEXT): strap-2/TYPE.$(FASLEXT) \
	strap-1/BOOLEAN.$(FASLEXT) strap-1/KOERCE.$(FASLEXT)
strap-2/FRETRCT.$(FASLEXT): strap-2/RETRACT.$(FASLEXT) strap-1/INS.$(FASLEXT)
strap-2/SETCAT.$(FASLEXT): strap-2/BASTYPE.$(FASLEXT) \
	strap-2/KOERCE.$(FASLEXT) strap-1/SINT.$(FASLEXT) \
	strap-1/STRING.$(FASLEXT)
strap-2/FINITE.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-1/PI.$(FASLEXT) strap-1/NNI.$(FASLEXT)
strap-2/ORDTYPE.$(FASLEXT): strap-2/BASTYPE.$(FASLEXT)
strap-2/ORDSET.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/ORDTYPE.$(FASLEXT)
strap-2/ORDFIN.$(FASLEXT): strap-2/ORDSET.$(FASLEXT) \
	strap-2/FINITE.$(FASLEXT)
strap-2/LOGIC.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/BOOLE.$(FASLEXT): strap-2/LOGIC.$(FASLEXT)
strap-2/PROPLOG.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/BOOLE.$(FASLEXT)
strap-2/LLINSET.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)
strap-2/RLINSET.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/LINSET.$(FASLEXT): strap-2/LLINSET.$(FASLEXT) \
	strap-2/RLINSET.$(FASLEXT)
strap-2/SGROUP.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/ABELSG.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-1/PI.$(FASLEXT)
strap-2/MONOID.$(FASLEXT): strap-2/SGROUP.$(FASLEXT)
strap-2/ABELMON.$(FASLEXT): strap-2/ABELSG.$(FASLEXT)
strap-2/CABMON.$(FASLEXT): strap-2/ABELMON.$(FASLEXT)
strap-2/OCAMON.$(FASLEXT): strap-2/OAMON.$(FASLEXT) \
	strap-2/CABMON.$(FASLEXT)
strap-2/OAMON.$(FASLEXT): strap-2/OASGP.$(FASLEXT) \
	strap-2/ABELMON.$(FASLEXT)
strap-2/OASGP.$(FASLEXT): strap-2/ORDSET.$(FASLEXT)
strap-2/ABELGRP.$(FASLEXT): strap-2/CABMON.$(FASLEXT) \
	strap-2/LLINSET.$(FASLEXT)
strap-2/OAGROUP.$(FASLEXT): strap-2/OCAMON.$(FASLEXT) \
	strap-2/ABELGRP.$(FASLEXT)
strap-2/RNG.$(FASLEXT): strap-2/ABELGRP.$(FASLEXT) \
	strap-2/SGROUP.$(FASLEXT)
strap-2/LMODULE.$(FASLEXT): strap-2/ABELGRP.$(FASLEXT) \
	strap-2/LLINSET.$(FASLEXT)
strap-2/RMODULE.$(FASLEXT): strap-2/ABELGRP.$(FASLEXT) \
	strap-2/RLINSET.$(FASLEXT)
strap-2/BMODULE.$(FASLEXT): strap-2/LMODULE.$(FASLEXT) \
	strap-2/RMODULE.$(FASLEXT)
strap-2/SRING.$(FASLEXT): strap-2/ABELMON.$(FASLEXT) \
	strap-2/MONOID.$(FASLEXT)
strap-2/RING.$(FASLEXT): strap-2/RNG.$(FASLEXT) \
	strap-2/SRING.$(FASLEXT) \
	strap-2/LMODULE.$(FASLEXT) strap-2/KRCFROM.$(FASLEXT)
strap-2/SRING.$(FASLEXT): strap-2/ABELMON.$(FASLEXT) \
	strap-2/MONOID.$(FASLEXT)
strap-2/DIOID.$(FASLEXT): strap-2/OAMON.$(FASLEXT) strap-2/SRING.$(FASLEXT)
strap-2/ORDRING.$(FASLEXT): strap-2/OAGROUP.$(FASLEXT) \
	strap-2/CHARZ.$(FASLEXT) strap-2/MONOID.$(FASLEXT)
strap-2/COMRING.$(FASLEXT): strap-2/RING.$(FASLEXT) \
	strap-2/BMODULE.$(FASLEXT)
strap-2/MODULE.$(FASLEXT): strap-2/COMRING.$(FASLEXT) \
	strap-2/BMODULE.$(FASLEXT) strap-2/LINSET.$(FASLEXT)
strap-2/INTDOM.$(FASLEXT): strap-2/COMRING.$(FASLEXT) \
	strap-2/ALGEBRA.$(FASLEXT) strap-2/ENTIRER.$(FASLEXT) \
	strap-1/FIELD.$(FASLEXT)
strap-2/GCDDOM.$(FASLEXT): strap-2/INTDOM.$(FASLEXT) \
	strap-1/LIST.$(FASLEXT) strap-1/SUP.$(FASLEXT) \
	strap-1/LSAGG.$(FASLEXT) strap-1/PFECAT.$(FASLEXT)
strap-2/ENTIRER.$(FASLEXT): strap-2/RING.$(FASLEXT) \
	strap-2/BMODULE.$(FASLEXT)
strap-2/OINTDOM.$(FASLEXT): strap-2/INTDOM.$(FASLEXT) \
	strap-2/ORDRING.$(FASLEXT)
strap-2/ALGEBRA.$(FASLEXT): strap-2/RING.$(FASLEXT) \
	strap-2/MODULE.$(FASLEXT) strap-2/KOERCE.$(FASLEXT)
strap-2/EUCDOM.$(FASLEXT): strap-2/PID.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT) strap-1/LSAGG.$(FASLEXT)
strap-2/PID.$(FASLEXT): strap-2/GCDDOM.$(FASLEXT) \
	strap-2/MAYBE.$(FASLEXT)
strap-2/OINTDOM.$(FASLEXT): strap-2/INTDOM.$(FASLEXT) \
	strap-2/ORDRING.$(FASLEXT)
strap-2/DIVRING.$(FASLEXT): strap-2/ENTIRER.$(FASLEXT) \
	strap-2/ALGEBRA.$(FASLEXT) strap-2/FRAC.$(FASLEXT)
strap-2/FIELD.$(FASLEXT): strap-2/EUCDOM.$(FASLEXT) \
	strap-2/UFD.$(FASLEXT) strap-2/DIVRING.$(FASLEXT)
strap-2/DIFFDOM.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/DIFFSPC.$(FASLEXT): strap-2/DIFFDOM.$(FASLEXT)
strap-2/DIFRING.$(FASLEXT): strap-2/RING.$(FASLEXT) \
	strap-2/DIFFSPC.$(FASLEXT)
strap-2/DSEXT.$(FASLEXT): strap-2/DIFFSPC.$(FASLEXT) \
	strap-2/PDSPC.$(FASLEXT)
strap-2/PDDOM.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/PDSPC.$(FASLEXT): strap-2/PDDOM.$(FASLEXT)
strap-2/PDRING.$(FASLEXT): strap-2/PDSPC.$(FASLEXT)
strap-2/PDRING.$(FASLEXT): strap-2/PDSPC.$(FASLEXT)
strap-2/DIFEXT.$(FASLEXT): strap-2/DSEXT.$(FASLEXT) \
	strap-2/DIFRING.$(FASLEXT) strap-2/PDRING.$(FASLEXT)
strap-2/LINEXP.$(FASLEXT): strap-2/LMODULE.$(FASLEXT)
strap-2/CFCAT.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/REAL.$(FASLEXT): strap-2/KONVERT.$(FASLEXT) \
	strap-1/DFLOAT.$(FASLEXT) strap-1/FLOAT.$(FASLEXT)
strap-2/CHARZ.$(FASLEXT): strap-2/RING.$(FASLEXT)
strap-2/STEP.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/PATMAB.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/UFD.$(FASLEXT): strap-2/GCDDOM.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT)
strap-2/INS.$(FASLEXT): strap-2/UFD.$(FASLEXT) strap-2/EUCDOM.$(FASLEXT) \
	strap-2/OINTDOM.$(FASLEXT) strap-2/DIFRING.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/RETRACT.$(FASLEXT) \
	strap-2/LINEXP.$(FASLEXT) strap-2/CFCAT.$(FASLEXT) \
	strap-2/REAL.$(FASLEXT) strap-2/CHARZ.$(FASLEXT) \
	strap-2/STEP.$(FASLEXT) strap-2/PATMAB.$(FASLEXT) \
	strap-2/FEVALAB.$(FASLEXT) strap-1/FRAC.$(FASLEXT) \
	strap-1/FLOAT.$(FASLEXT) strap-1/DFLOAT.$(FASLEXT) \
	strap-1/INFORM.$(FASLEXT) strap-1/SEXCAT.$(FASLEXT) \
	strap-1/PATTERN.$(FASLEXT)
strap-2/FPS.$(FASLEXT): strap-2/RNS.$(FASLEXT)
strap-2/RNS.$(FASLEXT): strap-2/FIELD.$(FASLEXT) strap-2/ORDRING.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-2/RADCAT.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/PATMAB.$(FASLEXT) \
	strap-2/CHARZ.$(FASLEXT)
strap-2/RADCAT.$(FASLEXT): strap-1/INS.$(FASLEXT)
strap-2/PFECAT.$(FASLEXT): strap-2/UFD.$(FASLEXT) strap-2/CHARZ.$(FASLEXT)
strap-2/QFCAT.$(FASLEXT): strap-2/ALGEBRA.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) \
	strap-2/FEVALAB.$(FASLEXT) strap-2/DIFEXT.$(FASLEXT) \
	strap-2/FLINEXP.$(FASLEXT) strap-2/PATAB.$(FASLEXT) \
	strap-2/FPATMAB.$(FASLEXT) strap-2/ORDSET.$(FASLEXT) \
	strap-2/OINTDOM.$(FASLEXT) strap-2/REAL.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/CHARZ.$(FASLEXT) \
	strap-2/CHARNZ.$(FASLEXT) strap-2/EUCDOM.$(FASLEXT) \
	strap-2/PFECAT.$(FASLEXT) strap-1/FIELD.$(FASLEXT) 

strap-2/FEVALAB.$(FASLEXT): strap-2/FUNCTOR.$(FASLEXT) \
	strap-2/EVALAB.$(FASLEXT) strap-2/IEVALAB.$(FASLEXT) \
	strap-2/ELTAB.$(FASLEXT)

strap-2/CHARNZ.$(FASLEXT): strap-2/RING.$(FASLEXT)

strap-2/FPATMAB.$(FASLEXT): strap-2/PATMAB.$(FASLEXT)

strap-2/PATAB.$(FASLEXT): strap-2/KONVERT.$(FASLEXT)

strap-2/FLINEXP.$(FASLEXT): strap-2/LINEXP.$(FASLEXT) \
	strap-1/MATRIX.$(FASLEXT)

strap-2/TRANFUN.$(FASLEXT): strap-2/TRIGCAT.$(FASLEXT) \
	strap-2/ATRIG.$(FASLEXT) strap-2/HYPCAT.$(FASLEXT) \
	strap-2/AHYP.$(FASLEXT) strap-2/ELEMFUN.$(FASLEXT)
strap-2/TRIGCAT.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-1/RING.$(FASLEXT)
strap-2/ATRIG.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-1/RING.$(FASLEXT)
strap-2/HYPCAT.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-1/RING.$(FASLEXT)
strap-2/AHYP.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/ELEMFUN.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-1/MONOID.$(FASLEXT)
strap-2/IDPC.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) strap-2/FUNCTOR.$(FASLEXT)
strap-2/AMR.$(FASLEXT): strap-2/RING.$(FASLEXT) strap-2/BMODULE.$(FASLEXT) \
	strap-2/ALGEBRA.$(FASLEXT) strap-2/FIELD.$(FASLEXT) \
	strap-2/CHARZ.$(FASLEXT) strap-2/CHARNZ.$(FASLEXT) \
	strap-2/FUNCTOR.$(FASLEXT)
strap-2/FAMR.$(FASLEXT): strap-2/AMR.$(FASLEXT) \
	strap-2/FRETRCT.$(FASLEXT)
strap-2/POLYCAT.$(FASLEXT): strap-2/PDRING.$(FASLEXT) \
	strap-2/FAMR.$(FASLEXT) strap-2/EVALAB.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-2/FLINEXP.$(FASLEXT) \
	strap-2/VECTOR.$(FASLEXT) strap-2/MATRIX.$(FASLEXT) \
	strap-2/LIST.$(FASLEXT) strap-1/SUP.$(FASLEXT) \
	strap-2/INFORM.$(FASLEXT)
strap-2/UPOLYC.$(FASLEXT): strap-2/POLYCAT.$(FASLEXT) \
	strap-2/ELTAB.$(FASLEXT) strap-2/DIFRING.$(FASLEXT) \
	strap-2/DIFEXT.$(FASLEXT) strap-2/STEP.$(FASLEXT) \
	strap-2/COMRING.$(FASLEXT) strap-2/INTDOM.$(FASLEXT) \
	strap-2/FIELD.$(FASLEXT) strap-2/ALGEBRA.$(FASLEXT)
strap-2/ARR2CAT.$(FASLEXT): strap-2/FINAGG.$(FASLEXT) strap-2/SMAGG.$(FASLEXT)
strap-2/FSAGG.$(FASLEXT): strap-2/KDAGG.$(FASLEXT) \
	strap-2/SETAGG.$(FASLEXT) strap-2/FINAGG.$(FASLEXT)
strap-2/ALAGG.$(FASLEXT): strap-2/TBAGG.$(FASLEXT) strap-2/LSAGG.$(FASLEXT)
strap-2/LSAGG.$(FASLEXT): strap-2/STAGG.$(FASLEXT) \
	strap-2/FLAGG.$(FASLEXT) strap-2/ELAGG.$(FASLEXT)
strap-2/TBAGG.$(FASLEXT): strap-2/KDAGG.$(FASLEXT)
strap-2/KDAGG.$(FASLEXT): strap-2/DIAGG.$(FASLEXT) strap-2/IXAGG.$(FASLEXT)
strap-2/DIAGG.$(FASLEXT): strap-2/DIOPS.$(FASLEXT)
strap-2/DIOPS.$(FASLEXT): strap-2/BGAGG.$(FASLEXT) strap-2/CLAGG.$(FASLEXT)
strap-2/SMAGG.$(FASLEXT): strap-2/HOAGG.$(FASLEXT)
strap-2/BGAGG.$(FASLEXT): strap-2/SMAGG.$(FASLEXT)
strap-2/STAGG.$(FASLEXT): strap-2/URAGG.$(FASLEXT)
strap-2/URAGG.$(FASLEXT): strap-2/RCAGG.$(FASLEXT)
strap-2/RCAGG.$(FASLEXT): strap-2/HOAGG.$(FASLEXT)
strap-2/ELAGG.$(FASLEXT): strap-2/LNAGG.$(FASLEXT) strap-2/SMAGG.$(FASLEXT)
strap-2/SRAGG.$(FASLEXT): strap-2/A1AGG.$(FASLEXT)
strap-2/A1AGG.$(FASLEXT): strap-2/FLAGG.$(FASLEXT) strap-2/SMAGG.$(FASLEXT)
strap-2/FLAGG.$(FASLEXT): strap-2/LNAGG.$(FASLEXT) strap-2/FINAGG.$(FASLEXT)
strap-2/FINAGG.$(FASLEXT): strap-2/HOAGG.$(FASLEXT)
strap-2/LNAGG.$(FASLEXT): strap-2/IXAGG.$(FASLEXT) \
	strap-2/CLAGG.$(FASLEXT) strap-2/ELTAB.$(FASLEXT)
strap-2/CLAGG.$(FASLEXT): strap-2/HOAGG.$(FASLEXT)
strap-2/IXAGG.$(FASLEXT): strap-2/HOAGG.$(FASLEXT) \
	strap-2/ELTAGG.$(FASLEXT)
strap-2/HOAGG.$(FASLEXT): strap-2/AGG.$(FASLEXT) strap-2/FUNCTOR.$(FASLEXT) \
	strap-2/KOERCE.$(FASLEXT) strap-2/BASTYPE.$(FASLEXT) \
	strap-2/SETCAT.$(FASLEXT) strap-2/EVALAB.$(FASLEXT)
strap-2/SETAGG.$(FASLEXT): strap-2/CLAGG.$(FASLEXT) strap-2/SETCAT.$(FASLEXT)
strap-2/AGG.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-1/INS.$(FASLEXT)
strap-2/ELTAGG.$(FASLEXT): strap-2/AGG.$(FASLEXT) strap-2/ELTAB.$(FASLEXT)
strap-2/ELTAB.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/EVALAB.$(FASLEXT): strap-2/IEVALAB.$(FASLEXT) \
	strap-1/EQ.$(FASLEXT) strap-1/LIST.$(FASLEXT) \
	strap-1/GROUP.$(FASLEXT) strap-1/VSPACE.$(FASLEXT)
strap-2/IEVALAB.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/LIST.$(FASLEXT)
strap-2/SEXCAT.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/KVTFROM.$(FASLEXT) strap-2/ELTAB.$(FASLEXT)
strap-2/MATCAT.$(FASLEXT): strap-2/ARR2CAT.$(FASLEXT) \
	strap-2/FLAGG.$(FASLEXT) strap-2/FINAGG.$(FASLEXT)
strap-2/CATEGORY.$(FASLEXT): strap-2/KOERCE.$(FASLEXT) \
	strap-1/OUTFORM.$(FASLEXT) strap-1/LIST.$(FASLEXT) \
	strap-1/CTORCALL.$(FASLEXT) strap-1/CATCTOR.$(FASLEXT)
strap-2/SINT.$(FASLEXT): strap-2/INS.$(FASLEXT) strap-2/INT.$(FASLEXT)
strap-2/PI.$(FASLEXT): strap-2/OASGP.$(FASLEXT) \
	strap-2/MONOID.$(FASLEXT) strap-2/NNI.$(FASLEXT)
strap-2/NNI.$(FASLEXT): strap-2/OAMONS.$(FASLEXT) strap-2/INT.$(FASLEXT)
strap-2/OAMONS.$(FASLEXT): strap-2/OCAMON.$(FASLEXT)
strap-2/INT.$(FASLEXT): strap-2/INS.$(FASLEXT) \
	strap-2/SUP.$(FASLEXT) strap-2/MATRIX.$(FASLEXT) \
	strap-2/VECTOR.$(FASLEXT) strap-2/DFLOAT.$(FASLEXT) \
	strap-2/FLOAT.$(FASLEXT)
strap-2/DFLOAT.$(FASLEXT): strap-2/FPS.$(FASLEXT) \
	strap-2/DIFRING.$(FASLEXT) strap-2/TRANFUN.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT)
strap-2/FLOAT.$(FASLEXT): strap-2/FPS.$(FASLEXT) \
	strap-2/DIFRING.$(FASLEXT) strap-2/TRANFUN.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/KVTFROM.$(FASLEXT)
strap-2/SUP.$(FASLEXT): strap-2/UPOLYC.$(FASLEXT) strap-2/PR.$(FASLEXT) \
	strap-2/FPC.$(FASLEXT) strap-2/FFIELDC.$(FASLEXT)
strap-2/FFIELDC.$(FASLEXT): strap-1/FPC.$(FASLEXT)
strap-2/FPC.$(FASLEXT): strap-1/FIELD.$(FASLEXT)
strap-2/PR.$(FASLEXT): strap-2/FAMR.$(FASLEXT) strap-2/FM.$(FASLEXT)
strap-2/FM.$(FASLEXT): strap-2/IDPAG.$(FASLEXT)
strap-2/IDPAG.$(FASLEXT): strap-2/ABELGRP.$(FASLEXT) \
	strap-2/IDPAM.$(FASLEXT)
strap-2/IDPAM.$(FASLEXT): strap-2/ABELMON.$(FASLEXT)  \
	strap-2/IDPC.$(FASLEXT) strap-2/IDPO.$(FASLEXT)
strap-2/IDPO.$(FASLEXT): strap-2/IDPC.$(FASLEXT)
strap-2/VECTOR.$(FASLEXT): strap-2/VECTCAT.$(FASLEXT) \
	strap-2/ARRAY1.$(FASLEXT)
strap-2/ARRAY1.$(FASLEXT): strap-1/A1AGG.$(FASLEXT)
strap-2/VECTCAT.$(FASLEXT): strap-1/A1AGG.$(FASLEXT)
strap-2/IARRAY1.$(FASLEXT): strap-2/A1AGG.$(FASLEXT) \
	strap-2/PRIMARR.$(FASLEXT)
strap-2/PRIMARR.$(FASLEXT): strap-2/A1AGG.$(FASLEXT)
strap-2/PAIR.$(FASLEXT): strap-2/KOERCE.$(FASLEXT) \
	strap-2/SETCAT.$(FASLEXT) strap-0/OUTFORM.$(FASLEXT)
strap-2/MATRIX.$(FASLEXT): strap-2/MATCAT.$(FASLEXT) \
	strap-2/IARRAY2.$(FASLEXT)
strap-2/IARRAY2.$(FASLEXT): strap-2/ARR2CAT.$(FASLEXT) \
	strap-2/PRIMARR.$(FASLEXT) strap-1/STRING.$(FASLEXT)
strap-2/INFORM.$(FASLEXT): strap-2/SEXCAT.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/ANY.$(FASLEXT) \
	strap-2/SEX.$(FASLEXT) strap-2/LIST.$(FASLEXT) \
	strap-2/SYMBOL.$(FASLEXT)
strap-2/LIST.$(FASLEXT): strap-2/LSAGG.$(FASLEXT)
strap-2/SEX.$(FASLEXT): strap-2/SEXOF.$(FASLEXT)
strap-2/OUTFORM.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/SEXOF.$(FASLEXT): strap-2/SEXCAT.$(FASLEXT)
strap-2/ANY.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/SEX.$(FASLEXT) strap-2/NONE.$(FASLEXT)
strap-2/NONE.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/SYMBOL.$(FASLEXT): strap-2/ORDSET.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/KRCFROM.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-2/KONVERT.$(FASLEXT) \
	strap-2/PATMAB.$(FASLEXT) strap-2/VOID.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT) strap-2/ALIST.$(FASLEXT) \
	strap-2/PRIMARR.$(FASLEXT) strap-2/REF.$(FASLEXT) \
	strap-2/STRING.$(FASLEXT)
strap-2/ALIST.$(FASLEXT): strap-2/ALAGG.$(FASLEXT) strap-2/REF.$(FASLEXT)
strap-2/REF.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/STRING.$(FASLEXT): strap-2/SRAGG.$(FASLEXT) \
	strap-2/CHAR.$(FASLEXT)

strap-2/FRAC.$(FASLEXT): strap-2/QFCAT.$(FASLEXT) strap-1/GCDDOM.$(FASLEXT) \
	strap-1/LA.$(FASLEXT) strap-1/EUCDOM.$(FASLEXT) \
	strap-1/INS.$(FASLEXT) strap-1/SUP.$(FASLEXT) \
	strap-1/PFECAT.$(FASLEXT) strap-1/FR.$(FASLEXT) \
	strap-1/CHARNZ.$(FASLEXT) strap-1/SEGCAT.$(FASLEXT) \
	strap-1/SEGXCAT.$(FASLEXT)

strap-2/CHAR.$(FASLEXT): strap-2/ORDFIN.$(FASLEXT) strap-2/CCLASS.$(FASLEXT)
strap-2/CCLASS.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/KOERCE.$(FASLEXT) \
	strap-2/FSAGG.$(FASLEXT) strap-2/KONVERT.$(FASLEXT) \
	strap-2/IBITS.$(FASLEXT)
strap-2/TABLE.$(FASLEXT): strap-2/TBAGG.$(FASLEXT)
strap-2/IBITS.$(FASLEXT): strap-2/BTAGG.$(FASLEXT)
strap-2/MAYBE.$(FASLEXT): strap-2/UTYPE.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-2/KOERCE.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT)
strap-2/BOOLEAN.$(FASLEXT): strap-2/PROPLOG.$(FASLEXT) \
	strap-2/ORDFIN.$(FASLEXT) strap-2/KONVERT.$(FASLEXT) \
	strap-1/PI.$(FASLEXT)

strap-2/EQ.$(FASLEXT): strap-2/FUNCTOR.$(FASLEXT) strap-2/SETCAT.$(FASLEXT) \
	strap-2/KOERCE.$(FASLEXT) strap-2/EVALAB.$(FASLEXT) \
	strap-2/ABELSG.$(FASLEXT) strap-2/ABELGRP.$(FASLEXT) \
	strap-2/SGROUP.$(FASLEXT) strap-2/MONOID.$(FASLEXT) \
	strap-2/RING.$(FASLEXT) strap-2/BMODULE.$(FASLEXT) \
	strap-2/COMRING.$(FASLEXT) strap-2/MODULE.$(FASLEXT) \
	strap-2/INTDOM.$(FASLEXT) strap-2/PDRING.$(FASLEXT) \
	strap-2/FIELD.$(FASLEXT) strap-2/VSPACE.$(FASLEXT) \
	strap-2/ES.$(FASLEXT) strap-2/GROUP.$(FASLEXT)

strap-2/ES.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) strap-2/RETRACT.$(FASLEXT) \
	strap-2/IEVALAB.$(FASLEXT) strap-2/EVALAB.$(FASLEXT) \
	strap-2/RING.$(FASLEXT) strap-2/RETRACT.$(FASLEXT) \
	strap-2/CACHSET.$(FASLEXT)

strap-2/BTAGG.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) \
	strap-1/BOOLE.$(FASLEXT) strap-1/LOGIC.$(FASLEXT) \
	strap-1/A1AGG.$(FASLEXT)

strap-2/UNISEG.$(FASLEXT): strap-2/SEGCAT.$(FASLEXT) \
	strap-2/SEGXCAT.$(FASLEXT) strap-2/ORDSET.$(FASLEXT) \
	strap-2/SETCAT.$(FASLEXT) strap-1/STREAM.$(FASLEXT)

strap-2/PATTERN.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-1/BOP.$(FASLEXT) \
	strap-1/NNI.$(FASLEXT) strap-1/ABELMON.$(FASLEXT)

#
#

.PRECIOUS: strap-0 strap-1 strap-2
strap-0 strap-1 strap-2:
	$(MKDIR_P) $@

.PRECIOUS: strap-0/%.$(FASLEXT)
strap-0/%.$(FASLEXT): %.spad initdb.$(FASLEXT) | strap-0
	$(BOOTSTRAP) --sysalg=strap-0 --output=$@ --bootstrap $<

.PRECIOUS: strap-1/%.$(FASLEXT)
strap-1/%.$(FASLEXT): %.spad initdb.$(FASLEXT) | strap-1
	$(BOOTSTRAP) --strap=strap-0 --sysalg=strap-1 \
		--output=$@ --optimize=3 $<

.PRECIOUS: strap-2/%.$(FASLEXT)
strap-2/%.$(FASLEXT): %.spad initdb.$(FASLEXT) | strap-2
	$(BOOTSTRAP) --strap=strap-1 --sysalg=strap-2 \
		--output=$@ --optimize=3 $<


SPADFILES= \
 $(OUTSRC)/acplot.spad $(OUTSRC)/aggcat2.spad $(OUTSRC)/aggcat.spad \
 $(OUTSRC)/algcat.spad $(OUTSRC)/algext.spad $(OUTSRC)/algfact.spad \
 $(OUTSRC)/algfunc.spad $(OUTSRC)/allfact.spad $(OUTSRC)/alql.spad \
 $(OUTSRC)/any.spad $(OUTSRC)/array1.spad \
 $(OUTSRC)/array2.spad $(OUTSRC)/attreg.spad \
 $(OUTSRC)/bags.spad $(OUTSRC)/bezout.spad $(OUTSRC)/boolean.spad \
 $(OUTSRC)/brill.spad $(OUTSRC)/domain.spad \
 $(OUTSRC)/card.spad $(OUTSRC)/carten.spad $(OUTSRC)/catdef.spad \
 $(OUTSRC)/cden.spad $(OUTSRC)/clifford.spad $(OUTSRC)/clip.spad \
 $(OUTSRC)/cmplxrt.spad $(OUTSRC)/coerce.spad $(OUTSRC)/color.spad \
 $(OUTSRC)/combfunc.spad $(OUTSRC)/combinat.spad $(OUTSRC)/complet.spad \
 $(OUTSRC)/constant.spad $(OUTSRC)/contfrac.spad \
 $(OUTSRC)/coordsys.spad $(OUTSRC)/cra.spad $(OUTSRC)/crfp.spad \
 $(OUTSRC)/curve.spad $(OUTSRC)/cycles.spad $(OUTSRC)/cyclotom.spad \
 $(OUTSRC)/ddfact.spad \
 $(OUTSRC)/defaults.spad $(OUTSRC)/defintef.spad $(OUTSRC)/defintrf.spad \
 $(OUTSRC)/degred.spad $(OUTSRC)/derham.spad $(OUTSRC)/dhmatrix.spad \
 $(OUTSRC)/divisor.spad $(OUTSRC)/dpolcat.spad $(OUTSRC)/drawopt.spad \
 $(OUTSRC)/drawpak.spad $(OUTSRC)/draw.spad \
 $(OUTSRC)/efstruc.spad $(OUTSRC)/efuls.spad $(OUTSRC)/efupxs.spad \
 $(OUTSRC)/eigen.spad $(OUTSRC)/elemntry.spad $(OUTSRC)/elfuts.spad \
 $(OUTSRC)/equation1.spad $(OUTSRC)/equation2.spad $(OUTSRC)/error.spad \
 $(OUTSRC)/expexpan.spad $(OUTSRC)/expr2ups.spad \
 $(OUTSRC)/exprode.spad $(OUTSRC)/expr.spad \
 $(OUTSRC)/facutil.spad $(OUTSRC)/ffcat.spad \
 $(OUTSRC)/ffcg.spad $(OUTSRC)/fff.spad $(OUTSRC)/ffhom.spad \
 $(OUTSRC)/ffnb.spad $(OUTSRC)/ffpoly2.spad $(OUTSRC)/ffpoly.spad \
 $(OUTSRC)/ffp.spad $(OUTSRC)/ffx.spad \
 $(OUTSRC)/files.spad $(OUTSRC)/float.spad $(OUTSRC)/fmod.spad \
 $(OUTSRC)/fname.spad $(OUTSRC)/fnla.spad \
 $(OUTSRC)/forttyp.spad $(OUTSRC)/fourier.spad \
 $(OUTSRC)/fparfrac.spad $(OUTSRC)/fraction.spad $(OUTSRC)/free.spad \
 $(OUTSRC)/fr.spad $(OUTSRC)/fs2expxp.spad $(OUTSRC)/fs2ups.spad \
 $(OUTSRC)/fspace.spad $(OUTSRC)/funcpkgs.spad \
 $(OUTSRC)/galfact.spad $(OUTSRC)/galfactu.spad $(OUTSRC)/galpolyu.spad \
 $(OUTSRC)/galutil.spad $(OUTSRC)/gaussfac.spad $(OUTSRC)/gaussian.spad \
 $(OUTSRC)/gbeuclid.spad $(OUTSRC)/gbintern.spad $(OUTSRC)/gb.spad \
 $(OUTSRC)/gdirprod.spad $(OUTSRC)/gdpoly.spad $(OUTSRC)/geneez.spad \
 $(OUTSRC)/generic.spad $(OUTSRC)/genufact.spad $(OUTSRC)/genups.spad \
 $(OUTSRC)/ghensel.spad $(OUTSRC)/gpgcd.spad $(OUTSRC)/gpol.spad \
 $(OUTSRC)/grdef.spad $(OUTSRC)/groebf.spad $(OUTSRC)/groebsol.spad \
 $(OUTSRC)/gseries.spad \
 $(OUTSRC)/ideal.spad $(OUTSRC)/idecomp.spad $(OUTSRC)/indexedp.spad \
 $(OUTSRC)/infprod.spad $(OUTSRC)/intaf.spad $(OUTSRC)/intalg.spad \
 $(OUTSRC)/intaux.spad $(OUTSRC)/intclos.spad $(OUTSRC)/intef.spad \
 $(OUTSRC)/integer.spad $(OUTSRC)/integrat.spad \
 $(OUTSRC)/interval.spad \
 $(OUTSRC)/intfact.spad $(OUTSRC)/intpm.spad \
 $(OUTSRC)/intrf.spad \
 $(OUTSRC)/irexpand.spad \
 $(OUTSRC)/irsn.spad $(OUTSRC)/ituple.spad \
 $(OUTSRC)/kl.spad $(OUTSRC)/kovacic.spad \
 $(OUTSRC)/laplace.spad $(OUTSRC)/laurent.spad $(OUTSRC)/leadcdet.spad \
 $(OUTSRC)/lie.spad $(OUTSRC)/limitps.spad $(OUTSRC)/lindep.spad \
 $(OUTSRC)/lingrob.spad $(OUTSRC)/liouv.spad $(OUTSRC)/listgcd.spad \
 $(OUTSRC)/list.spad $(OUTSRC)/lmdict.spad $(OUTSRC)/lodof.spad \
 $(OUTSRC)/lodop.spad $(OUTSRC)/lodo.spad $(OUTSRC)/logic.spad \
 $(OUTSRC)/manip.spad $(OUTSRC)/mappkg.spad $(OUTSRC)/matcat.spad \
 $(OUTSRC)/matfuns.spad $(OUTSRC)/mathml.spad \
 $(OUTSRC)/matrix.spad $(OUTSRC)/matstor.spad \
 $(OUTSRC)/mesh.spad $(OUTSRC)/mfinfact.spad $(OUTSRC)/misc.spad \
 $(OUTSRC)/mkfunc.spad $(OUTSRC)/mkrecord.spad \
 $(OUTSRC)/mlift.spad $(OUTSRC)/moddfact.spad $(OUTSRC)/modgcd.spad \
 $(OUTSRC)/modmonom.spad $(OUTSRC)/modmon.spad $(OUTSRC)/modring.spad \
 $(OUTSRC)/moebius.spad $(OUTSRC)/mring.spad $(OUTSRC)/mset.spad \
 $(OUTSRC)/mts.spad $(OUTSRC)/multfact.spad $(OUTSRC)/multpoly.spad \
 $(OUTSRC)/multsqfr.spad \
 $(OUTSRC)/naalgc.spad $(OUTSRC)/naalg.spad \
 $(OUTSRC)/newdata.spad $(OUTSRC)/newpoint.spad \
 $(OUTSRC)/newpoly.spad $(OUTSRC)/nlinsol.spad $(OUTSRC)/nlode.spad \
 $(OUTSRC)/npcoef.spad \
 $(OUTSRC)/nregset.spad \
 $(OUTSRC)/nsregset.spad $(OUTSRC)/numeigen.spad $(OUTSRC)/numeric.spad \
 $(OUTSRC)/numode.spad $(OUTSRC)/numquad.spad $(OUTSRC)/numsolve.spad \
 $(OUTSRC)/numtheor.spad \
 $(OUTSRC)/oct.spad $(OUTSRC)/odealg.spad $(OUTSRC)/odeef.spad \
 $(OUTSRC)/oderf.spad $(OUTSRC)/opalg.spad \
 $(OUTSRC)/op.spad $(OUTSRC)/ore.spad \
 $(OUTSRC)/outform.spad $(OUTSRC)/out.spad \
 $(OUTSRC)/pade.spad $(OUTSRC)/padiclib.spad $(OUTSRC)/padic.spad \
 $(OUTSRC)/paramete.spad $(OUTSRC)/partperm.spad $(OUTSRC)/patmatch1.spad \
 $(OUTSRC)/patmatch2.spad $(OUTSRC)/pattern.spad $(OUTSRC)/pcurve.spad \
 $(OUTSRC)/pdecomp.spad $(OUTSRC)/perman.spad $(OUTSRC)/permgrps.spad \
 $(OUTSRC)/perm.spad $(OUTSRC)/pfbr.spad $(OUTSRC)/pfo.spad \
 $(OUTSRC)/pfr.spad $(OUTSRC)/pf.spad $(OUTSRC)/pgcd.spad \
 $(OUTSRC)/pgrobner.spad $(OUTSRC)/pinterp.spad $(OUTSRC)/pleqn.spad \
 $(OUTSRC)/plot3d.spad $(OUTSRC)/plot.spad $(OUTSRC)/plottool.spad \
 $(OUTSRC)/polset.spad $(OUTSRC)/poltopol.spad $(OUTSRC)/polycat.spad \
 $(OUTSRC)/poly.spad $(OUTSRC)/primelt.spad $(OUTSRC)/print.spad \
 $(OUTSRC)/product.spad $(OUTSRC)/prs.spad $(OUTSRC)/prtition.spad \
 $(OUTSRC)/pscat.spad $(OUTSRC)/pseudolin.spad $(OUTSRC)/ptranfn.spad \
 $(OUTSRC)/puiseux.spad \
 $(OUTSRC)/qalgset.spad $(OUTSRC)/quat.spad \
 $(OUTSRC)/radeigen.spad $(OUTSRC)/radix.spad $(OUTSRC)/random.spad \
 $(OUTSRC)/ratfact.spad $(OUTSRC)/rdeef.spad $(OUTSRC)/rderf.spad \
 $(OUTSRC)/rdesys.spad $(OUTSRC)/real0q.spad $(OUTSRC)/realzero.spad \
 $(OUTSRC)/reclos.spad $(OUTSRC)/regset.spad $(OUTSRC)/rep1.spad \
 $(OUTSRC)/rep2.spad $(OUTSRC)/resring.spad $(OUTSRC)/retract.spad \
 $(OUTSRC)/rf.spad $(OUTSRC)/riccati.spad $(OUTSRC)/rinterp.spad \
 $(OUTSRC)/rule.spad \
 $(OUTSRC)/seg.spad $(OUTSRC)/setorder.spad $(OUTSRC)/sets.spad \
 $(OUTSRC)/sex.spad $(OUTSRC)/sf.spad $(OUTSRC)/sgcf.spad \
 $(OUTSRC)/sign.spad $(OUTSRC)/si.spad $(OUTSRC)/smith.spad \
 $(OUTSRC)/solvedio.spad $(OUTSRC)/solvefor.spad $(OUTSRC)/solvelin.spad \
 $(OUTSRC)/solverad.spad $(OUTSRC)/sortpak.spad $(OUTSRC)/space.spad \
 $(srcdir)/spad-parser.spad \
 $(OUTSRC)/special.spad $(OUTSRC)/sregset.spad $(OUTSRC)/s.spad \
 $(OUTSRC)/stream.spad $(OUTSRC)/string.spad $(OUTSRC)/sttaylor.spad \
 $(OUTSRC)/sttf.spad $(OUTSRC)/sturm.spad $(OUTSRC)/suchthat.spad \
 $(OUTSRC)/suls.spad $(OUTSRC)/sum.spad $(OUTSRC)/sups.spad \
 $(OUTSRC)/supxs.spad $(OUTSRC)/suts.spad $(OUTSRC)/symbol.spad \
 $(OUTSRC)/syntax.spad \
 $(OUTSRC)/syssolp.spad $(OUTSRC)/system.spad \
 $(OUTSRC)/tableau.spad $(OUTSRC)/table.spad $(OUTSRC)/taylor.spad \
 $(OUTSRC)/tex.spad $(OUTSRC)/tools.spad $(OUTSRC)/transsolve.spad \
 $(OUTSRC)/tree.spad $(OUTSRC)/trigcat.spad $(OUTSRC)/triset.spad \
 $(OUTSRC)/tube.spad $(OUTSRC)/twofact.spad \
 $(OUTSRC)/unifact.spad $(OUTSRC)/updecomp.spad $(OUTSRC)/updivp.spad \
 $(OUTSRC)/utsode.spad \
 $(OUTSRC)/variable.spad $(OUTSRC)/vector.spad $(OUTSRC)/view2D.spad \
 $(OUTSRC)/view3D.spad $(OUTSRC)/viewDef.spad $(OUTSRC)/viewpack.spad \
 $(OUTSRC)/void.spad \
 $(OUTSRC)/weier.spad $(OUTSRC)/wtpol.spad \
 $(OUTSRC)/xlpoly.spad $(OUTSRC)/xpoly.spad \
 $(OUTSRC)/ystream.spad \
 $(OUTSRC)/zerodim.spad


TESTS=$(INPUT)/INTHEORY.input $(INPUT)/VIEW2D.input $(INPUT)/TESTFR.input

# 
$(OUT)/SINT.$(FASLEXT): $(OUT)/INT.$(FASLEXT)
$(OUT)/NNI.$(FASLEXT): $(OUT)/INT.$(FASLEXT)
$(OUT)/PI.$(FASLEXT): $(OUT)/NNI.$(FASLEXT)
$(OUT)/INT.$(FASLEXT): $(OUT)/INS.$(FASLEXT) \
	strap-2/SUP.$(FASLEXT) strap-2/STRING.$(FASLEXT) \
	strap-2/MATRIX.$(FASLEXT) strap-2/VECTOR.$(FASLEXT) \
	strap-2/FR.$(FASLEXT)

$(OUT)/BINOPC.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/IEVALAB.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	strap-2/LIST.$(FASLEXT)

$(OUT)/EVALAB.$(FASLEXT): $(OUT)/IEVALAB.$(FASLEXT) \
	strap-2/EQ.$(FASLEXT) strap-2/LIST.$(FASLEXT)

$(OUT)/ELTAB.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/SETCAT.$(FASLEXT): $(OUT)/BASTYPE.$(FASLEXT) \
	strap-2/SINT.$(FASLEXT) strap-2/STRING.$(FASLEXT)

$(OUT)/UTYPE.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/AHYP.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/PRIMCAT.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/ATTREG.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/CONDUIT.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/CFCAT.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/SEGCAT.$(FASLEXT): $(OUT)/KRCFROM.$(FASLEXT)

$(OUT)/KOERCE.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/KRCFROM.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/KONVERT.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/KVTFROM.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/RETRACT.$(FASLEXT): $(OUT)/KRCFROM.$(FASLEXT)

$(OUT)/BASTYPE.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT)

$(OUT)/SETCAT.$(FASLEXT): $(OUT)/BASTYPE.$(FASLEXT)

$(OUT)/FINITE.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	strap-2/NNI.$(FASLEXT) strap-2/PI.$(FASLEXT)

$(OUT)/ORDSET.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	$(OUT)/ORDTYPE.$(FASLEXT)
$(OUT)/FILECAT.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/STEP.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/PATMAB.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/REAL.$(FASLEXT): $(OUT)/KONVERT.$(FASLEXT)

$(OUT)/LOGIC.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/BOOLE.$(FASLEXT): $(OUT)/LOGIC.$(FASLEXT)

$(OUT)/SGROUP.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	strap-2/PI.$(FASLEXT)
$(OUT)/ABELSG.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/LMODULE.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	$(OUT)/LLINSET.$(FASLEXT) $(OUT)/SGROUP.$(FASLEXT)
$(OUT)/RMODULE.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) $(OUT)/RLINSET.$(FASLEXT)
$(OUT)/BMODULE.$(FASLEXT): $(OUT)/LMODULE.$(FASLEXT) \
	$(OUT)/RMODULE.$(FASLEXT) $(OUT)/SGROUP.$(FASLEXT)
$(OUT)/MODULE.$(FASLEXT): $(OUT)/BMODULE.$(FASLEXT) $(OUT)/LINSET.$(FASLEXT)
$(OUT)/ALGEBRA.$(FASLEXT): $(OUT)/MODULE.$(FASLEXT)
$(OUT)/OAGROUP.$(FASLEXT): $(OUT)/OCAMON.$(FASLEXT)
$(OUT)/OCAMON.$(FASLEXT): $(OUT)/OAMON.$(FASLEXT)
$(OUT)/OAMON.$(FASLEXT): $(OUT)/OASGP.$(FASLEXT)
$(OUT)/OAMONS.$(FASLEXT): $(OUT)/OAMON.$(FASLEXT)
$(OUT)/OASGP.$(FASLEXT): $(OUT)/ORDSET.$(FASLEXT) $(OUT)/ABELSG.$(FASLEXT)
$(OUT)/RNG.$(FASLEXT): $(OUT)/SGROUP.$(FASLEXT)
$(OUT)/DIOID.$(FASLEXT): $(OUT)/OAMON.$(FASLEXT) $(OUT)/SRING.$(FASLEXT)

$(OUT)/LLINSET.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/RLINSET.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/LINSET.$(FASLEXT): $(OUT)/LLINSET.$(FASLEXT) $(OUT)/RLINSET.$(FASLEXT)

$(OUT)/CTORKIND.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/IOMODE.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)

$(OUT)/PRINT.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/RTVALUE.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/SYSPTR.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)

$(OUT)/VOID.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT)
$(OUT)/OUT.$(FASLEXT): $(OUT)/VOID.$(FASLEXT)

$(OUT)/DIFFDOM.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/DIFFSPC.$(FASLEXT): $(OUT)/DIFFDOM.$(FASLEXT)
$(OUT)/DIFFMOD.$(FASLEXT): $(OUT)/DIFFSPC.$(FASLEXT)
$(OUT)/PDDOM.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/PDSPC.$(FASLEXT): $(OUT)/PDDOM.$(FASLEXT)
$(OUT)/DSEXT.$(FASLEXT): $(OUT)/DIFFSPC.$(FASLEXT) $(OUT)/PDSPC.$(FASLEXT)
$(OUT)/ORDTYPE.$(FASLEXT): $(OUT)/BASTYPE.$(FASLEXT)
$(OUT)/ORDSTRCT.$(FASLEXT): $(OUT)/ORDTYPE.$(FASLEXT) $(OUT)/HOMOTOP.$(FASLEXT)

$(OUT)/BINOP.$(FASLEXT): $(OUT)/BINOPC.$(FASLEXT) $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/IDEMOPC.$(FASLEXT): $(OUT)/BINOPC.$(FASLEXT) $(OUT)/BASTYPE.$(FASLEXT)
$(OUT)/SGPOPC.$(FASLEXT): $(OUT)/BINOPC.$(FASLEXT) $(OUT)/BASTYPE.$(FASLEXT)
$(OUT)/SGPOP.$(FASLEXT): $(OUT)/SGPOPC.$(FASLEXT) $(OUT)/SETCAT.$(FASLEXT) \
	$(OUT)/BINOP.$(FASLEXT)
$(OUT)/MONOPC.$(FASLEXT): $(OUT)/SGPOPC.$(FASLEXT)

$(OUT)/FUNCTOR.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/REPSQ.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	strap-2/PI.$(FASLEXT)
$(OUT)/REPDB.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	strap-2/PI.$(FASLEXT)

$(OUT)/ABELSG.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	$(OUT)/REPDB.$(FASLEXT)

$(OUT)/FEVALAB.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	$(OUT)/ELTAB.$(FASLEXT) $(OUT)/EVALAB.$(FASLEXT) \
	$(OUT)/IEVALAB.$(FASLEXT) strap-2/EQ.$(FASLEXT) \
	strap-2/LIST.$(FASLEXT) strap-2/SYMBOL.$(FASLEXT)

$(OUT)/INS.$(FASLEXT): $(OUT)/UFD.$(FASLEXT) $(OUT)/EUCDOM.$(FASLEXT) \
	$(OUT)/OINTDOM.$(FASLEXT) $(OUT)/DIFRING.$(FASLEXT) \
	$(OUT)/KONVERT.$(FASLEXT) $(OUT)/RETRACT.$(FASLEXT) \
	$(OUT)/LINEXP.$(FASLEXT) $(OUT)/CFCAT.$(FASLEXT) \
	$(OUT)/REAL.$(FASLEXT) $(OUT)/CHARZ.$(FASLEXT) \
	$(OUT)/STEP.$(FASLEXT) $(OUT)/PATMAB.$(FASLEXT) \
	$(OUT)/FEVALAB.$(FASLEXT) strap-2/FRAC.$(FASLEXT) \
	strap-2/FLOAT.$(FASLEXT) strap-2/DFLOAT.$(FASLEXT) \
	strap-2/INFORM.$(FASLEXT) strap-2/SEXCAT.$(FASLEXT) \
	strap-2/PATTERN.$(FASLEXT) strap-2/PI.$(FASLEXT) \
	strap-2/NNI.$(FASLEXT)

$(OUT)/MAYBE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT) $(OUT)/RETRACT.$(FASLEXT)

$(OUT)/SRING.$(FASLEXT): $(OUT)/ABELMON.$(FASLEXT) $(OUT)/MONOID.$(FASLEXT)
$(OUT)/RING.$(FASLEXT): $(OUT)/RNG.$(FASLEXT) $(OUT)/SRING.$(FASLEXT) \
	$(OUT)/LMODULE.$(FASLEXT) $(OUT)/KOERCE.$(FASLEXT)
$(OUT)/MONOID.$(FASLEXT): $(OUT)/SGROUP.$(FASLEXT)
$(OUT)/ABELGRP.$(FASLEXT): $(OUT)/CABMON.$(FASLEXT) $(OUT)/LLINSET.$(FASLEXT)

$(OUT)/ABELMON.$(FASLEXT): $(OUT)/ABELSG.$(FASLEXT) \
	$(OUT)/REPSQ.$(FASLEXT)

$(OUT)/ORDRING.$(FASLEXT): $(OUT)/OAGROUP.$(FASLEXT) $(OUT)/CHARZ.$(FASLEXT) \
	$(OUT)/MONOID.$(FASLEXT)
$(OUT)/COMRING.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/BMODULE.$(FASLEXT)
$(OUT)/DIFRING.$(FASLEXT): $(OUT)/RING.$(FASLEXT)
$(OUT)/ENTIRER.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/BMODULE.$(FASLEXT)
$(OUT)/INTDOM.$(FASLEXT): $(OUT)/COMRING.$(FASLEXT) $(OUT)/ALGEBRA.$(FASLEXT) \
	$(OUT)/ENTIRER.$(FASLEXT) $(OUT)/FIELD.$(FASLEXT)
$(OUT)/OINTDOM.$(FASLEXT): $(OUT)/INTDOM.$(FASLEXT) $(OUT)/ORDRING.$(FASLEXT)
$(OUT)/GCDDOM.$(FASLEXT): $(OUT)/INTDOM.$(FASLEXT)
$(OUT)/UFD.$(FASLEXT): $(OUT)/GCDDOM.$(FASLEXT) $(OUT)/ES.$(FASLEXT)
$(OUT)/ES.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/CACHSET.$(FASLEXT) \
	$(OUT)/PATAB.$(FASLEXT) strap-2/PRIMARR.$(FASLEXT) \
	strap-2/REF.$(FASLEXT) strap-2/ALIST.$(FASLEXT) \
	$(OUT)/MAYBE.$(FASLEXT)
$(OUT)/CACHSET.$(FASLEXT): $(OUT)/ORDSET.$(FASLEXT)
$(OUT)/BOOLEAN.$(FASLEXT): $(OUT)/PROPLOG.$(FASLEXT)
$(OUT)/VECTCAT.$(FASLEXT): $(OUT)/RADCAT.$(FASLEXT)
$(OUT)/IARRAY1.$(FASLEXT): $(OUT)/PRIMARR.$(FASLEXT)
$(OUT)/DIFRING.$(FASLEXT): $(OUT)/DIFFSPC.$(FASLEXT)
$(OUT)/PDMOD.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/PDSPC.$(FASLEXT) \
	$(OUT)/COMRING.$(FASLEXT) $(OUT)/MODULE.$(FASLEXT)
$(OUT)/DMEXT.$(FASLEXT): $(OUT)/DSEXT.$(FASLEXT) $(OUT)/DIFFMOD.$(FASLEXT) \
	$(OUT)/PDMOD.$(FASLEXT)
$(OUT)/PATAB.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT) \
	strap-2/PATTERN.$(FASLEXT)


oa_algebra_layer_0 = \
  TYPE     BINOPC   BINOP    IDEMOPC  SGPOPC   SGPOP    \
  MONOPC   FUNCTOR  \
  BASTYPE  BASTYPE- SETCAT   SETCAT-  SGROUP   SGROUP-  \
  LLINSET  RLINSET  LINSET   ABELSG   ABELSG-  \
  ABELGRP  ABELGRP- ABELMON  ABELMON- ORDTYPE  ORDTYPE- \
  RMODULE  ALGEBRA  ALGEBRA-    \
  FINITE   FINITE-  MONOID   MONOID-  GROUP    GROUP-   \
  RING     RING-    OINTDOM   \
  BMODULE  STEP     LMODULE  \
  AHYP     CFCAT    ELTAB    KOERCE   KONVERT  \
  KRCFROM  KVTFROM  IEVALAB  IEVALAB- EVALAB   EVALAB-  \
  RETRACT  RETRACT- VOID     REPSQ    REPDB    \
  PRIMCAT  PTRANFN  SPFCAT   UTYPE    HOMOTOP   \
  ORDSET   OASGP    DIFRING  SRING    OSGROUP  \
  MODULE   MODULE-  PID      OAGROUP  OAGROUP- OCAMON   \
  OAMON    OAMON-   DIOID    INTDOM   INTDOM-  CACHSET  \
  RNG      RNG-     ORDFIN   OAMONS   CABMON   COMRING  \
  GCDDOM   GCDDOM-  UFD      UFD-     ES        ES-     \
  FIELD    FIELD-   VECTCAT  VECTCAT- RADCAT   RADCAT-  \
  MKBCFUNC MKRECORD MKUCFUNC ENTIRER  ENTIRER- ORDRING  \
  DIFFDOM  DIFFDOM- DIFFSPC  DIFFSPC- DIFFMOD  \
  LINEXP   PATMAB   REAL     CHARZ    LOGIC    LOGIC-   \
  PDDOM    PDDOM-   PDSPC    PDSPC-   \
  DSEXT    DSEXT-   ORDSTRCT          \
  BOOLE    BOOLE-   SRING    MAYBE    \
  INS      INS-     DIVRING  DIVRING- EUCDOM   EUCDOM-  \
  FPS      FPS-     RNS      RNS-     PATAB    \
  POLYCAT  POLYCAT- QFCAT    QFCAT-   FEVALAB  FEVALAB-  \
  INT      NNI      PI       SINT     SYMBOL   DFLOAT

oa_algebra_layer_0_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_0))

oa_algebra_layer_0_objects = \
	$(addprefix $(OUT)/, \
           $(addsuffix .$(FASLEXT),$(oa_algebra_layer_0))) 

$(OUT)/HOMOTOP.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT) $(OUT)/KRCFROM.$(FASLEXT)
$(OUT)/ITUPLE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT) $(OUT)/STREAM.$(FASLEXT)
$(OUT)/PPCURVE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT)
$(OUT)/PSCURVE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT)

$(OUT)/PROPLOG.$(FASLEXT): $(OUT)/BOOLE.$(FASLEXT) $(OUT)/SETCAT.$(FASLEXT)

$(OUT)/BYTEORD.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)

$(OUT)/PROPERTY.$(FASLEXT): $(OUT)/IDENT.$(FASLEXT) $(OUT)/PAIR.$(FASLEXT)
$(OUT)/BINDING.$(FASLEXT): $(OUT)/PROPERTY.$(FASLEXT)
$(OUT)/OPERCAT.$(FASLEXT): $(OUT)/ARITY.$(FASLEXT)
$(OUT)/STREAM.$(FASLEXT): $(OUT)/LZSTAGG.$(FASLEXT)

$(OUT)/MONOP.$(FASLEXT): $(OUT)/MONOPC.$(FASLEXT) $(OUT)/PAIR.$(FASLEXT) \
	$(OUT)/SETCAT.$(FASLEXT)

$(OUT)/IDPT.$(FASLEXT): $(OUT)/PAIR.$(FASLEXT)

$(OUT)/AGG.$(FASLEXT): $(OUT)/BOOLE.$(FASLEXT)
$(OUT)/ELTAGG.$(FASLEXT): $(OUT)/AGG.$(FASLEXT) $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/IXAGG.$(FASLEXT): $(OUT)/ELTAGG.$(FASLEXT) $(OUT)/EVALAB.$(FASLEXT)
$(OUT)/RCAGG.$(FASLEXT): $(OUT)/EVALAB.$(FASLEXT) $(OUT)/SETCAT.$(FASLEXT) \
	$(OUT)/AGG.$(FASLEXT)
$(OUT)/QUAGG.$(FASLEXT): $(OUT)/BGAGG.$(FASLEXT) $(OUT)/FINAGG.$(FASLEXT)
$(OUT)/SKAGG.$(FASLEXT): $(OUT)/BGAGG.$(FASLEXT) $(OUT)/FINAGG.$(FASLEXT)
$(OUT)/DQAGG.$(FASLEXT): $(OUT)/QUAGG.$(FASLEXT) $(OUT)/SKAGG.$(FASLEXT)
$(OUT)/PRQAGG.$(FASLEXT): $(OUT)/BGAGG.$(FASLEXT) $(OUT)/FINAGG.$(FASLEXT)
#$(OUT)/DLAGG.$(FASLEXT): $(OUT)/RCAGG.$(FASLEXT)
#$(OUT)/URAGG.$(FASLEXT): $(OUT)/RCAGG.$(FASLEXT)
#$(OUT)/BRAGG.$(FASLEXT): $(OUT)/RCAGG.$(FASLEXT)
$(OUT)/FLAGG.$(FASLEXT): $(OUT)/BMODULE.$(FASLEXT) $(OUT)/PID.$(FASLEXT) \
	$(OUT)/OAGROUP.$(FASLEXT) $(OUT)/RETRACT.$(FASLEXT) \
	$(OUT)/LINEXP.$(FASLEXT) $(OUT)/PATMAB.$(FASLEXT) \
	$(OUT)/REAL.$(FASLEXT) $(OUT)/CHARZ.$(FASLEXT) \
	$(OUT)/DIFFSPC.$(FASLEXT) $(OUT)/FINAGG.$(FASLEXT)
$(OUT)/A1AGG.$(FASLEXT): $(OUT)/FLAGG.$(FASLEXT) $(OUT)/SMAGG.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT) strap-2/UNISEG.$(FASLEXT) \
	$(OUT)/ORDSET.$(FASLEXT)
$(OUT)/SRAGG.$(FASLEXT): $(OUT)/A1AGG.$(FASLEXT)
$(OUT)/STAGG.$(FASLEXT): $(OUT)/URAGG.$(FASLEXT) $(OUT)/LNAGG.$(FASLEXT)
$(OUT)/LNAGG.$(FASLEXT): $(OUT)/SEGCAT.$(FASLEXT)
$(OUT)/SETAGG.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) $(OUT)/CLAGG.$(FASLEXT)
$(OUT)/DIOPS.$(FASLEXT): strap-2/STRING.$(FASLEXT)
$(OUT)/DIAGG.$(FASLEXT): $(OUT)/DIOPS.$(FASLEXT)
$(OUT)/KDAGG.$(FASLEXT): $(OUT)/DIAGG.$(FASLEXT) $(OUT)/IXAGG.$(FASLEXT)
$(OUT)/TBAGG.$(FASLEXT): $(OUT)/KDAGG.$(FASLEXT)
$(OUT)/ALAGG.$(FASLEXT): $(OUT)/TBAGG.$(FASLEXT)
$(OUT)/LIST.$(FASLEXT): $(OUT)/LSAGG.$(FASLEXT) $(OUT)/REF.$(FASLEXT) \
	$(OUT)/ALIST.$(FASLEXT) $(OUT)/PRIMARR.$(FASLEXT)
$(OUT)/HOAGG.$(FASLEXT): $(OUT)/FUNCTOR.$(FASLEXT)
$(OUT)/SMAGG.$(FASLEXT): $(OUT)/HOAGG.$(FASLEXT)
$(OUT)/FINAGG.$(FASLEXT): $(OUT)/HOAGG.$(FASLEXT)
$(OUT)/ELAGG.$(FASLEXT): $(OUT)/SMAGG.$(FASLEXT) strap-2/UNISEG.$(FASLEXT)
$(OUT)/CLAGG.$(FASLEXT): $(OUT)/HOAGG.$(FASLEXT)
$(OUT)/FSAGG.$(FASLEXT): $(OUT)/FINAGG.$(FASLEXT)
$(OUT)/ALIST.$(FASLEXT): $(OUT)/ALAGG.$(FASLEXT)

$(OUT)/STRING.$(FASLEXT): $(OUT)/SRAGG.$(FASLEXT) $(OUT)/ORDFIN.$(FASLEXT)

$(OUT)/SEXCAT.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) $(OUT)/PID.$(FASLEXT) \
	$(OUT)/OAGROUP.$(FASLEXT) $(OUT)/LINEXP.$(FASLEXT) \
	$(OUT)/PATMAB.$(FASLEXT) $(OUT)/REAL.$(FASLEXT) \
	$(OUT)/CHARZ.$(FASLEXT) $(OUT)/AGG.$(FASLEXT) \
	$(OUT)/IXAGG.$(FASLEXT) $(OUT)/FLAGG.$(FASLEXT) \
	$(OUT)/ELAGG.$(FASLEXT)

$(OUT)/MSETAGG.$(FASLEXT): $(OUT)/MDAGG.$(FASLEXT) $(OUT)/SETAGG.$(FASLEXT)
$(OUT)/MDAGG.$(FASLEXT): $(OUT)/DIOPS.$(FASLEXT)
$(OUT)/REF.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) $(OUT)/IDENT.$(FASLEXT)

oa_algebra_layer_1 = \
  ITUPLE    IDPT    ITFUN2   SEGCAT    \
  FILECAT  SMAGG    \
  PPCURVE  PSCURVE  RESLATC            \
  IDENT    OUTFORM  BINDING           BOOLEAN            \
  DROPT1   PLOT1    IARRAY1  \
  DATAARY  PROPLOG  BYTEORD  \
  AGG      AGG-     ELTAGG   ELTAGG-  IXAGG    IXAGG-   \
  BGAGG    BGAGG-   BRAGG    BRAGG-   ELAGG    ELAGG-   \
  DLAGG    DQAGG    QUAGG    SKAGG    PRQAGG   ALAGG    \
  FLAGG    FLAGG-   URAGG    URAGG-   LNAGG    LNAGG-   \
  A1AGG    A1AGG-   LSAGG    LSAGG-   SRAGG    SRAGG-   \
  FSAGG    FSAGG-   STAGG    STAGG-   CLAGG    CLAGG-   \
  RCAGG    RCAGG-   SETAGG   SETAGG-  HOAGG    HOAGG-   \
  TBAGG    TBAGG-   KDAGG    KDAGG-   DIAGG    DIAGG-   \
  DIOPS    DIOPS-   STRING   FINAGG   FINAGG-  MDAGG    \
  MONOP    PRIMARR  SEXCAT             \
  PROPERTY ARITY    OPERCAT  OPERCAT- PAIR      STREAM   \
  COMBOPC  EQ2      NONE1    CONDUIT  IOMODE   CTORKIND \
  PDMOD    DMEXT    LZSTAGG  LZSTAGG- MSETAGG   \
  ITFUN3   STREAM1  STREAM2  STREAM3  ANY1     SEGBIND2 \
  ALIST   LIST      RTVALUE  SYSPTR   ATTREG   REF       

oa_algebra_layer_1_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_1))

oa_algebra_layer_1_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_1)))
oa_algebra_layer_2 = \
  SYNTAX   INTRET   SEGXCAT CONTOUR  LIST3   MKFUNC   \
  COMMONOP          FNCAT   SCACHE   BOP     BOP1     \
  DOMTMPLT FCTRDATA

$(OUT)/FNCAT.$(FASLEXT): $(OUT)/HOMOTOP.$(FASLEXT) $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/SYNTAX.$(FASLEXT): $(OUT)/IDENT.$(FASLEXT)
$(OUT)/COMMONOP.$(FASLEXT): $(OUT)/BOP.$(FASLEXT)
$(OUT)/DOMTMPLT.$(FASLEXT): $(OUT)/SYNTAX.$(FASLEXT) $(OUT)/OAMONS.$(FASLEXT)
$(OUT)/FCTRDATA.$(FASLEXT): $(OUT)/DOMTMPLT.$(FASLEXT)

oa_algebra_layer_2_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_2))

oa_algebra_layer_2_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_2)))
oa_algebra_layer_3 = \
   GRDEF     SCOPE    MAPHACK1 MAPHACK2 MAPHACK3 MAPPKG1  \
   MAPPKG2   MAPPKG3  INTBIT   MONAD    MONAD-   


$(OUT)/MONAD.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
$(OUT)/SCOPE.$(FASLEXT): $(OUT)/BINDING.$(FASLEXT)

oa_algebra_layer_3_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_3))

oa_algebra_layer_3_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_3)))

$(OUT)/ENV.$(FASLEXT): $(OUT)/SCOPE.$(FASLEXT)
$(OUT)/COMM.$(FASLEXT): $(OUT)/OSI.$(FASLEXT)
$(OUT)/PALETTE.$(FASLEXT): $(OUT)/COLOR.$(FASLEXT)


oa_algebra_layer_4 = \
  ANON     OSI      COMM     COMPPROP EXIT     \
  FAMONC   FORMULA1 IDPC              NONE     \
  COLOR             ONECOMP2          \
  PALETTE  PARPCURV PARPC2   PARSCURV PARSC2   PARSURF  \
  PARSU2            PATRES2  PATTERN1 PDECAT            \
  RFDIST   RIDIST   SPACEC   SPLNODE  \
  SUCH     TEX1     UDVO     YSTREAM           ENV      \
  ATRIG    ATRIG-   LALG     LALG- 


oa_algebra_layer_4_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_4))

oa_algebra_layer_4_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_4)))

$(OUT)/KERNEL.$(FASLEXT): $(OUT)/KERNEL2.$(FASLEXT)
$(OUT)/DVARCAT.$(FASLEXT): $(OUT)/DIFFSPC.$(FASLEXT)
$(OUT)/PDRING.$(FASLEXT): $(OUT)/PDSPC.$(FASLEXT)
$(OUT)/IDPAM.$(FASLEXT): $(OUT)/IDPO.$(FASLEXT)

oa_algebra_layer_5 = \
           CHARNZ            DVARCAT  DVARCAT- ELEMFUN  \
  ELEMFUN- FCOMP    FPATMAB  IDPAM    IDPO     \
  INCRMAPS KERNEL2           MODMONOM MONADWU  MONADWU- \
  MRF2     NARNG    NARNG-   NSUP2    ODVAR    OPQUERY  \
           ORDMON   PATMATCH PERMCAT  PDRING            \
  SDVAR    SUP2     TRIGCAT  TRIGCAT- UP2      \
  ELABEXPR KERNEL

oa_algebra_layer_5_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_5))

oa_algebra_layer_5_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_5)))

$(OUT)/PROPFRML.$(FASLEXT): $(OUT)/KERNEL.$(FASLEXT)
$(OUT)/KTVLOGIC.$(FASLEXT): $(OUT)/PROPLOG.$(FASLEXT) $(OUT)/BYTE.$(FASLEXT)
$(OUT)/PROPFUN1.$(FASLEXT): $(OUT)/PROPFRML.$(FASLEXT)
$(OUT)/PROPFUN2.$(FASLEXT): $(OUT)/PROPFRML.$(FASLEXT)
$(OUT)/DIFEXT.$(FASLEXT): $(OUT)/DSEXT.$(FASLEXT)
$(OUT)/BYTE.$(FASLEXT): $(OUT)/LOGIC.$(FASLEXT)
$(OUT)/SYSNNI.$(FASLEXT): $(OUT)/LOGIC.$(FASLEXT) $(OUT)/ORDFIN.$(FASLEXT)

oa_algebra_layer_6 = \
  PROPFRML PROPFUN1 AUTOMOR  CHARPOL  \
  DIFEXT            ES1      ES2     GRMOD    GRMOD-   \
  HYPCAT   HYPCAT-           MODRING NASRING  NASRING- \
  SORTPAK  ZMOD     PROPFUN2                           \
  KTVLOGIC                   BYTE    SYSINT   SYSNNI

oa_algebra_layer_6_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_6))
oa_algebra_layer_6_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_6)))

$(OUT)/IFAMON.$(FASLEXT): $(OUT)/LMOPS.$(FASLEXT)
$(OUT)/IDPOAM.$(FASLEXT): $(OUT)/IDPAM.$(FASLEXT)

oa_algebra_layer_7 = \
  BTCAT   BTCAT-  LMOPS             FMCAT   IDPOAM   \
  IFAMON  GRALG   GRALG-                             \
  INT8    INT16   INT32    INT64    UINT8   UINT16   \
  UINT32  UINT64

oa_algebra_layer_7_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_7))

oa_algebra_layer_7_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_7)))

$(OUT)/BSTREE.$(FASLEXT): $(OUT)/BTREE.$(FASLEXT)
$(OUT)/ITAYLOR.$(FASLEXT): $(OUT)/STREAM.$(FASLEXT)
$(OUT)/BTREE.$(FASLEXT): $(OUT)/TREE.$(FASLEXT)

oa_algebra_layer_8 = \
  BSTREE  BTOURN   CARD     FACTFUNC TREE \
  FR2     FRAC2    FRUTIL   ITAYLOR   MLO      NAALG \
  NAALG-  OP       ORDCOMP2 RANDSRC   UNISEG2  XALG  \
  BTREE   POLY     SMP                ARR2CAT  ARR2CAT-

oa_algebra_layer_8_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_8))

oa_algebra_layer_8_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_8)))

$(OUT)/FT.$(FASLEXT): $(OUT)/FST.$(FASLEXT) $(OUT)/POLY.$(FASLEXT)
$(OUT)/IDPOAMS.$(FASLEXT): $(OUT)/IDPOAM.$(FASLEXT)
$(OUT)/IDPAG.$(FASLEXT): $(OUT)/IDPAM.$(FASLEXT)

oa_algebra_layer_9 = \
  AMR      AMR-     DLP      EAB      \
  FAGROUP  FAMONOID FLINEXP  FLINEXP- FRETRCT  FRETRCT- \
  FSERIES  FT       IDPAG    IDPOAMS  INFINITY LA       \
  OMLO     ORTHPOL  PRODUCT  PADICCT  \
                    RATRET   RADUTIL   \
  XFALG    ZLINDEP  BBTREE   TABLE    INTABL            \
  SIG      FMONCAT  FST


oa_algebra_layer_9_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_9))

oa_algebra_layer_9_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_9)))
$(OUT)/ANY.$(FASLEXT): $(OUT)/SEX.$(FASLEXT)

$(OUT)/SEX.$(FASLEXT): $(OUT)/SEXOF.$(FASLEXT)
$(OUT)/PENDTREE.$(FASLEXT): $(OUT)/TREE.$(FASLEXT)
$(OUT)/QFORM.$(FASLEXT): $(OUT)/SQMATRIX.$(FASLEXT)
$(OUT)/SQMATRIX.$(FASLEXT): $(OUT)/SMATCAT.$(FASLEXT) \
	$(OUT)/MATRIX.$(FASLEXT) $(OUT)/IARRAY2.$(FASLEXT)
$(OUT)/MATCAT.$(FASLEXT): $(OUT)/FINAGG.$(FASLEXT)
$(OUT)/SMATCAT.$(FASLEXT): $(OUT)/RMATCAT.$(FASLEXT)
$(OUT)/RMATCAT.$(FASLEXT): $(OUT)/DIRPROD.$(FASLEXT)
$(OUT)/DIRPROD.$(FASLEXT): $(OUT)/DIRPCAT.$(FASLEXT)
$(OUT)/DIRPCAT.$(FASLEXT): $(OUT)/VSPACE.$(FASLEXT)
$(OUT)/MATRIX.$(FASLEXT): $(OUT)/MATCAT.$(FASLEXT)
$(OUT)/BTAGG.$(FASLEXT): $(OUT)/BOOLE.$(FASLEXT)
$(OUT)/PATLRES.$(FASLEXT): $(OUT)/PATRES.$(FASLEXT)
$(OUT)/PBWLB.$(FASLEXT): $(OUT)/LWORD.$(FASLEXT)
$(OUT)/LWORD.$(FASLEXT): $(OUT)/MAGMA.$(FASLEXT)
$(OUT)/PFR.$(FASLEXT): $(OUT)/FR.$(FASLEXT)

oa_algebra_layer_10 = \
  BPADIC   ANY      LWORD    FR \
  SEXOF    CRAPACK  DEQUEUE  DLIST    \
  FAMR     FAMR-    FLASORT  \
  FLAGG2   FGROUP   FM       FM1      \
  FPC      FPC-     FMONOID  INDE     \
  IPADIC   IROOT    IR2       \
  LIECAT   LIECAT-  LIST2    LIST2MAP \
  SEX      HASHTBL  MAGMA    \
  MOEBIUS  MODFIELD MODOP    \
  MRING    MTHING   NCNTFRAC NCODIV   \
  NUMTUBE  ODR      OFMONOID ONECOMP  \
  ORDCOMP  OREPCAT  OREPCAT- OWP      \
  PADIC    PATTERN2 PATLRES  PARTPERM \
  PBWLB    PENDTREE PGE      PGROEB   \
  PINTERP  PFR      PMDOWN   \
  PRTITION PMINS    PMLSAGG  PMTOOLS  \
  PSCAT    PSCAT-   QFORM    QUEUE    \
  SQMATRIX SEG      SEG2     MTSCAT   \
  STACK    STTAYLOR TABLBUMP TABLEAU  \
  TOPSP    TRANFUN  TRANFUN- TUBE     \
  UDPO     UNISEG   VIEW     VSPACE   \
  VSPACE-  XPOLYC   XPR      BTAGG    BTAGG-  \
  FUNDESC  SMATCAT  SMATCAT- \
  RMATRIX  RMATCAT  RMATCAT- DIRPROD \
  DIRPCAT  DIRPCAT- MATRIX  \
  MATCAT   MATCAT-  IARRAY2  FFIELDC  FFIELDC-


oa_algebra_layer_10_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_10))

oa_algebra_layer_10_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_10)))
$(OUT)/ARRAY2.$(FASLEXT): $(OUT)/IFARRAY.$(FASLEXT)
$(OUT)/ORESUP.$(FASLEXT): $(OUT)/PR.$(FASLEXT)
$(OUT)/PLOT3D.$(FASLEXT): $(OUT)/POINT.$(FASLEXT)
$(OUT)/POINT.$(FASLEXT): $(OUT)/PTCAT.$(FASLEXT)

oa_algebra_layer_11 = \
  APPLYORE ARRAY1   ARRAY12  ARRAY2   \
  ASTACK   COMBINAT POINT    PTCAT \
  CSTTOOLS FARRAY   \
  FLALG    GALUTIL  HEAP              \
  IFARRAY  INTCAT   INTHEORY \
  IRREDFFX LFCAT    LODOCAT  LODOCAT- \
                             MATSTOR  \
  ORESUP   OREPCTO  OREUP    PLOT3D   \
  PR       PREASSOC PRIMARR2 REDORDER \
           SYMPOLY  \
  TS       TUPLE    UPSCAT   UPSCAT-  \
                    XDPOLY   XEXPPKG  \
  XF       XF-               XPOLY    \
  XRPOLY   FFIELDC  FFIELDC- MTSCAT

oa_algebra_layer_11_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_11))

oa_algebra_layer_11_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_11)))
oa_algebra_layer_12 = \
  ULSCAT                           BITS     DIRPROD2 \
           LPOLY   LSMP    LSMP1   MATCAT2           TRIMAT  \
                   SYSTEM  HOSTNAME PORTNUM

oa_algebra_layer_12_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_12))

oa_algebra_layer_12_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_12)))

$(OUT)/UPOLYC.$(FASLEXT): $(OUT)/PFECAT.$(FASLEXT)
$(OUT)/SETMN.$(FASLEXT): $(OUT)/IBITS.$(FASLEXT)
$(OUT)/LEXP.$(FASLEXT): $(OUT)/XPBWPOLY.$(FASLEXT)
$(OUT)/XPBWPOLY.$(FASLEXT): $(OUT)/XPOLYC.$(FASLEXT) \
	$(OUT)/PBWLB.$(FASLEXT)

oa_algebra_layer_13 = \
  ASSOCEQ  CARTEN   CLIF     CLIP     \
  UPOLYC   UPOLYC-                    \
  COORDSYS DBASE    DHMATRIX DIOSP    \
                    FAXF     FAXF-    FFPOLY2  \
  FNLA     GRAY     HB       IRSN     \
  MHROWRED NUMODE   NUMQUAD  MESH     \
  ODESYS   ODETOOLS ORDFUNS  PERMAN   \
  PFECAT   PFECAT-           PSEUDLIN \
  PTPACK   REP2     SETMN    PLOTTOOL          \
  SYMFUNC  VECTOR2  VECTOR   CHAR     XPBWPOLY \
  INBCON   INBCON-  OUTBCON  OUTBCON- LEXP    

oa_algebra_layer_13_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_13))

oa_algebra_layer_13_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_13)))
$(OUT)/FS.$(FASLEXT): $(OUT)/UPOLYC.$(FASLEXT)
$(OUT)/FILE.$(FASLEXT): $(OUT)/FNAME.$(FASLEXT)

oa_algebra_layer_14 = \
  FS       FS-      ACF      ACF-     \
  ACFS    ACFS-    BALFACT  BEZOUT   BINARY   BINFILE  BOUNDZRO \
  BPADICRT BRILL    CDEN     CHVAR    \
  COMMUPC  CONTFRAC CVMP     CYCLOTOM \
  CYCLES   DDFACT   DECIMAL           DISPLAY  DMP      \
  DPMO     DPOLCAT  DPOLCAT- EMR      EQ       ERROR    \
  EVALCYC  FACUTIL  FF       FFCG     \
  FFCGX    FFHOM    FFNB     FFNBX    \
  FFPOLY   FFX      FFSLPE   FGLMICPK \
  FILE     FINAALG  FINAALG- FINRALG  \
  FINRALG-          FLOATRP  FNAME    \
  FORMULA  FRAC     \
  GENEEZ   GENMFACT GENPGCD  \
  GALFACTU GALPOLYU GB       GBEUCLID \
  GBF      GBINTERN GHENSEL  GMODPOL  \
  GOSPER   GRIMAGE  GROEBSOL HDMP     \
  HDP      HEXADEC  HEUGCD   IBPTOOLS \
  IFF               ICARD    ICDEN    \
  IDECOMP           IMATLIN  IMATQF   \
  INMODGCD INNMFACT INPSIGN  INTHERTR \
  INTRAT   INTRF    INTSLPE  INTTR    \
  ISUMP    LAUPOL   LEADCDET LGROBP   \
  LIMITRF  LINDEP   LO       LPEFRAC  \
  LSPP     MATLIN   MCDEN    MDDFACT  \
  MFINFACT MLIFT    \
  MMAP     MODMON   MONOTOOL MPCPF    \
  MPC2     MPC3     MPOLY    MPRFF    \
  MRATFAC  MULTSQFR NORMRETR NPCOEF   \
  NSUP     NTPOLFN  ODP      ODEPRIM  \
  ODEPRRIC                    \
  PADICRAT PADICRC  PCOMP    PDECOMP  \
  PF       PFBR     PFBRU    PFOTOOLS \
  PFRPAC   PGCD     PINTERPA PLEQN    \
  PMPLCAT  PMQFCAT  PNTHEORY POLUTIL  \
  POLTOPOL POLYCATQ POLYLIFT POLYROOT \
  POLY2    POLY2UP  PRS      PSQFR    \
  PUSHVAR  QALGSET  QFCAT2   RADIX    \
  RATFACT  RCFIELD  RCFIELD- RDETR    \
  RDETRS   REAL0    REAL0Q   REALSOLV \
  RESRING  RETSOL   RF       RFFACTOR \
                    RRCC     RRCC-    \
  SCPKG    SHDP     SHP      SIGNRF   \
  SMITH             SMTS     SOLVEFOR \
  SPLTREE  STINPROD STTFNC   SUBRESP  \
  SUMRF    SUP      SUPFRACF TANEXP   \
  TEX      TEXTFILE          \
  TWOFACT  UNIFACT  UP       UPCDEN   \
  UPDECOMP UPDIVP   UPMP     UPOLYC2  \
  UPXSCAT  UPSQFREE VIEWDEF  VIEW2D   \
           WEIER    WP       \
  EQTBL    GSTBL            \
           STBL     STRTBL\
                    SYMS     SYMTAB \
  IOBCON   


oa_algebra_layer_14_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_14))

oa_algebra_layer_14_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_14)))
$(OUT)/EXPUPXS.$(FASLEXT): $(OUT)/UPXS.$(FASLEXT)
$(OUT)/UPXS.$(FASLEXT): $(OUT)/UPXSCAT.$(FASLEXT) $(OUT)/UPXSCONS.$(FASLEXT) \
	$(OUT)/ULS.$(FASLEXT) $(OUT)/PDDOM.$(FASLEXT)
$(OUT)/UPXSCONS.$(FASLEXT): $(OUT)/UPXSCCA.$(FASLEXT)
$(OUT)/ULS.$(FASLEXT): $(OUT)/ULSCONS.$(FASLEXT) $(OUT)/PDDOM.$(FASLEXT)
$(OUT)/ULSCONS.$(FASLEXT): $(OUT)/ULSCCAT.$(FASLEXT)
$(OUT)/ULSCCAT.$(FASLEXT): $(OUT)/UTS.$(FASLEXT)
$(OUT)/UTS.$(FASLEXT): $(OUT)/UTSCAT.$(FASLEXT) $(OUT)/PDDOM.$(FASLEXT)

oa_algebra_layer_15 = \
  DSMP     EXPUPXS  UPXS     UPXSCONS \
  FRAMALG  FRAMALG- ODPOL   \
  PLOT     RMCAT2   ROIRC    SDPOL   \
  ULS      ULSCONS  TUBETOOL UPXSCCA \
  UPXSCCA- JVMBCODE          BYTEBUF    OVERSET \
  ULSCCAT  ULSCCAT- UTS      UTSCAT  UTSCAT-

oa_algebra_layer_15_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_15))

oa_algebra_layer_15_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_15)))
$(OUT)/SPACE3.$(FASLEXT): $(OUT)/SUBSPACE.$(FASLEXT)

oa_algebra_layer_16 = \
  DPMM     EFUPXS  FFINTBAS FRIDEAL  ULS2      \
  FRIDEAL2 FRMOD   CARTEN2  COMPLEX2 DRAWHACK  \
  IBATOOL  INTFACT DEGRED   PMPRED   UPXS2   \
  MONOGEN MONOGEN- NFINTBAS PMASS    PADEPAC \
  SPACE3   FFF     SUBSPACE PTFUNC2  DRAWCX  DRAWPT   


oa_algebra_layer_16_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_16))

oa_algebra_layer_16_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_16)))

oa_algebra_layer_17 = \
  CCLASS  FSAGG2  GALFACT IALGFACT CTORCAT  CTORCAT- \
  IBACHIN MMLFORM NORMMA  ODERED  OMSAGG    OPSIG    \
  PERM    PERMGRP PRIMES  PWFFINTB DROPT   \
  RDIST   SAE     SAEFACT SAERFFC  \
  SGCF VIEW3D 

$(OUT)/VIEW3D.$(FASLEXT): $(OUT)/DROPT.$(FASLEXT)

oa_algebra_layer_17_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_17))

oa_algebra_layer_17_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_17)))
$(OUT)/PSETCAT.$(FASLEXT): $(OUT)/RPOLCAT.$(FASLEXT)

oa_algebra_layer_18 = \
	IPF     CATCTOR DOMCTOR   CTORCALL \
  KAFILE  PATRES TBCMPPK PSETCAT    PSETCAT-  \
  RPOLCAT  RPOLCAT-

oa_algebra_layer_18_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_18))

oa_algebra_layer_18_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_18)))
$(OUT)/TSETCAT.$(FASLEXT): $(OUT)/PSETCAT.$(FASLEXT) $(OUT)/RPOLCAT.$(FASLEXT)
$(OUT)/FPARFRAC.$(FASLEXT): $(OUT)/DIFFSPC.$(FASLEXT)

oa_algebra_layer_19 = \
  ACPLOT   ANTISYM           \
                             COMPCAT \
  COMPCAT- DRAW     DRAWCFUN \
  DROPT0   EP       \
  FFCAT    FFCAT-   FFCGP    FFNBP   \
  FFP      FLOAT    FPARFRAC         \
  FRNAALG  FRNAALG- EXPR             \
           FUNCTION GDMP     HACKPI  \
  IDEAL    INFORM   INFORM1  IPRNTPK \
  IR       ISUPS             LIB     \
  LMDICT   LODOOPS           MKFLCFN \
  MSET                         \
  NREP     NUMFMT   OC       OC-     \
           ODERAT                    \
                    PATTERN  OVAR    \
  PMKERNEL PMSYM             PRIMELT \
  QALGSET2 QEQUAT   RECLOS   REP1    \
           QUATCAT  QUATCAT- RFFACT  \
           ROMAN    RNGBIND \
           RULECOLD SAOS     SEGBIND \
  SET      SPECOUT           \
                    SYSSOLP          \
           VARIABLE WFFINTBS SPADPRSR \
  PARSER            TSETCAT  TSETCAT-

oa_algebra_layer_19_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_19))

oa_algebra_layer_19_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_19)))
$(OUT)/AN.$(FASLEXT): $(OUT)/IAN.$(FASLEXT)
$(OUT)/IAN.$(FASLEXT): $(OUT)/EXPR.$(FASLEXT)
$(OUT)/SUTS.$(FASLEXT): $(OUT)/PDDOM.$(FASLEXT)
$(OUT)/OCT.$(FASLEXT): $(OUT)/QUAT.$(FASLEXT)
$(OUT)/EXPEXPAN.$(FASLEXT): $(OUT)/UPXSSING.$(FASLEXT)

oa_algebra_layer_20 = \
  AF       ALGFACT  ALGFF    ALGMANIP ALGMFACT ALGPKG   \
  ALGSC    AN       APPRULE  CINTSLPE COMPFACT COMPLEX  \
  COMPLPAT CMPLXRT  CPMATCH  CRFP     \
  CTRIGMNP \
  DBLRESP  DERHAM   DFSFUN   DRAWCURV \
  EF       EFSTRUC  \
  ELFUTS   EXPEXPAN EXPRODE  \
  EXPRTUBE EXPR2    FDIVCAT  \
  FDIVCAT- FDIV2    FFCAT2   FLOATCP  \
  FORDER   FSRED    FSUPFACT \
  FRNAAF2  FSPECF   FS2      FS2UPS   \
  GAUSSFAC GCNAALG  GENUFACT GENUPS   \
  GTSET    GPOLSET  IAN      INEP     \
  INFPROD0 INFSP    INPRODFF INPRODPF \
  INTAF    INTALG   INTEF    INTG0    \
  INTHERAL INTPAF   INTPM    INTTOOLS \
  ITRIGMNP JORDAN   KOVACIC  LF       \
  LIE      LODOF    LSQM              \
  MULTFACT NCEP     \
  NLINSOL  NSMP     NUMERIC  OCT      \
  OCTCT2   ODEPAL   ODERTRIC PADE     \
  PAN2EXPR          PFO      PFOQ     \
  PICOERCE PMASSFS  PMFS     PMPREDFS \
  PSETPK   QUAT     QUATCT2  RADFF    \
  RDEEF    RDEEFS   RDIV     RSETCAT  \
  RSETCAT- RULE     RULESET  SIMPAN   \
  SOLVESER SUMFS    SUTS     \
  TOOLSIGN TRIGMNIP TRMANIP  ULSCCAT  \
  ULSCCAT- UPXSSING UTSODE   UTSODETL \
  UTS2     WUTSET   

oa_algebra_layer_20_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_20))

oa_algebra_layer_20_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_20)))
$(OUT)/FDIV.$(FASLEXT): $(OUT)/HELLFDIV.$(FASLEXT)
$(OUT)/GSERIES.$(FASLEXT): $(OUT)/PDDOM.$(FASLEXT)
$(OUT)/SULS.$(FASLEXT): $(OUT)/PDDOM.$(FASLEXT)
$(OUT)/SUPXS.$(FASLEXT): $(OUT)/PDDOM.$(FASLEXT)

oa_algebra_layer_21 = \
  DEFINTEF DFINTTLS DEFINTRF \
  EFULS             EXPR2UPS \
  FDIV     FSCINT   FSINT    FS2EXPXP \
  GSERIES  HELLFDIV INVLAPLA IR2F     \
  IRRF2F   LAPLACE  LIMITPS  LODEEF   \
  NODE1    ODECONST ODEINT   REP      \
  SOLVERAD SULS     SUPXS             \
                    UPXSCONS 

oa_algebra_layer_21_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_21))

oa_algebra_layer_21_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_21)))
oa_algebra_layer_22 = \
           COMBF    FSPRMELT \
  INBFF    LODO     LODO1    LODO2    \
  NTSCAT   REGSET   RGCHAIN  RSETGCD  \
  RSDCMPK  SFRTCAT  SIGNEF   SNTSCAT  \
  SOLVETRA SRDCMPK  SREGSET  STTF     \
           ZDSOLVE

oa_algebra_layer_22_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_22))

oa_algebra_layer_22_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_22)))

$(OUT)/MSYSCMD.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

oa_algebra_layer_23 = \
  CPIMA    IRURPK   LAZM3PK  LEXTRIPK \
  NORMPK   QCMPACK  RURPK    SFRGCD   \
  SFQCMPK  INTRVL   ODEEF    DOMAIN   CATEGORY

oa_algebra_layer_23_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_23))

oa_algebra_layer_23_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_23)))
oa_algebra_layer_24 =  \
	RINTERP	 ASTCAT   ASTCAT-  SASTCAT  HEADAST  \
	LITERAL           TYPEAST  IMPTAST  MAPPAST  ATTRAST  \
	JOINAST  IFAST    RPTAST   WHILEAST INAST    CLLCTAST \
	LSTAST   EXITAST  RETAST   SEGAST   PRTDAST  CRCEAST   \
	LETAST   SUCHAST  RDUCEAST COLONAST ADDAST   CAPSLAST \
	CASEAST  HASAST   ISAST    CATAST   WHEREAST COMMAAST \
	QQUTAST  DEFAST   MACROAST SPADXPT  SPADAST  PARAMAST \
	INBFILE  OUTBFILE IOBFILE  RGBCMDL  RGBCSPC  STEPAST  \
        CTOR     IP4ADDR  NETCLT   INETCLTS                   \
	IRFORM   COMPILER MSYSCMD  PRINT   OUT      \
	ITFORM   ELABOR   TALGOP   YDIAGRAM LINELT   DBASIS   \
	LINFORM  LINBASIS JVMOP    JVMCFACC JVMFDACC JVMMDACC \
	JVMCSTTG

oa_algebra_layer_24_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_24))

oa_algebra_layer_24_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_24)))

$(OUT)/SASTCAT.$(FASLEXT): $(OUT)/ASTCAT.$(FASLEXT)
$(OUT)/LITERAL.$(FASLEXT): $(OUT)/SASTCAT.$(FASLEXT)
$(OUT)/PARAMAST.$(FASLEXT): $(OUT)/IDENT.$(FASLEXT) $(OUT)/STRING.$(FASLEXT)
$(OUT)/HEADAST.$(FASLEXT): $(OUT)/PARAMAST.$(FASLEXT)
$(OUT)/SPADXPT.$(FASLEXT): $(OUT)/ASTCAT.$(FASLEXT)
$(OUT)/ATTRAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT) $(OUT)/SPADAST.$(FASLEXT)
$(OUT)/TYPEAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/IMPTAST.$(FASLEXT): $(OUT)/TYPEAST.$(FASLEXT)
$(OUT)/MAPPAST.$(FASLEXT): $(OUT)/TYPEAST.$(FASLEXT)
$(OUT)/SIGAST.$(FASLEXT): $(OUT)/SIG.$(FASLEXT) $(OUT)/IDENT.$(FASLEXT)
$(OUT)/JOINAST.$(FASLEXT): $(OUT)/TYPEAST.$(FASLEXT)
$(OUT)/IFAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/RPTAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/WHILEAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/INAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/STEPAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT) \
		$(OUT)/IDENT.$(FASLEXT) $(OUT)/MAYBE.$(FASLEXT)
$(OUT)/CLLCTAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT) $(OUT)/SPADAST.$(FASLEXT)
$(OUT)/LSTAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/EXITAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/RETAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/SEGAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/SEQAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/PRTDAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/CRCEAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT) $(OUT)/SYNTAX.$(FASLEXT)
$(OUT)/RSTRCAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/LETAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/RDUCEAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/SUCHTAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/COLONAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/ADDAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/CAPSLAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/CASEAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/HASAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/ISAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/CATAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/WHEREAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/COMMAAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/QQUTAST.$(FASLEXT): $(OUT)/SASTCAT.$(FASLEXT)
$(OUT)/DEFAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT) $(OUT)/SPADAST.$(FASLEXT)
$(OUT)/MACROAST.$(FASLEXT): $(OUT)/SPADXPT.$(FASLEXT)
$(OUT)/SPADAST.$(FASLEXT): $(OUT)/SIGAST.$(FASLEXT) \
	$(OUT)/RSTRCAST.$(FASLEXT) $(OUT)/SEQAST.$(FASLEXT) \
	$(OUT)/SUCHTAST.$(FASLEXT)

$(OUT)/INBFILE.$(FASLEXT): $(OUT)/FNAME.$(FASLEXT) \
	$(OUT)/INBCON.$(FASLEXT) $(OUT)/STRING.$(FASLEXT)
$(OUT)/OUTBFILE.$(FASLEXT): $(OUT)/FNAME.$(FASLEXT) \
	$(OUT)/OUTBCON.$(FASLEXT) $(OUT)/STRING.$(FASLEXT)
$(OUT)/IOBFILE.$(FASLEXT): $(OUT)/INBFILE.$(FASLEXT) \
	$(OUT)/OUTBFILE.$(FASLEXT)

$(OUT)/CTOR.$(FASLEXT): $(OUT)/CTORKIND.$(FASLEXT) \
	$(OUT)/IDENT.$(FASLEXT)

$(OUT)/NETCLT.$(FASLEXT): $(OUT)/IOBCON.$(FASLEXT)
$(OUT)/INETCLTS.$(FASLEXT): $(OUT)/NETCLT.$(FASLEXT)


$(OUT)/IRFORM.$(FASLEXT): $(OUT)/SYNTAX.$(FASLEXT)
$(OUT)/COMPILER.$(FASLEXT): $(OUT)/SYNTAX.$(FASLEXT) $(OUT)/ENV.$(FASLEXT) \
	$(OUT)/ELABOR.$(FASLEXT)
$(OUT)/ITFORM.$(FASLEXT): $(OUT)/IRFORM.$(FASLEXT)
$(OUT)/ELABOR.$(FASLEXT): $(OUT)/ITFORM.$(FASLEXT) $(OUT)/ENV.$(FASLEXT) \
	$(OUT)/IRFORM.$(FASLEXT)


$(OUT)/LINBASIS.$(FASLEXT): $(OUT)/ORDFIN.$(FASLEXT) $(OUT)/OVAR.$(FASLEXT)
$(OUT)/LINELT.$(FASLEXT): $(OUT)/VSPACE.$(FASLEXT) $(OUT)/FM.$(FASLEXT) \
	$(OUT)/LINBASIS.$(FASLEXT)
$(OUT)/DBASIS.$(FASLEXT): $(OUT)/ORDFIN.$(FASLEXT) $(OUT)/KVTFROM.$(FASLEXT)
$(OUT)/LINFORM.$(FASLEXT): $(OUT)/DBASIS.$(FASLEXT) \
	$(OUT)/VSPACE.$(FASLEXT) $(OUT)/LINELT.$(FASLEXT)

$(OUT)/JVMOP.$(FASLEXT): $(OUT)/JVMBCODE.$(FASLEXT)
$(OUT)/JVMCFACC.$(FASLEXT): $(OUT)/LOGIC.$(FASLEXT)
$(OUT)/JVMFDACC.$(FASLEXT): $(OUT)/LOGIC.$(FASLEXT)
$(OUT)/JVMMDACC.$(FASLEXT): $(OUT)/LOGIC.$(FASLEXT)


.PHONY: all-algebra all-algstrap

all-am: all-algebra
all-algebra: stamp
	@ echo finished $(builddir)

stamp: $(SPADFILES) 24-stamp $(TESTS)
	-rm -f stamp
	$(STAMP) stamp

all-algstrap: strap-stamp

.PRECIOUS: $(OUT)/%.$(FASLEXT) %.spad
$(OUT)/%.$(FASLEXT) $(OUT)/%-.$(FASLEXT): %.spad initdb.$(FASLEXT)
	$(COMPILE_SPAD) && cp -p $*.NRLIB/code.$(FASLEXT) $@ \
	  && if test -f $*-.NRLIB/code.$(FASLEXT); then \
	       cp -p $*-.NRLIB/code.$(FASLEXT) $(OUT)/$*-.$(FASLEXT); \
	     else :; fi

$(OUT)/%-.$(FASLEXT): $(OUT)/%.$(FASLEXT)

$(SPADFILES): | $(OUTSRC)

.PRECIOUS: $(OUTSRC)/%.spad
$(OUTSRC)/%.spad: $(srcdir)/%.spad.pamphlet
	$(oa_hammer) --tangle --output=$@ $<

.PRECIOUS: $(builddir)/%.tex
.PRECIOUS: $(builddir)/%.dvi

$(DOC)/%.dvi:  | $(DOC)

$(DOC):
	@ $(MKDIR_P) $(DOC)

$(DOC)/%.dvi: $(builddir)/%.dvi
	@cp -p $< $@

$(builddir)/%.dvi: $(oa_build_texdir)/diagrams.tex \
		   $(oa_build_texdir)/axiom.sty

$(builddir)/%.dvi: $(builddir)/%.tex
	$(oa_hammer) --latex $<

$(builddir)/%.tex: $(srcdir)/%.pamphlet
	$(oa_hammer) --weave --output=$@ $<

$(oa_build_texdir)/diagrams.tex: $(oa_src_docdir)/diagrams.tex
	@cp -p $< $@

SPADPRSR.NRLIB/code.$(FASLEXT): spad-parser.spad
	$(COMPILE_SPAD)

PARSER.NRLIB/code.$(FASLEXT): script-parser.spad
	$(COMPILE_SPAD)


$(INPUT)/TESTFR.input: $(srcdir)/fr.spad.pamphlet
	$(oa_hammer) --tangle='TEST FR' --output=$@ $<

$(INPUT)/INTHEORY.input: $(srcdir)/numtheor.spad.pamphlet
	$(oa_hammer) --tangle='TEST INTHEORY' --output=$@ $<

$(INPUT)/VIEW2D.input: $(srcdir)/view2D.spad.pamphlet
	$(oa_hammer) --tangle='TEST VIEW2D' --output=$@ $<


$(DOC)/diagrams.tex: $(oa_src_docdir)/diagrams.tex
	@cp -p $< $@
 
$(oa_algebra_layer_1_objects): 0-stamp
$(oa_algebra_layer_2_objects): 1-stamp
$(oa_algebra_layer_3_objects): 2-stamp
$(oa_algebra_layer_4_objects): 3-stamp
$(oa_algebra_layer_5_objects): 4-stamp
$(oa_algebra_layer_6_objects): 5-stamp
$(oa_algebra_layer_7_objects): 6-stamp
$(oa_algebra_layer_8_objects): 7-stamp
$(oa_algebra_layer_9_objects): 8-stamp
$(oa_algebra_layer_10_objects): 9-stamp
$(oa_algebra_layer_11_objects): 10-stamp
$(oa_algebra_layer_12_objects): 11-stamp
$(oa_algebra_layer_13_objects): 12-stamp
$(oa_algebra_layer_14_objects): 13-stamp
$(oa_algebra_layer_15_objects): 14-stamp
$(oa_algebra_layer_16_objects): 15-stamp
$(oa_algebra_layer_17_objects): 16-stamp
$(oa_algebra_layer_18_objects): 17-stamp
$(oa_algebra_layer_19_objects): 18-stamp
$(oa_algebra_layer_20_objects): 19-stamp
$(oa_algebra_layer_21_objects): 20-stamp
$(oa_algebra_layer_22_objects): 21-stamp
$(oa_algebra_layer_23_objects): 22-stamp
$(oa_algebra_layer_24_objects): 23-stamp

$(oa_algebra_layer_0_objects): | $(OUT)
$(OUT):
	$(MKDIR_P) $@

0-stamp: $(oa_algebra_layer_0_objects)
1-stamp: $(oa_algebra_layer_1_objects)
2-stamp: $(oa_algebra_layer_2_objects)
3-stamp: $(oa_algebra_layer_3_objects)
4-stamp: $(oa_algebra_layer_4_objects)
5-stamp: $(oa_algebra_layer_5_objects)
6-stamp: $(oa_algebra_layer_6_objects)
7-stamp: $(oa_algebra_layer_7_objects)
8-stamp: $(oa_algebra_layer_8_objects)
9-stamp: $(oa_algebra_layer_9_objects)
10-stamp: $(oa_algebra_layer_10_objects)
11-stamp: $(oa_algebra_layer_11_objects)
12-stamp: $(oa_algebra_layer_12_objects)
13-stamp: $(oa_algebra_layer_13_objects)
14-stamp: $(oa_algebra_layer_14_objects)
15-stamp: $(oa_algebra_layer_15_objects)
16-stamp: $(oa_algebra_layer_16_objects)
17-stamp: $(oa_algebra_layer_17_objects)
18-stamp: $(oa_algebra_layer_18_objects)
19-stamp: $(oa_algebra_layer_19_objects)
20-stamp: $(oa_algebra_layer_20_objects)
21-stamp: $(oa_algebra_layer_21_objects)
22-stamp: $(oa_algebra_layer_22_objects)
23-stamp: $(oa_algebra_layer_23_objects)
24-stamp: $(oa_algebra_layer_24_objects)

ALL_LAYERS = \
	all-layer-0 \
	all-layer-1 \
	all-layer-2 \
	all-layer-3 \
	all-layer-4 \
	all-layer-5 \
	all-layer-6 \
	all-layer-7 \
	all-layer-8 \
	all-layer-9 \
	all-layer-10 \
	all-layer-11 \
	all-layer-12 \
	all-layer-13 \
	all-layer-14 \
	all-layer-15 \
	all-layer-16 \
	all-layer-17 \
	all-layer-18 \
	all-layer-19 \
	all-layer-20 \
	all-layer-21 \
	all-layer-22 \
	all-layer-23 \
	all-layer-24 

.PHONY: $(ALL_LAYERS)
$(ALL_LAYERS): all-layer-%: %-stamp

%-stamp:
	@ rm -f $@
	@ $(STAMP) $@
	@ echo ==================================
	@ echo === layer $* of 24 complete ======
	@ echo ==================================


.PHONY: all-initdb
all-initdb: initdb.$(FASLEXT)

initdb.clisp: $(SPADFILES)
	$(DRIVER) --execpath=$(INTERPSYS) --system="$(oa_targetdir)" \
		--spad-srcdir=$(OUTSRC) \
		--output=initdb.$$$$ --build-initdb && \
		$(top_confdir)/move-if-change initdb.$$$$ $@

initdb.$(FASLEXT): initdb.clisp
	$(DRIVER) --execpath=$(BOOTSYS) --syslib=$(oa_target_libdir) \
		--compile --output=$@ --load-directory=../interp $<

mostlyclean-local:
	@rm -f $(OUT)/*.$(FASLEXT) $(OUT)/*.daase
	@rm -rf *.NRLIB
	@rm -rf *.DAASE *.daase libdb.text
	@rm -rf strap* initdb.*
	@rm -f *stamp

clean-local: mostlyclean-local

distclean-local: clean-local

# We use `sinclude' instead of `include' so that Automake doesn't get
# the idea of wanting to interpret the content of a file that doesn't 
# exist at the time it is run.
sinclude extract.mk
