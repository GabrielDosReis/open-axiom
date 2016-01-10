# Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
# All rights reserved.
# Copyright (C) 2007-2016, Gabriel Dos Reis.
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

top_confdir = $(top_srcdir)/config

OUT=$(oa_targetdir)/algebra
DOC=$(oa_target_docdir)/src/algebra
OUTSRC=$(oa_target_srcdir)/algebra
INPUT=../input


STAMP = echo timestamp >

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
strap-0/SETAGG.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) strap-0/CLAGG.$(FASLEXT)
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
strap-0/TABLE.$(FASLEXT): strap-0/TBAGG.$(FASLEXT) strap-0/INTABL.$(FASLEXT)
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
	strap-0/ALGEBRA.$(FASLEXT) strap-0/FRETRCT.$(FASLEXT) \
	strap-0/GCDDOM.$(FASLEXT) strap-0/REAL.$(FASLEXT) \
	strap-0/UFD.$(FASLEXT) strap-0/KONVERT.$(FASLEXT) \
	strap-0/INS.$(FASLEXT) strap-0/ELTAB.$(FASLEXT) \
	strap-0/EVALAB.$(FASLEXT) strap-0/IEVALAB.$(FASLEXT)

strap-0/PATTERN.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/RETRACT.$(FASLEXT)

strap-0/SEX.$(FASLEXT): strap-0/SEXOF.$(FASLEXT)
strap-0/SEXOF.$(FASLEXT): strap-0/SEXCAT.$(FASLEXT)

strap-0/SYMBOL.$(FASLEXT): strap-0/ORDSET.$(FASLEXT)

strap-0/BOP.$(FASLEXT): strap-0/ORDSET.$(FASLEXT) strap-0/OPERCAT.$(FASLEXT)

strap-0/ALAGG.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) strap-0/TBAGG.$(FASLEXT) \
	strap-0/SMAGG.$(FASLEXT) strap-0/LSAGG.$(FASLEXT)

strap-0/ALIST.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) strap-0/ALAGG.$(FASLEXT)

strap-0/CHAR.$(FASLEXT): strap-0/ORDFIN.$(FASLEXT)

strap-0/KERNEL.$(FASLEXT): strap-0/CACHSET.$(FASLEXT) \
	strap-0/SETCAT.$(FASLEXT) \
	strap-0/ORDSET.$(FASLEXT) strap-0/PATAB.$(FASLEXT)

strap-0/REPSQ.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/REPDB.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/OPERCAT.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/CACHSET.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/LO.$(FASLEXT): strap-0/MODULE.$(FASLEXT) \
	strap-0/COMRING.$(FASLEXT) strap-0/OAGROUP.$(FASLEXT)

strap-0/CCLASS.$(FASLEXT): strap-0/SETCAT.$(FASLEXT) \
	strap-0/KONVERT.$(FASLEXT) strap-0/FSAGG.$(FASLEXT)

strap-0/FSAGG.$(FASLEXT): strap-0/DIAGG.$(FASLEXT) \
	strap-0/SETAGG.$(FASLEXT) strap-0/FINAGG.$(FASLEXT)

strap-0/FLASORT.$(FASLEXT): strap-0/TYPE.$(FASLEXT) strap-0/FLAGG.$(FASLEXT) \
	strap-0/SMAGG.$(FASLEXT)

strap-0/GENPGCD.$(FASLEXT): strap-0/POLYCAT.$(FASLEXT) \
	strap-0/ORDSET.$(FASLEXT) strap-0/OAMONS.$(FASLEXT) \
	strap-0/PFECAT.$(FASLEXT)

strap-0/PFBR.$(FASLEXT): strap-0/PFECAT.$(FASLEXT) strap-0/OAMONS.$(FASLEXT) \
	strap-0/POLYCAT.$(FASLEXT)

strap-0/DDFACT.$(FASLEXT): strap-0/FFIELDC.$(FASLEXT)
strap-0/FFIELDC.$(FASLEXT): strap-0/FPC.$(FASLEXT)
strap-0/INDE.$(FASLEXT): strap-0/IDPC.$(FASLEXT)
strap-0/STREAM.$(FASLEXT): strap-0/LZSTAGG.$(FASLEXT)
strap-0/SAE.$(FASLEXT): strap-0/UPOLYC.$(FASLEXT) strap-0/MONOGEN.$(FASLEXT)
strap-0/MONOGEN.$(FASLEXT): strap-0/COMRING.$(FASLEXT) \
	strap-0/UPOLYC.$(FASLEXT) strap-0/FRAMALG.$(FASLEXT)

strap-0/FRAMALG.$(FASLEXT): strap-0/FINRALG.$(FASLEXT)
strap-0/FINRALG.$(FASLEXT): strap-0/COMRING.$(FASLEXT) \
	strap-0/UPOLYC.$(FASLEXT) strap-0/ALGEBRA.$(FASLEXT)
strap-0/NORMRETR.$(FASLEXT): strap-0/FAXF.$(FASLEXT)
strap-0/FAXF.$(FASLEXT): strap-0/XF.$(FASLEXT)
strap-0/COMPLEX.$(FASLEXT): strap-0/COMPCAT.$(FASLEXT)
strap-0/COMPCAT.$(FASLEXT): strap-0/COMRING.$(FASLEXT) \
	strap-0/MONOGEN.$(FASLEXT)
strap-0/MSET.$(FASLEXT): strap-0/MSETAGG.$(FASLEXT)
strap-0/MSETAGG.$(FASLEXT): strap-0/MDAGG.$(FASLEXT)
strap-0/AN.$(FASLEXT): strap-0/ACF.$(FASLEXT)
strap-0/SEGBIND.$(FASLEXT): strap-0/RNGBIND.$(FASLEXT) \
	strap-0/SEG.$(FASLEXT)
strap-0/EXPR.$(FASLEXT): strap-0/FS.$(FASLEXT) \
	strap-0/ACFS.$(FASLEXT) strap-0/COMBOPC.$(FASLEXT) \
	strap-0/LFCAT.$(FASLEXT) strap-0/SPFCAT.$(FASLEXT)
strap-0/ACFS.$(FASLEXT): strap-0/ACF.$(FASLEXT)
strap-0/IFAMON.$(FASLEXT): strap-0/FAMONC.$(FASLEXT)
strap-0/HDP.$(FASLEXT): strap-0/DIRPCAT.$(FASLEXT)
strap-0/DMP.$(FASLEXT): strap-0/GDMP.$(FASLEXT)
strap-0/LFCAT.$(FASLEXT): strap-0/PRIMCAT.$(FASLEXT)
strap-0/INTRVL.$(FASLEXT): strap-0/INTCAT.$(FASLEXT)
strap-0/FS2UPS.$(FASLEXT): strap-0/UPSCAT.$(FASLEXT) \
	strap-0/PTRANFN.$(FASLEXT)
strap-0/UPSCAT.$(FASLEXT): strap-0/PSCAT.$(FASLEXT)
strap-0/UPXS.$(FASLEXT): strap-0/UPXSCAT.$(FASLEXT) strap-0/UPXSCCA.$(FASLEXT)
strap-0/EFUPXS.$(FASLEXT): strap-0/ULSCAT.$(FASLEXT)
strap-0/ULS.$(FASLEXT): strap-0/ULSCCAT.$(FASLEXT)
strap-0/EFULS.$(FASLEXT): strap-0/UTSCAT.$(FASLEXT)
strap-0/POINT.$(FASLEXT): strap-0/PTCAT.$(FASLEXT)
strap-0/SPACE3.$(FASLEXT): strap-0/SPACEC.$(FASLEXT)
strap-0/LODO.$(FASLEXT): strap-0/LODOCAT.$(FASLEXT)
strap-0/LODOCAT.$(FASLEXT): strap-0/OREPCAT.$(FASLEXT)
strap-0/SASTCAT.$(FASLEXT): strap-0/ASTCAT.$(FASLEXT)
strap-0/ASTCAT.$(FASLEXT): strap-0/HOMOTOP.$(FASLEXT)
strap-0/SPADAST.$(FASLEXT): strap-0/SPADXPT.$(FASLEXT)
strap-0/SPADXPT.$(FASLEXT): strap-0/SASTCAT.$(FASLEXT)

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

strap-1/BOOLE.$(FASLEXT): strap-1/LOGIC.$(FASLEXT) strap-0/INFORM.$(FASLEXT)

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
	strap-0/LIST.$(FASLEXT) strap-0/SUP.$(FASLEXT) \
	strap-0/FRAC.$(FASLEXT)

strap-1/EUCDOM.$(FASLEXT): strap-1/PID.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT) strap-0/LSAGG.$(FASLEXT)

strap-1/UFD.$(FASLEXT): strap-1/GCDDOM.$(FASLEXT) \
	strap-1/BOOLEAN.$(FASLEXT) strap-0/FR.$(FASLEXT)

strap-1/LLINSET.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/RLINSET.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/LINSET.$(FASLEXT): strap-1/LLINSET.$(FASLEXT) \
	strap-1/RLINSET.$(FASLEXT)

strap-1/SGROUP.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) strap-0/REPSQ.$(FASLEXT)

strap-1/ABELSG.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) strap-0/REPDB.$(FASLEXT)

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
	strap-0/TABLE.$(FASLEXT) strap-0/ONECOMP.$(FASLEXT) \
	strap-0/DLP.$(FASLEXT) strap-0/DDFACT.$(FASLEXT)

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
	strap-0/NNI.$(FASLEXT) strap-0/FRAC.$(FASLEXT) \
	strap-0/SYMBOL.$(FASLEXT) strap-0/SEX.$(FASLEXT) \
	strap-0/INTFACT.$(FASLEXT) strap-0/PRIMES.$(FASLEXT) \
	strap-0/COMBINAT.$(FASLEXT) strap-0/PATRES.$(FASLEXT) \
	strap-0/PMINS.$(FASLEXT)

strap-0/INTFACT.$(FASLEXT): strap-0/INS.$(FASLEXT)
strap-0/PRIMES.$(FASLEXT): strap-0/INS.$(FASLEXT)
strap-0/COMBINAT.$(FASLEXT): strap-0/INS.$(FASLEXT)
strap-0/PATRES.$(FASLEXT): strap-0/SETCAT.$(FASLEXT)
strap-0/PMINS.$(FASLEXT): strap-0/INS.$(FASLEXT)

strap-1/PATMAB.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/PATTERN.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/RETRACT.$(FASLEXT) strap-0/SYMBOL.$(FASLEXT) \
	strap-0/BOP.$(FASLEXT) strap-0/ANY1.$(FASLEXT)

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

strap-1/DIFFSPC.$(FASLEXT): strap-1/DIFFDOM.$(FASLEXT) \
	strap-0/NNI.$(FASLEXT)

strap-1/PDRING.$(FASLEXT): strap-1/PDSPC.$(FASLEXT)

strap-1/PDSPC.$(FASLEXT): strap-1/PDDOM.$(FASLEXT) \
	strap-0/NNI.$(FASLEXT)

strap-1/PDDOM.$(FASLEXT): strap-1/TYPE.$(FASLEXT)

strap-1/TRANFUN.$(FASLEXT): strap-1/TRIGCAT.$(FASLEXT) \
	strap-1/ATRIG.$(FASLEXT) strap-1/HYPCAT.$(FASLEXT) \
	strap-1/AHYP.$(FASLEXT) strap-1/ELEMFUN.$(FASLEXT)

strap-1/TRIGCAT.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-1/RING.$(FASLEXT)
strap-1/ATRIG.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-1/RING.$(FASLEXT)
strap-1/HYPCAT.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-1/RING.$(FASLEXT)
strap-1/AHYP.$(FASLEXT): strap-1/TYPE.$(FASLEXT)
strap-1/ELEMFUN.$(FASLEXT): strap-1/TYPE.$(FASLEXT) strap-1/MONOID.$(FASLEXT)

strap-1/IDPC.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) strap-1/FUNCTOR.$(FASLEXT) \
	strap-0/IDPT.$(FASLEXT)

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
	strap-0/INFORM.$(FASLEXT) strap-0/GENPGCD.$(FASLEXT) \
	strap-0/PFBR.$(FASLEXT) strap-0/MULTSQFR.$(FASLEXT) \
	strap-0/PSQFR.$(FASLEXT) strap-0/PMPLCAT.$(FASLEXT) \
	strap-0/POLYLIFT.$(FASLEXT)

strap-1/UPOLYC.$(FASLEXT): strap-1/POLYCAT.$(FASLEXT) \
	strap-1/ELTAB.$(FASLEXT) strap-1/DIFRING.$(FASLEXT) \
	strap-1/DIFEXT.$(FASLEXT) strap-1/STEP.$(FASLEXT) \
	strap-1/COMRING.$(FASLEXT) strap-1/INTDOM.$(FASLEXT) \
	strap-1/FIELD.$(FASLEXT) strap-1/ALGEBRA.$(FASLEXT) \
	strap-1/SAOS.$(FASLEXT) strap-0/PFBRU.$(FASLEXT) \
	strap-0/FR2.$(FASLEXT) strap-0/UPSQFREE.$(FASLEXT)

strap-1/PFECAT.$(FASLEXT): strap-1/UFD.$(FASLEXT) strap-1/CHARZ.$(FASLEXT) \
	strap-0/LPEFRAC.$(FASLEXT)

strap-1/QFCAT.$(FASLEXT): strap-1/FIELD.$(FASLEXT) \
	strap-1/ALGEBRA.$(FASLEXT) strap-1/RETRACT.$(FASLEXT) \
	strap-1/FEVALAB.$(FASLEXT) strap-1/DIFEXT.$(FASLEXT) \
	strap-1/FLINEXP.$(FASLEXT) strap-1/PATAB.$(FASLEXT) \
	strap-1/FPATMAB.$(FASLEXT) strap-1/ORDSET.$(FASLEXT) \
	strap-1/OINTDOM.$(FASLEXT) strap-1/REAL.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/CHARZ.$(FASLEXT) \
	strap-1/CHARNZ.$(FASLEXT) strap-1/EUCDOM.$(FASLEXT) \
	strap-1/PFECAT.$(FASLEXT) strap-0/MCDEN.$(FASLEXT) \
	strap-0/PMQFCAT.$(FASLEXT)

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

strap-1/KDAGG.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/DIAGG.$(FASLEXT) strap-1/IXAGG.$(FASLEXT) \
	strap-1/SMAGG.$(FASLEXT)

strap-1/DIAGG.$(FASLEXT): strap-1/DIOPS.$(FASLEXT)

strap-1/DIOPS.$(FASLEXT): strap-1/BGAGG.$(FASLEXT) strap-1/CLAGG.$(FASLEXT)

strap-1/BGAGG.$(FASLEXT): strap-1/SMAGG.$(FASLEXT)

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

strap-1/A1AGG.$(FASLEXT): strap-1/FLAGG.$(FASLEXT) strap-1/SMAGG.$(FASLEXT) \
	strap-0/FLASORT.$(FASLEXT)

strap-1/FLAGG.$(FASLEXT): strap-1/LNAGG.$(FASLEXT) strap-1/FINAGG.$(FASLEXT)

strap-1/FINAGG.$(FASLEXT): strap-1/HOAGG.$(FASLEXT)

strap-1/LNAGG.$(FASLEXT): strap-1/IXAGG.$(FASLEXT) \
	strap-1/CLAGG.$(FASLEXT) strap-1/ELTAB.$(FASLEXT) \
	strap-0/UNISEG.$(FASLEXT)

strap-1/CLAGG.$(FASLEXT): strap-1/HOAGG.$(FASLEXT)
strap-1/IXAGG.$(FASLEXT): strap-1/HOAGG.$(FASLEXT) \
	strap-1/ELTAGG.$(FASLEXT) strap-0/VOID.$(FASLEXT)

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
	strap-1/VECTOR.$(FASLEXT) strap-0/INTSLPE.$(FASLEXT) \
	strap-0/GALFACT.$(FASLEXT) strap-0/FRUTIL.$(FASLEXT) \
	strap-0/HEUGCD.$(FASLEXT)

strap-1/MAYBE.$(FASLEXT): strap-1/UTYPE.$(FASLEXT) \
	strap-1/RETRACT.$(FASLEXT) strap-1/KOERCE.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT)

strap-1/DFLOAT.$(FASLEXT): strap-1/FPS.$(FASLEXT) \
	strap-1/DIFRING.$(FASLEXT) strap-1/TRANFUN.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-0/DFSFUN.$(FASLEXT)

strap-1/DFSFUN.$(FASLEXT): strap-0/COMPLEX.$(FASLEXT) strap-0/INTRET.$(FASLEXT)

strap-1/FLOAT.$(FASLEXT): strap-1/FPS.$(FASLEXT) \
	strap-1/DIFRING.$(FASLEXT) strap-1/TRANFUN.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/KVTFROM.$(FASLEXT) \
	strap-1/INT.$(FASLEXT) strap-0/IROOT.$(FASLEXT)

strap-1/SUP.$(FASLEXT): strap-1/UPOLYC.$(FASLEXT) strap-1/PR.$(FASLEXT) \
	strap-1/FPC.$(FASLEXT) strap-1/FFIELDC.$(FASLEXT) \
	strap-0/UPMP.$(FASLEXT) strap-0/TWOFACT.$(FASLEXT) \
	strap-0/FFSLPE.$(FASLEXT) strap-0/PRS.$(FASLEXT)

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
	strap-1/IARRAY2.$(FASLEXT) strap-0/MATSTOR.$(FASLEXT) \
	strap-0/MATLIN.$(FASLEXT)

strap-1/IARRAY2.$(FASLEXT): strap-1/ARR2CAT.$(FASLEXT) \
	strap-1/PRIMARR.$(FASLEXT) strap-1/INT.$(FASLEXT) \
	strap-1/FLAGG.$(FASLEXT) strap-0/STRING.$(FASLEXT) 

strap-1/PRIMARR.$(FASLEXT): strap-1/A1AGG.$(FASLEXT)

strap-1/LIST.$(FASLEXT): strap-1/KOERCE.$(FASLEXT) \
	strap-1/SETCAT.$(FASLEXT) strap-1/KONVERT.$(FASLEXT) \
	strap-1/LSAGG.$(FASLEXT)

strap-1/VECTOR.$(FASLEXT): strap-1/VECTCAT.$(FASLEXT) \
	strap-1/ARRAY1.$(FASLEXT)

strap-1/ARRAY1.$(FASLEXT): strap-1/A1AGG.$(FASLEXT) strap-1/IARRAY1.$(FASLEXT)

strap-1/IARRAY1.$(FASLEXT): strap-1/A1AGG.$(FASLEXT) \
	strap-0/PRIMARR.$(FASLEXT)

strap-1/FRAC.$(FASLEXT): strap-1/QFCAT.$(FASLEXT) \
	strap-1/LA.$(FASLEXT) strap-0/UPOLYC2.$(FASLEXT)

strap-1/STRING.$(FASLEXT): strap-1/SRAGG.$(FASLEXT) strap-1/CHAR.$(FASLEXT) \
	strap-1/IDENT.$(FASLEXT)

strap-1/CHAR.$(FASLEXT): strap-1/ORDFIN.$(FASLEXT) strap-1/CCLASS.$(FASLEXT)

strap-1/CCLASS.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-1/KOERCE.$(FASLEXT) \
	strap-1/FSAGG.$(FASLEXT) strap-1/KONVERT.$(FASLEXT) \
	strap-1/IBITS.$(FASLEXT)

strap-1/TABLE.$(FASLEXT): strap-1/TBAGG.$(FASLEXT) \
	strap-0/HASHTBL.$(FASLEXT)

strap-1/IBITS.$(FASLEXT): strap-1/BTAGG.$(FASLEXT)

strap-1/CTOR.$(FASLEXT): strap-1/CTORCAT.$(FASLEXT) \
	strap-1/IDENT.$(FASLEXT) strap-1/MAYBE.$(FASLEXT)

strap-1/CATCTOR.$(FASLEXT): strap-1/CTORCAT.$(FASLEXT) \
	strap-1/KOERCE.$(FASLEXT) strap-1/CTOR.$(FASLEXT)

strap-1/SAOS.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-0/SYMBOL.$(FASLEXT)

strap-1/STREAM.$(FASLEXT): strap-1/LZSTAGG.$(FASLEXT) \
	strap-1/SMAGG.$(FASLEXT) strap-0/STREAM2.$(FASLEXT) \
	strap-1/STREAM3.$(FASLEXT)

strap-1/LZSTAGG.$(FASLEXT): strap-1/STAGG.$(FASLEXT) \
	strap-0/CSTTOOLS.$(FASLEXT)

strap-1/BOP.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) strap-1/OPERCAT.$(FASLEXT) \
	strap-0/SYMBOL.$(FASLEXT) strap-0/NNI.$(FASLEXT) \
	strap-0/BOOLEAN.$(FASLEXT) strap-0/MAYBE.$(FASLEXT) \
	strap-0/LIST.$(FASLEXT) strap-0/ALIST.$(FASLEXT) \
	strap-0/OUTFORM.$(FASLEXT) strap-0/INFORM.$(FASLEXT) \
	strap-0/ARITY.$(FASLEXT) strap-0/NONE.$(FASLEXT)

strap-1/ES.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) strap-1/RETRACT.$(FASLEXT) \
	strap-1/IEVALAB.$(FASLEXT) strap-1/REF.$(FASLEXT) \
	strap-1/ALIST.$(FASLEXT) strap-1/PRIMARR.$(FASLEXT) \
	strap-0/BOP.$(FASLEXT) 	strap-0/SYMBOL.$(FASLEXT) \
	strap-0/KERNEL.$(FASLEXT) strap-0/COMMONOP.$(FASLEXT) \
	strap-0/NONE.$(FASLEXT) strap-0/ARITY.$(FASLEXT) \
	strap-0/BOP1.$(FASLEXT)

strap-1/EQ.$(FASLEXT): strap-1/FUNCTOR.$(FASLEXT) strap-1/IEVALAB.$(FASLEXT) \
	strap-1/SETCAT.$(FASLEXT) strap-1/KOERCE.$(FASLEXT) \
	strap-1/ABELSG.$(FASLEXT) strap-1/ABELGRP.$(FASLEXT) \
	strap-1/MONOID.$(FASLEXT) strap-1/GROUP.$(FASLEXT) \
	strap-1/BMODULE.$(FASLEXT) strap-1/COMRING.$(FASLEXT) \
	strap-1/VSPACE.$(FASLEXT) strap-0/ES.$(FASLEXT) \
	strap-0/POLY.$(FASLEXT) strap-0/MULTFACT.$(FASLEXT) \
	strap-0/INDE.$(FASLEXT) strap-0/CARD.$(FASLEXT)

strap-1/LA.$(FASLEXT): strap-1/ALGEBRA.$(FASLEXT) \
	strap-1/COMRING.$(FASLEXT) strap-0/LO.$(FASLEXT)

strap-1/FR.$(FASLEXT): strap-1/INTDOM.$(FASLEXT) strap-1/DIFEXT.$(FASLEXT) \
	strap-1/ALGEBRA.$(FASLEXT) strap-1/FEVALAB.$(FASLEXT) \
	strap-1/FRETRCT.$(FASLEXT) strap-1/FUNCTOR.$(FASLEXT) \
	strap-0/LIST.$(FASLEXT)

strap-1/INFORM.$(FASLEXT): strap-1/SEXCAT.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/REF.$(FASLEXT) \
	strap-1/ALIST.$(FASLEXT) strap-1/PRIMARR.$(FASLEXT) \
	strap-0/STRING.$(FASLEXT) \
	strap-0/SYMBOL.$(FASLEXT) strap-0/INT.$(FASLEXT) \
	strap-0/DFLOAT.$(FASLEXT) strap-0/OUTFORM.$(FASLEXT) \
	strap-0/ANY.$(FASLEXT)

strap-1/SEGXCAT.$(FASLEXT): strap-1/SEGCAT.$(FASLEXT)

strap-1/SYMBOL.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) \
	strap-1/KOERCE.$(FASLEXT) strap-1/KRCFROM.$(FASLEXT) \
	strap-1/KONVERT.$(FASLEXT) strap-1/KVTFROM.$(FASLEXT) \
	strap-0/REF.$(FASLEXT) strap-0/ALIST.$(FASLEXT) \
	strap-0/PMSYM.$(FASLEXT)

strap-1/INTFACT.$(FASLEXT): strap-1/LMDICT.$(FASLEXT)
strap-1/LMDICT.$(FASLEXT): strap-1/MDAGG.$(FASLEXT)
strap-1/SET.$(FASLEXT): strap-1/FARRAY.$(FASLEXT)
strap-1/FARRAY.$(FASLEXT): strap-1/IFARRAY.$(FASLEXT)
strap-1/UNISEG.$(FASLEXT): strap-1/SEG.$(FASLEXT) strap-0/STREAM.$(FASLEXT)
strap-1/SEG.$(FASLEXT): strap-1/SETCAT.$(FASLEXT)

strap-1/PATRES.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) \
	strap-0/PATTERN.$(FASLEXT) strap-0/PATTERN1.$(FASLEXT)

strap-1/MCDEN.$(FASLEXT): strap-1/LIST2.$(FASLEXT) strap-1/MATCAT2.$(FASLEXT)
strap-1/LIST2.$(FASLEXT): strap-1/FLAGG2.$(FASLEXT)

strap-1/PMQFCAT.$(FASLEXT): strap-0/PMDOWN.$(FASLEXT)
strap-1/PFBR.$(FASLEXT): strap-0/COMMUPC.$(FASLEXT)
strap-1/MULTSQFR.$(FASLEXT): strap-0/GENEEZ.$(FASLEXT) \
	strap-0/MLIFT.$(FASLEXT) strap-0/PGCD.$(FASLEXT) \
	strap-0/FACUTIL.$(FASLEXT)

strap-1/PMPLCAT.$(FASLEXT): strap-0/PMTOOLS.$(FASLEXT)
strap-1/DDFACT.$(FASLEXT): strap-1/MODMON.$(FASLEXT)
strap-1/MODMON.$(FASLEXT): strap-1/UPOLYC.$(FASLEXT)
strap-1/TWOFACT.$(FASLEXT): strap-1/FFX.$(FASLEXT) \
	strap-0/GHENSEL.$(FASLEXT) strap-0/NORMRETR.$(FASLEXT)
strap-1/FFX.$(FASLEXT): strap-1/FFP.$(FASLEXT) strap-0/FFPOLY.$(FASLEXT)
strap-1/FFP.$(FASLEXT): strap-1/FFIELDC.$(FASLEXT) strap-1/SUP.$(FASLEXT) \
	strap-1/FAXF.$(FASLEXT)
strap-1/FAXF.$(FASLEXT): strap-1/XF.$(FASLEXT) strap-0/SAE.$(FASLEXT)
strap-1/MATLIN.$(FASLEXT): strap-0/IMATLIN.$(FASLEXT) strap-0/IMATQF.$(FASLEXT)
strap-1/GALFACT.$(FASLEXT): strap-1/GALFACTU.$(FASLEXT) \
	strap-1/GALPOLYU.$(FASLEXT) strap-0/MDDFACT.$(FASLEXT) \
	strap-0/UPDECOMP.$(FASLEXT) strap-0/BRILL.$(FASLEXT) \
	strap-0/CYCLOTOM.$(FASLEXT)
strap-1/GALFACTU.$(FASLEXT): strap-1/FPS.$(FASLEXT) strap-1/RING.$(FASLEXT) \
	strap-1/UPOLYC.$(FASLEXT) strap-1/GALUTIL.$(FASLEXT)
strap-1/GALUTIL.$(FASLEXT): strap-1/RING.$(FASLEXT)
strap-1/GALPOLYU.$(FASLEXT): strap-1/UPOLYC.$(FASLEXT) \
	strap-1/RING.$(FASLEXT) strap-0/MSET.$(FASLEXT)
strap-1/HEUGCD.$(FASLEXT): strap-1/INMODGCD.$(FASLEXT)
strap-1/INMODGCD.$(FASLEXT): strap-0/EUCDOM.$(FASLEXT) \
	strap-0/UPOLYC.$(FASLEXT) strap-0/EMR.$(FASLEXT)
strap-1/ANY1.$(FASLEXT): strap-1/NONE1.$(FASLEXT)
strap-1/POLY.$(FASLEXT): strap-1/SMP.$(FASLEXT) strap-0/UDPO.$(FASLEXT)
strap-1/MULTFACT.$(FASLEXT): strap-0/MRATFAC.$(FASLEXT) \
	strap-0/MPRFF.$(FASLEXT) strap-0/INNMFACT.$(FASLEXT) \
	strap-0/GENUFACT.$(FASLEXT)
strap-1/INDE.$(FASLEXT): strap-1/IDPOAMS.$(FASLEXT) strap-1/IDPC.$(FASLEXT)
strap-1/IDPOAMS.$(FASLEXT): strap-1/IDPOAM.$(FASLEXT)
strap-1/IDPC.$(FASLEXT): strap-1/SETCAT.$(FASLEXT) strap-1/FUNCTOR.$(FASLEXT)
strap-1/GENEEZ.$(FASLEXT): strap-1/VECTOR2.$(FASLEXT)
strap-1/MLIFT.$(FASLEXT): strap-1/NPCOEF.$(FASLEXT) strap-1/SUP2.$(FASLEXT) \
	strap-1/PGCD.$(FASLEXT) strap-1/FACUTIL.$(FASLEXT)

strap-1/PMDOWN.$(FASLEXT): strap-1/PATRES2.$(FASLEXT)
strap-1/BOP1.$(FASLEXT): strap-1/NONE.$(FASLEXT) strap-1/ARITY.$(FASLEXT)
strap-1/KERNEL.$(FASLEXT): strap-1/CACHSET.$(FASLEXT) strap-1/SCACHE.$(FASLEXT)
strap-1/UPDECOMP.$(FASLEXT): strap-0/UPDIVP.$(FASLEXT)
strap-1/COMPLEX.$(FASLEXT): strap-1/COMPCAT.$(FASLEXT)
strap-1/COMPCAT.$(FASLEXT): strap-0/CINTSLPE.$(FASLEXT) \
	strap-0/COMPLPAT.$(FASLEXT) strap-0/CPMATCH.$(FASLEXT)
strap-1/MONOGEN.$(FASLEXT): strap-0/CPIMA.$(FASLEXT)
strap-1/ALIST.$(FASLEXT): strap-1/ALAGG.$(FASLEXT)

strap-1/MPRFF.$(FASLEXT): strap-0/MFINFACT.$(FASLEXT)
strap-1/INNMFACT.$(FASLEXT): strap-0/LEADCDET.$(FASLEXT) \
	strap-0/AN.$(FASLEXT)
strap-1/GENUFACT.$(FASLEXT): strap-0/OUT.$(FASLEXT) \
	strap-0/RATFACT.$(FASLEXT) strap-0/COMPFACT.$(FASLEXT) \
	strap-0/ALGFACT.$(FASLEXT) strap-0/UP.$(FASLEXT) \
	strap-0/SAEFACT.$(FASLEXT) strap-0/SAERFFC.$(FASLEXT)
strap-1/EMR.$(FASLEXT): strap-0/MODRING.$(FASLEXT)
strap-1/MFINFACT.$(FASLEXT): strap-0/MPC2.$(FASLEXT)
strap-1/ACF.$(FASLEXT): strap-0/NTPOLFN.$(FASLEXT) strap-0/PDECOMP.$(FASLEXT)
strap-1/FS.$(FASLEXT): strap-0/MKUCFUNC.$(FASLEXT) strap-1/ES.$(FASLEXT) \
	strap-0/LIST2MAP.$(FASLEXT) strap-0/INFORM1.$(FASLEXT) \
	strap-0/POLYCATQ.$(FASLEXT)
strap-1/LF.$(FASLEXT): strap-0/SEGBIND.$(FASLEXT)
strap-1/LFCAT.$(FASLEXT): strap-1/PRIMCAT.$(FASLEXT)
strap-1/ALGMANIP.$(FASLEXT): strap-1/POLYROOT.$(FASLEXT) \
	strap-1/POLYCATQ.$(FASLEXT) strap-1/FS.$(FASLEXT)
strap-1/POLYROOT.$(FASLEXT): strap-0/FACTFUNC.$(FASLEXT)
strap-1/AN.$(FASLEXT): strap-1/IAN.$(FASLEXT)
strap-1/IAN.$(FASLEXT): strap-0/EXPR.$(FASLEXT) strap-0/EXPR2.$(FASLEXT)
strap-1/EXPR2.$(FASLEXT): strap-0/FS2.$(FASLEXT) strap-0/ES2.$(FASLEXT)
strap-1/MKUCFUNC.$(FASLEXT): strap-0/MKFUNC.$(FASLEXT)
strap-1/FAGROUP.$(FASLEXT): strap-1/FAMONC.$(FASLEXT) \
	strap-0/IFAMON.$(FASLEXT)
strap-1/CMPLXRT.$(FASLEXT): strap-0/INFSP.$(FASLEXT)
strap-1/POLTOPOL.$(FASLEXT): strap-1/OVAR.$(FASLEXT) \
	strap-1/HDMP.$(FASLEXT) strap-0/MPC3.$(FASLEXT) \
	strap-0/DMP.$(FASLEXT) strap-0/DIRPROD.$(FASLEXT)
strap-1/HDMP.$(FASLEXT): strap-1/GDMP.$(FASLEXT) strap-0/HDP.$(FASLEXT)
strap-1/GDMP.$(FASLEXT): strap-1/DIRPCAT.$(FASLEXT)
strap-1/GROEBSOL.$(FASLEXT): strap-0/GB.$(FASLEXT) \
	strap-1/GBINTERN.$(FASLEXT) strap-1/LGROBP.$(FASLEXT) \
	strap-1/GENMFACT.$(FASLEXT)
strap-1/GENMFACT.$(FASLEXT): strap-0/MPCPF.$(FASLEXT)
strap-1/COMBF.$(FASLEXT): strap-0/SEGBIND.$(FASLEXT)
strap-1/ACFS.$(FASLEXT): strap-1/ACF.$(FASLEXT)
strap-1/MPCPF.$(FASLEXT): strap-1/PUSHVAR.$(FASLEXT) \
	strap-0/GENMFACT.$(FASLEXT)
strap-1/SIGNEF.$(FASLEXT): strap-0/TOOLSIGN.$(FASLEXT) \
	strap-0/SIGNRF.$(FASLEXT) strap-0/LIMITPS.$(FASLEXT) \
	strap-0/TRIGMNIP.$(FASLEXT) strap-0/ORDCOMP.$(FASLEXT) \
	strap-0/INTRVL.$(FASLEXT)
strap-1/SIGNRF.$(FASLEXT): strap-0/INPSIGN.$(FASLEXT) \
	strap-0/TOOLSIGN.$(FASLEXT) strap-0/ORDCOMP.$(FASLEXT)
strap-1/LIMITPS.$(FASLEXT): strap-0/EFSTRUC.$(FASLEXT) \
	strap-0/FS2UPS.$(FASLEXT) strap-0/UPXS.$(FASLEXT) \
	strap-0/EFUPXS.$(FASLEXT) strap-0/ULS.$(FASLEXT) \
	strap-0/EFULS.$(FASLEXT) strap-0/UTS.$(FASLEXT) \
	strap-0/FS2EXPXP.$(FASLEXT) strap-0/EXPEXPAN.$(FASLEXT) \
	strap-0/SIGNEF.$(FASLEXT)
strap-1/TRIGMNIP.$(FASLEXT): strap-0/ITRIGMNP.$(FASLEXT)
strap-1/ULS.$(FASLEXT): strap-0/VARIABLE.$(FASLEXT) \
	strap-0/ULSCONS.$(FASLEXT)
strap-1/UTS.$(FASLEXT): strap-1/ITAYLOR.$(FASLEXT) \
	strap-0/FSINT.$(FASLEXT)
strap-1/ITAYLOR.$(FASLEXT): strap-0/STTAYLOR.$(FASLEXT)
strap-1/STTAYLOR.$(FASLEXT): strap-0/YSTREAM.$(FASLEXT)
strap-1/FSINT.$(FASLEXT): strap-0/INTTOOLS.$(FASLEXT) \
	strap-0/INTEF.$(FASLEXT) strap-0/IR2F.$(FASLEXT) \
	strap-0/TRMANIP.$(FASLEXT) strap-0/IR2.$(FASLEXT) \
	strap-0/FSCINT.$(FASLEXT) strap-0/IR.$(FASLEXT)
strap-1/UP.$(FASLEXT): strap-0/VARIABLE.$(FASLEXT)
strap-1/RULE.$(FASLEXT): strap-0/APPRULE.$(FASLEXT) \
	strap-0/PMASSFS.$(FASLEXT)
strap-1/PTPACK.$(FASLEXT): strap-0/POINT.$(FASLEXT)
strap-1/PLOTTOOL.$(FASLEXT): strap-0/GRDEF.$(FASLEXT) \
	strap-1/PLOT.$(FASLEXT) strap-1/CLIP.$(FASLEXT) \
	strap-1/DROPT0.$(FASLEXT) strap-1/DROPT1.$(FASLEXT) \
	strap-0/VIEW.$(FASLEXT)

strap-1/DROPT0.$(FASLEXT): strap-1/DROPT1.$(FASLEXT)
strap-1/DROPT1.$(FASLEXT): strap-1/DROPT.$(FASLEXT)
strap-1/DROPT.$(FASLEXT): strap-0/SPACE3.$(FASLEXT)
strap-1/ODEEF.$(FASLEXT): strap-0/NODE1.$(FASLEXT) \
	strap-0/LODO.$(FASLEXT) strap-0/ODESYS.$(FASLEXT) \
	strap-0/LODEEF.$(FASLEXT)
strap-1/ASTCAT.$(FASLEXT): strap-0/SYNTAX.$(FASLEXT) \
	strap-0/HOMOTOP.$(FASLEXT)
strap-1/IMPTAST.$(FASLEXT): strap-0/TYPEAST.$(FASLEXT)
strap-1/DEFAST.$(FASLEXT): strap-0/HEADAST.$(FASLEXT) \
	strap-0/SPADAST.$(FASLEXT) strap-0/SIG.$(FASLEXT)
strap-1/CATAST.$(FASLEXT): strap-0/CTORKIND.$(FASLEXT)
strap-1/HEADAST.$(FASLEXT): strap-0/PARAMAST.$(FASLEXT)
strap-1/ITFORM.$(FASLEXT): strap-0/IRFORM.$(FASLEXT)
strap-1/LINBASIS.$(FASLEXT): strap-0/DBASIS.$(FASLEXT)
strap-1/FR2.$(FASLEXT): strap-0/COMPLEX.$(FASLEXT)

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
	strap-1/STRING.$(FASLEXT) strap-1/OUTFORM.$(FASLEXT)
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
strap-2/SGROUP.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-1/REPSQ.$(FASLEXT) strap-1/PI.$(FASLEXT)
strap-2/ABELSG.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-1/PI.$(FASLEXT) strap-1/REPDB.$(FASLEXT)
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
	strap-1/LSAGG.$(FASLEXT) strap-1/PFECAT.$(FASLEXT) \
	strap-1/FRAC.$(FASLEXT)
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
strap-2/DIFFSPC.$(FASLEXT): strap-2/DIFFDOM.$(FASLEXT) \
	strap-1/NNI.$(FASLEXT)
strap-2/DIFRING.$(FASLEXT): strap-2/RING.$(FASLEXT) \
	strap-2/DIFFSPC.$(FASLEXT)
strap-2/DSEXT.$(FASLEXT): strap-2/DIFFSPC.$(FASLEXT) \
	strap-2/PDSPC.$(FASLEXT) strap-1/SYMBOL.$(FASLEXT)
strap-2/PDDOM.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/PDSPC.$(FASLEXT): strap-2/PDDOM.$(FASLEXT) \
	strap-1/NNI.$(FASLEXT)
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
	strap-2/FR.$(FASLEXT) strap-1/BOOLEAN.$(FASLEXT)
strap-2/INS.$(FASLEXT): strap-2/UFD.$(FASLEXT) strap-2/EUCDOM.$(FASLEXT) \
	strap-2/OINTDOM.$(FASLEXT) strap-2/DIFRING.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/RETRACT.$(FASLEXT) \
	strap-2/LINEXP.$(FASLEXT) strap-2/CFCAT.$(FASLEXT) \
	strap-2/REAL.$(FASLEXT) strap-2/CHARZ.$(FASLEXT) \
	strap-2/STEP.$(FASLEXT) strap-2/PATMAB.$(FASLEXT) \
	strap-2/FEVALAB.$(FASLEXT) strap-1/FRAC.$(FASLEXT) \
	strap-1/FLOAT.$(FASLEXT) strap-1/DFLOAT.$(FASLEXT) \
	strap-1/INFORM.$(FASLEXT) strap-1/SEXCAT.$(FASLEXT) \
	strap-1/PATTERN.$(FASLEXT) strap-1/PRIMES.$(FASLEXT) \
	strap-1/COMBINAT.$(FASLEXT) strap-1/PATRES.$(FASLEXT) \
	strap-1/PMINS.$(FASLEXT)
strap-2/FPS.$(FASLEXT): strap-2/RNS.$(FASLEXT)
strap-2/RNS.$(FASLEXT): strap-2/FIELD.$(FASLEXT) strap-2/ORDRING.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-2/RADCAT.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/PATMAB.$(FASLEXT) \
	strap-2/CHARZ.$(FASLEXT)
strap-2/RADCAT.$(FASLEXT): strap-1/INS.$(FASLEXT)
strap-2/PFECAT.$(FASLEXT): strap-2/UFD.$(FASLEXT) strap-2/CHARZ.$(FASLEXT) \
	strap-1/LPEFRAC.$(FASLEXT)
strap-2/QFCAT.$(FASLEXT): strap-2/ALGEBRA.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) \
	strap-2/FEVALAB.$(FASLEXT) strap-2/DIFEXT.$(FASLEXT) \
	strap-2/FLINEXP.$(FASLEXT) strap-2/PATAB.$(FASLEXT) \
	strap-2/FPATMAB.$(FASLEXT) strap-2/ORDSET.$(FASLEXT) \
	strap-2/OINTDOM.$(FASLEXT) strap-2/REAL.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/CHARZ.$(FASLEXT) \
	strap-2/CHARNZ.$(FASLEXT) strap-2/EUCDOM.$(FASLEXT) \
	strap-2/PFECAT.$(FASLEXT) strap-1/FIELD.$(FASLEXT) \
	strap-1/MCDEN.$(FASLEXT) strap-1/PMQFCAT.$(FASLEXT)

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
strap-2/TRIGCAT.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-2/RING.$(FASLEXT)
strap-2/ATRIG.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-2/RING.$(FASLEXT)
strap-2/HYPCAT.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-2/RING.$(FASLEXT)
strap-2/AHYP.$(FASLEXT): strap-2/TYPE.$(FASLEXT)
strap-2/ELEMFUN.$(FASLEXT): strap-2/TYPE.$(FASLEXT) strap-1/MONOID.$(FASLEXT)
strap-2/IDPC.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) strap-2/FUNCTOR.$(FASLEXT) \
	strap-1/IDPT.$(FASLEXT)
strap-2/AMR.$(FASLEXT): strap-2/RING.$(FASLEXT) strap-2/BMODULE.$(FASLEXT) \
	strap-2/ALGEBRA.$(FASLEXT) strap-2/FIELD.$(FASLEXT) \
	strap-2/CHARZ.$(FASLEXT) strap-2/CHARNZ.$(FASLEXT) \
	strap-2/FUNCTOR.$(FASLEXT)
strap-2/FAMR.$(FASLEXT): strap-2/AMR.$(FASLEXT) \
	strap-2/FRETRCT.$(FASLEXT)
strap-2/POLYCAT.$(FASLEXT): strap-2/PDRING.$(FASLEXT) \
	strap-2/FAMR.$(FASLEXT) strap-2/EVALAB.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-2/FLINEXP.$(FASLEXT) \
	strap-1/VECTOR.$(FASLEXT) strap-1/MATRIX.$(FASLEXT) \
	strap-1/LIST.$(FASLEXT) strap-1/SUP.$(FASLEXT) \
	strap-1/INFORM.$(FASLEXT) strap-1/GENPGCD.$(FASLEXT) \
	strap-1/PFBR.$(FASLEXT) strap-1/MULTSQFR.$(FASLEXT) \
	strap-1/PSQFR.$(FASLEXT) strap-1/PMPLCAT.$(FASLEXT) \
	strap-1/POLYLIFT.$(FASLEXT)
strap-2/UPOLYC.$(FASLEXT): strap-2/POLYCAT.$(FASLEXT) \
	strap-2/ELTAB.$(FASLEXT) strap-2/DIFRING.$(FASLEXT) \
	strap-2/DIFEXT.$(FASLEXT) strap-2/STEP.$(FASLEXT) \
	strap-2/COMRING.$(FASLEXT) strap-2/INTDOM.$(FASLEXT) \
	strap-2/FIELD.$(FASLEXT) strap-2/ALGEBRA.$(FASLEXT) \
	strap-1/PFBRU.$(FASLEXT) strap-1/FR2.$(FASLEXT) \
	strap-1/UPSQFREE.$(FASLEXT)
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
strap-2/A1AGG.$(FASLEXT): strap-2/FLAGG.$(FASLEXT) strap-2/SMAGG.$(FASLEXT) \
	strap-1/FLASORT.$(FASLEXT)
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
	strap-1/FLOAT.$(FASLEXT) strap-1/INTSLPE.$(FASLEXT) \
	strap-1/GALFACT.$(FASLEXT) strap-1/FRUTIL.$(FASLEXT) \
	strap-1/HEUGCD.$(FASLEXT)
strap-2/DFLOAT.$(FASLEXT): strap-2/FPS.$(FASLEXT) \
	strap-2/DIFRING.$(FASLEXT) strap-2/TRANFUN.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-1/DFSFUN.$(FASLEXT)
strap-2/FLOAT.$(FASLEXT): strap-2/FPS.$(FASLEXT) \
	strap-2/DIFRING.$(FASLEXT) strap-2/TRANFUN.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/KVTFROM.$(FASLEXT) \
	strap-2/INT.$(FASLEXT) strap-1/IROOT.$(FASLEXT) \
	strap-1/STRING.$(FASLEXT)
strap-2/SUP.$(FASLEXT): strap-2/UPOLYC.$(FASLEXT) strap-2/PR.$(FASLEXT) \
	strap-2/FPC.$(FASLEXT) strap-2/FFIELDC.$(FASLEXT)  \
	strap-1/UPMP.$(FASLEXT) strap-1/TWOFACT.$(FASLEXT) \
	strap-1/FFSLPE.$(FASLEXT) strap-1/PRS.$(FASLEXT)
strap-2/FFIELDC.$(FASLEXT): strap-2/FPC.$(FASLEXT) strap-2/STEP.$(FASLEXT) \
	strap-2/DIFRING.$(FASLEXT) strap-2/PFECAT.$(FASLEXT)  \
	strap-1/TABLE.$(FASLEXT) strap-1/ONECOMP.$(FASLEXT) \
	strap-1/DLP.$(FASLEXT) strap-1/DDFACT.$(FASLEXT)
strap-2/FPC.$(FASLEXT): strap-2/FIELD.$(FASLEXT)
strap-2/PR.$(FASLEXT): strap-2/FAMR.$(FASLEXT) strap-2/FM.$(FASLEXT)
strap-2/FM.$(FASLEXT): strap-2/IDPAG.$(FASLEXT)
strap-2/IDPAG.$(FASLEXT): strap-2/ABELGRP.$(FASLEXT) \
	strap-2/IDPAM.$(FASLEXT)
strap-2/IDPAM.$(FASLEXT): strap-2/ABELMON.$(FASLEXT)  \
	strap-2/IDPC.$(FASLEXT) strap-2/IDPO.$(FASLEXT)
strap-2/IDPO.$(FASLEXT): strap-2/IDPC.$(FASLEXT)
strap-2/VECTOR.$(FASLEXT): strap-2/VECTCAT.$(FASLEXT) \
	strap-2/ARRAY1.$(FASLEXT)
strap-2/ARRAY1.$(FASLEXT): strap-1/A1AGG.$(FASLEXT) \
	strap-1/PRIMARR.$(FASLEXT) strap-1/NNI.$(FASLEXT) \
	strap-1/INT.$(FASLEXT)
strap-2/VECTCAT.$(FASLEXT): strap-1/A1AGG.$(FASLEXT) \
	strap-1/INT.$(FASLEXT)
strap-2/IARRAY1.$(FASLEXT): strap-2/A1AGG.$(FASLEXT) \
	strap-2/PRIMARR.$(FASLEXT)
strap-2/PRIMARR.$(FASLEXT): strap-2/A1AGG.$(FASLEXT)
strap-2/PAIR.$(FASLEXT): strap-2/KOERCE.$(FASLEXT) \
	strap-2/SETCAT.$(FASLEXT) strap-0/OUTFORM.$(FASLEXT)
strap-2/MATRIX.$(FASLEXT): strap-2/MATCAT.$(FASLEXT) \
	strap-2/IARRAY2.$(FASLEXT) strap-1/MATSTOR.$(FASLEXT) \
	strap-1/MATLIN.$(FASLEXT)
strap-2/IARRAY2.$(FASLEXT): strap-2/ARR2CAT.$(FASLEXT) \
	strap-2/PRIMARR.$(FASLEXT) strap-1/STRING.$(FASLEXT)
strap-2/INFORM.$(FASLEXT): strap-2/SEXCAT.$(FASLEXT) \
	strap-2/KONVERT.$(FASLEXT) strap-2/ANY.$(FASLEXT) \
	strap-2/SEX.$(FASLEXT) strap-2/LIST.$(FASLEXT) \
	strap-2/SYMBOL.$(FASLEXT)
strap-2/LIST.$(FASLEXT): strap-2/LSAGG.$(FASLEXT)
strap-2/SEX.$(FASLEXT): strap-2/SEXOF.$(FASLEXT)
strap-2/OUTFORM.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-1/NUMFMT.$(FASLEXT) strap-1/VOID.$(FASLEXT) \
	strap-1/DFLOAT.$(FASLEXT) strap-1/SYMBOL.$(FASLEXT) \
	strap-1/LIST.$(FASLEXT)
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
	strap-2/STRING.$(FASLEXT) strap-1/PMSYM.$(FASLEXT)
strap-2/ALIST.$(FASLEXT): strap-2/ALAGG.$(FASLEXT) strap-2/REF.$(FASLEXT)
strap-2/REF.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)
strap-2/STRING.$(FASLEXT): strap-2/SRAGG.$(FASLEXT) \
	strap-2/CHAR.$(FASLEXT)

strap-2/FRAC.$(FASLEXT): strap-2/QFCAT.$(FASLEXT) strap-1/GCDDOM.$(FASLEXT) \
	strap-1/LA.$(FASLEXT) strap-1/EUCDOM.$(FASLEXT) \
	strap-1/INS.$(FASLEXT) strap-1/SUP.$(FASLEXT) \
	strap-1/PFECAT.$(FASLEXT) strap-1/FR.$(FASLEXT) \
	strap-1/CHARNZ.$(FASLEXT) strap-1/SEGCAT.$(FASLEXT) \
	strap-1/SEGXCAT.$(FASLEXT) strap-1/UPOLYC2.$(FASLEXT)

strap-2/CHAR.$(FASLEXT): strap-2/ORDFIN.$(FASLEXT) strap-2/CCLASS.$(FASLEXT)
strap-2/CCLASS.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/KOERCE.$(FASLEXT) \
	strap-2/FSAGG.$(FASLEXT) strap-2/KONVERT.$(FASLEXT) \
	strap-2/IBITS.$(FASLEXT)
strap-2/TABLE.$(FASLEXT): strap-2/TBAGG.$(FASLEXT)
strap-2/IBITS.$(FASLEXT): strap-2/BTAGG.$(FASLEXT) \
	strap-1/NNI.$(FASLEXT) strap-1/INT.$(FASLEXT)
strap-2/MAYBE.$(FASLEXT): strap-2/UTYPE.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-2/KOERCE.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT)
strap-2/BOOLEAN.$(FASLEXT): strap-2/PROPLOG.$(FASLEXT) \
	strap-2/ORDFIN.$(FASLEXT) strap-2/KONVERT.$(FASLEXT) \
	strap-1/PI.$(FASLEXT) strap-1/INFORM.$(FASLEXT)

strap-2/EQ.$(FASLEXT): strap-2/FUNCTOR.$(FASLEXT) strap-2/SETCAT.$(FASLEXT) \
	strap-2/KOERCE.$(FASLEXT) strap-2/EVALAB.$(FASLEXT) \
	strap-2/ABELSG.$(FASLEXT) strap-2/ABELGRP.$(FASLEXT) \
	strap-2/SGROUP.$(FASLEXT) strap-2/MONOID.$(FASLEXT) \
	strap-2/RING.$(FASLEXT) strap-2/BMODULE.$(FASLEXT) \
	strap-2/COMRING.$(FASLEXT) strap-2/MODULE.$(FASLEXT) \
	strap-2/INTDOM.$(FASLEXT) strap-2/PDRING.$(FASLEXT) \
	strap-2/FIELD.$(FASLEXT) strap-2/VSPACE.$(FASLEXT) \
	strap-2/ES.$(FASLEXT) strap-2/GROUP.$(FASLEXT) \
	strap-1/POLY.$(FASLEXT) strap-1/MULTFACT.$(FASLEXT) \
	strap-1/INDE.$(FASLEXT) strap-1/CARD.$(FASLEXT)

strap-2/VSPACE.$(FASLEXT): strap-2/FIELD.$(FASLEXT) strap-2/MODULE.$(FASLEXT)

strap-2/GROUP.$(FASLEXT): strap-2/MONOID.$(FASLEXT)

strap-2/ES.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) strap-2/RETRACT.$(FASLEXT) \
	strap-2/IEVALAB.$(FASLEXT) strap-2/EVALAB.$(FASLEXT) \
	strap-2/RING.$(FASLEXT) strap-2/RETRACT.$(FASLEXT) \
	strap-2/CACHSET.$(FASLEXT) strap-1/BOP1.$(FASLEXT) \
	strap-1/KERNEL.$(FASLEXT) strap-1/COMMONOP.$(FASLEXT) \
	strap-1/BOP.$(FASLEXT)

strap-2/CACHSET.$(FASLEXT): strap-2/SETCAT.$(FASLEXT)

strap-2/BTAGG.$(FASLEXT): strap-1/ORDSET.$(FASLEXT) \
	strap-1/BOOLE.$(FASLEXT) strap-1/LOGIC.$(FASLEXT) \
	strap-1/A1AGG.$(FASLEXT)

strap-2/UNISEG.$(FASLEXT): strap-2/SEGCAT.$(FASLEXT) \
	strap-2/SEGXCAT.$(FASLEXT) strap-2/ORDSET.$(FASLEXT) \
	strap-2/SETCAT.$(FASLEXT) strap-1/STREAM.$(FASLEXT)

strap-2/PATTERN.$(FASLEXT): strap-2/SETCAT.$(FASLEXT) \
	strap-2/RETRACT.$(FASLEXT) strap-1/BOP.$(FASLEXT) \
	strap-1/NNI.$(FASLEXT) strap-1/ABELMON.$(FASLEXT) \
	strap-1/ANY1.$(FASLEXT) strap-1/ANY.$(FASLEXT) \
	strap-1/PATTERN1.$(FASLEXT)

strap-2/FR.$(FASLEXT): strap-2/INTDOM.$(FASLEXT) strap-2/INTDOM.$(FASLEXT) \
	strap-2/DIFEXT.$(FASLEXT) strap-2/ALGEBRA.$(FASLEXT) \
	strap-2/FEVALAB.$(FASLEXT) strap-2/FRETRCT.$(FASLEXT) \
	strap-2/FUNCTOR.$(FASLEXT) strap-2/VOID.$(FASLEXT) \
	strap-1/LIST.$(FASLEXT) strap-1/SEX.$(FASLEXT) \
	strap-1/INT.$(FASLEXT) strap-1/NNI.$(FASLEXT) \
	strap-1/INFORM.$(FASLEXT) strap-1/INS.$(FASLEXT) \
	strap-1/REAL.$(FASLEXT) strap-1/IEVALAB.$(FASLEXT) \
	strap-1/FLOAT.$(FASLEXT) strap-1/DFLOAT.$(FASLEXT) \
	strap-1/INTFACT.$(FASLEXT) strap-1/SET.$(FASLEXT) \
	strap-1/UNISEG.$(FASLEXT) strap-1/REF.$(FASLEXT) \
	strap-1/ALIST.$(FASLEXT) strap-1/PRIMARR.$(FASLEXT)

strap-2/KERNEL.$(FASLEXT): strap-1/CACHSET.$(FASLEXT) \
	strap-1/SCACHE.$(FASLEXT) strap-1/ARITY.$(FASLEXT) \
	strap-1/NONE.$(FASLEXT)

strap-2/PFBR.$(FASLEXT): strap-1/COMMUPC.$(FASLEXT)
strap-2/MULTSQFR.$(FASLEXT): strap-1/GENEEZ.$(FASLEXT) \
	strap-1/MLIFT.$(FASLEXT)
strap-2/PMPLCAT.$(FASLEXT): strap-1/PMTOOLS.$(FASLEXT) \
	strap-1/PMDOWN.$(FASLEXT)

strap-2/GALFACT.$(FASLEXT): strap-1/GALFACTU.$(FASLEXT) \
	strap-1/GALPOLYU.$(FASLEXT) strap-1/MDDFACT.$(FASLEXT) \
	strap-1/UPDECOMP.$(FASLEXT) strap-1/BRILL.$(FASLEXT) \
	strap-1/CYCLOTOM.$(FASLEXT) strap-1/MSET.$(FASLEXT) \
	strap-1/GHENSEL.$(FASLEXT)

strap-2/DFSFUN.$(FASLEXT): strap-1/COMPLEX.$(FASLEXT) \
	strap-1/INTRET.$(FASLEXT) strap-1/MONOGEN.$(FASLEXT) \
	strap-1/FRAMALG.$(FASLEXT) strap-1/FINRALG.$(FASLEXT)

strap-2/POLY.$(FASLEXT): strap-2/UDPO.$(FASLEXT)

strap-2/MULTFACT.$(FASLEXT): strap-1/MRATFAC.$(FASLEXT) \
	strap-1/MPRFF.$(FASLEXT) strap-1/INNMFACT.$(FASLEXT) \
	strap-1/GENUFACT.$(FASLEXT)

strap-2/MONOGEN.$(FASLEXT): strap-1/CPIMA.$(FASLEXT)

strap-2/COMPCAT.$(FASLEXT): strap-1/CINTSLPE.$(FASLEXT) \
	strap-1/COMPLPAT.$(FASLEXT) strap-1/CPMATCH.$(FASLEXT)
strap-2/GENEEZ.$(FASLEXT): strap-1/EMR.$(FASLEXT)
strap-2/MPRFF.$(FASLEXT): strap-1/MFINFACT.$(FASLEXT)
strap-2/ACFS.$(FASLEXT): strap-2/ACF.$(FASLEXT) strap-1/FS.$(FASLEXT)
strap-2/ACF.$(FASLEXT): strap-2/NTPOLFN.$(FASLEXT) \
	strap-2/PDECOMP.$(FASLEXT)
strap-2/NTPOLFN.$(FASLEXT): strap-2/PNTHEORY.$(FASLEXT)
strap-2/PNTHEORY.$(FASLEXT): strap-1/INTHEORY.$(FASLEXT)
strap-2/EXPR.$(FASLEXT): strap-1/COMBOPC.$(FASLEXT) \
	strap-1/LFCAT.$(FASLEXT) strap-1/SPFCAT.$(FASLEXT) \
	strap-1/KERNEL2.$(FASLEXT) strap-1/ALGMANIP.$(FASLEXT) \
	strap-1/AF.$(FASLEXT) strap-1/EF.$(FASLEXT) \
	strap-1/FSPECF.$(FASLEXT) strap-1/COMBF.$(FASLEXT) \
	strap-1/SEGBIND.$(FASLEXT) strap-1/POLYCATQ.$(FASLEXT) \
	strap-1/SUPFRACF.$(FASLEXT) strap-1/AN.$(FASLEXT) \
	strap-1/EXPR2.$(FASLEXT) strap-1/ES2.$(FASLEXT) \
	strap-1/MKUCFUNC.$(FASLEXT) strap-2/PMFS.$(FASLEXT) \
	strap-1/LIST2MAP.$(FASLEXT) strap-1/FAGROUP.$(FASLEXT) \
	strap-1/ACFS.$(FASLEXT) strap-1/LF.$(FASLEXT)
strap-2/EXPR2.$(FASLEXT): strap-2/EXPR.$(FASLEXT) strap-1/FS2.$(FASLEXT)
strap-2/PMFS.$(FASLEXT): strap-2/PMKERNEL.$(FASLEXT)
strap-2/INFSP.$(FASLEXT): strap-1/POLY2.$(FASLEXT) \
	strap-1/CMPLXRT.$(FASLEXT) strap-1/REAL0.$(FASLEXT) \
	strap-1/SUBRESP.$(FASLEXT) strap-1/POLTOPOL.$(FASLEXT) \
	strap-1/DMP.$(FASLEXT) strap-1/DIRPROD.$(FASLEXT) \
	strap-1/GROEBSOL.$(FASLEXT)

strap-2/GENMFACT.$(FASLEXT): strap-1/MPCPF.$(FASLEXT)
strap-2/TOOLSIGN.$(FASLEXT): strap-1/SIGNEF.$(FASLEXT)
strap-2/SIGNEF.$(FASLEXT): strap-1/SIGNRF.$(FASLEXT) \
	strap-1/LIMITPS.$(FASLEXT) strap-1/TRIGMNIP.$(FASLEXT) \
	strap-1/ORDCOMP.$(FASLEXT) strap-2/INTRVL.$(FASLEXT) \
	strap-1/TOOLSIGN.$(FASLEXT)
strap-2/INTRVL.$(FASLEXT): strap-2/INTCAT.$(FASLEXT)
strap-2/UPXS.$(FASLEXT): strap-2/UPXSCCA.$(FASLEXT) \
	strap-1/ULS.$(FASLEXT) strap-1/ULSCCAT.$(FASLEXT) \
	strap-1/VARIABLE.$(FASLEXT) strap-1/UTS.$(FASLEXT) \
	strap-1/UPXSCONS.$(FASLEXT)
strap-2/UPXSCCA.$(FASLEXT): strap-2/UPXSCAT.$(FASLEXT)
strap-2/UPXSCAT.$(FASLEXT): strap-2/UPSCAT.$(FASLEXT) \
	strap-2/ULSCAT.$(FASLEXT)
strap-2/UPSCAT.$(FASLEXT): strap-2/PSCAT.$(FASLEXT)
strap-2/UTS.$(FASLEXT): strap-2/UTSCAT.$(FASLEXT) \
	strap-1/VARIABLE.$(FASLEXT) strap-1/ITAYLOR.$(FASLEXT)
strap-2/UTSCAT.$(FASLEXT): strap-1/STTAYLOR.$(FASLEXT) \
	strap-1/STTF.$(FASLEXT) strap-1/STTFNC.$(FASLEXT) \
	strap-1/UP.$(FASLEXT) strap-1/FSINT.$(FASLEXT) \
	strap-1/UPSCAT.$(FASLEXT) strap-1/PSCAT.$(FASLEXT)
strap-2/APPRULE.$(FASLEXT): strap-1/RULE.$(FASLEXT)
strap-2/PLOT.$(FASLEXT): strap-1/PPCURVE.$(FASLEXT) \
	strap-1/PTPACK.$(FASLEXT) strap-1/POINT.$(FASLEXT)
strap-2/GRIMAGE.$(FASLEXT): strap-1/COLOR.$(FASLEXT) \
	strap-1/PALETTE.$(FASLEXT) \
	strap-1/VIEWDEF.$(FASLEXT) \
	strap-1/PLOTTOOL.$(FASLEXT) \
	strap-1/COORDSYS.$(FASLEXT) \
	strap-1/PTCAT.$(FASLEXT)
strap-2/NODE1.$(FASLEXT): strap-1/ODEINT.$(FASLEXT) \
	strap-1/ODEEF.$(FASLEXT)
strap-2/SPADAST.$(FASLEXT): strap-2/SASTCAT.$(FASLEXT) \
	strap-2/SPADXPT.$(FASLEXT) \
	strap-2/TYPEAST.$(FASLEXT) \
	strap-2/IMPTAST.$(FASLEXT) \
	strap-2/DEFAST.$(FASLEXT) \
	strap-2/MACROAST.$(FASLEXT) \
	strap-2/WHEREAST.$(FASLEXT) \
	strap-2/CATAST.$(FASLEXT) \
	strap-2/JOINAST.$(FASLEXT) \
	strap-2/CAPSLAST.$(FASLEXT) \
	strap-2/SIGAST.$(FASLEXT) \
	strap-2/ATTRAST.$(FASLEXT) \
	strap-2/MAPPAST.$(FASLEXT) \
	strap-2/IFAST.$(FASLEXT) \
	strap-2/RPTAST.$(FASLEXT) \
	strap-2/WHILEAST.$(FASLEXT) \
	strap-2/INAST.$(FASLEXT) \
	strap-2/STEPAST.$(FASLEXT) \
	strap-2/CLLCTAST.$(FASLEXT) \
	strap-2/LSTAST.$(FASLEXT) \
	strap-2/EXITAST.$(FASLEXT) \
	strap-2/RETAST.$(FASLEXT) \
	strap-2/CRCEAST.$(FASLEXT) \
	strap-2/PRTDAST.$(FASLEXT) \
	strap-2/RSTRCAST.$(FASLEXT) \
	strap-2/SEGAST.$(FASLEXT) \
	strap-2/SEQAST.$(FASLEXT) \
	strap-2/LETAST.$(FASLEXT) \
	strap-2/SUCHTAST.$(FASLEXT) \
	strap-2/COLONAST.$(FASLEXT) \
	strap-2/ADDAST.$(FASLEXT) \
	strap-2/CASEAST.$(FASLEXT) \
	strap-2/HASAST.$(FASLEXT) \
	strap-2/ISAST.$(FASLEXT) \
	strap-2/CATAST.$(FASLEXT) \
	strap-2/WHEREAST.$(FASLEXT) \
	strap-2/COMMAAST.$(FASLEXT) \
	strap-2/QQUTAST.$(FASLEXT) \
	strap-2/MACROAST.$(FASLEXT)
strap-2/DEFAST.$(FASLEXT): strap-1/HEADAST.$(FASLEXT) \
	strap-1/SIG.$(FASLEXT)
strap-1/SPADAST.$(FASLEXT): strap-0/SPADXPT.$(FASLEXT) \
	strap-0/SPADXPT.$(FASLEXT) \
	strap-0/TYPEAST.$(FASLEXT) \
	strap-0/IMPTAST.$(FASLEXT) \
	strap-0/DEFAST.$(FASLEXT) \
	strap-0/MACROAST.$(FASLEXT) \
	strap-0/WHEREAST.$(FASLEXT) \
	strap-0/CATAST.$(FASLEXT) \
	strap-0/JOINAST.$(FASLEXT) \
	strap-0/CAPSLAST.$(FASLEXT) \
	strap-0/SIGAST.$(FASLEXT) \
	strap-0/ATTRAST.$(FASLEXT) \
	strap-0/MAPPAST.$(FASLEXT) \
	strap-0/IFAST.$(FASLEXT) \
	strap-0/RPTAST.$(FASLEXT) \
	strap-0/WHILEAST.$(FASLEXT) \
	strap-0/INAST.$(FASLEXT) \
	strap-0/STEPAST.$(FASLEXT) \
	strap-0/CLLCTAST.$(FASLEXT) \
	strap-0/LSTAST.$(FASLEXT) \
	strap-0/EXITAST.$(FASLEXT) \
	strap-0/RETAST.$(FASLEXT) \
	strap-0/CRCEAST.$(FASLEXT) \
	strap-0/PRTDAST.$(FASLEXT) \
	strap-0/RSTRCAST.$(FASLEXT) \
	strap-0/SEGAST.$(FASLEXT) \
	strap-0/SEQAST.$(FASLEXT) \
	strap-0/LETAST.$(FASLEXT) \
	strap-0/SUCHTAST.$(FASLEXT) \
	strap-0/COLONAST.$(FASLEXT) \
	strap-0/ADDAST.$(FASLEXT) \
	strap-0/CASEAST.$(FASLEXT) \
	strap-0/HASAST.$(FASLEXT) \
	strap-0/ISAST.$(FASLEXT) \
	strap-0/CATAST.$(FASLEXT) \
	strap-0/WHEREAST.$(FASLEXT) \
	strap-0/COMMAAST.$(FASLEXT) \
	strap-0/QQUTAST.$(FASLEXT) \
	strap-0/MACROAST.$(FASLEXT)

strap-1/SPADXPT.$(FASLEXT): strap-0/SASTCAT.$(FASLEXT)
strap-2/SASTCAT.$(FASLEXT): strap-2/HOMOTOP.$(FASLEXT) \
	strap-1/ASTCAT.$(FASLEXT) strap-1/SYNTAX.$(FASLEXT)

strap-2/CATAST.$(FASLEXT): strap-1/CTORKIND.$(FASLEXT)
strap-2/TYPEAST.$(FASLEXT): strap-1/SPADAST.$(FASLEXT)
strap-2/IRFORM.$(FASLEXT): strap-1/ITFORM.$(FASLEXT)
strap-2/LINELT.$(FASLEXT): strap-1/LINBASIS.$(FASLEXT)
strap-2/FR2.$(FASLEXT): strap-1/COMPLEX.$(FASLEXT)


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
 $(OUTSRC)/void.spad $(OUTSRC)/parsers.spad \
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
	strap-2/FR.$(FASLEXT) strap-2/INTSLPE.$(FASLEXT) \
	strap-2/UPSQFREE.$(FASLEXT) strap-2/GALFACT.$(FASLEXT) \
	strap-2/FRUTIL.$(FASLEXT) strap-2/FR2.$(FASLEXT) \
	strap-2/HEUGCD.$(FASLEXT)

$(OUT)/BINOPC.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/IEVALAB.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	strap-2/LIST.$(FASLEXT)

$(OUT)/EVALAB.$(FASLEXT): $(OUT)/IEVALAB.$(FASLEXT) \
	strap-2/EQ.$(FASLEXT) strap-2/LIST.$(FASLEXT)

$(OUT)/ELTAB.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)

$(OUT)/SETCAT.$(FASLEXT): $(OUT)/BASTYPE.$(FASLEXT) \
	strap-2/OUTFORM.$(FASLEXT) \
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
$(OUT)/REAL.$(FASLEXT): $(OUT)/KONVERT.$(FASLEXT) \
	strap-2/FLOAT.$(FASLEXT)

$(OUT)/LOGIC.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/BOOLE.$(FASLEXT): $(OUT)/LOGIC.$(FASLEXT)

$(OUT)/SGROUP.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT) \
	strap-2/PI.$(FASLEXT) strap-2/REPSQ.$(FASLEXT)

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


$(OUT)/DIFFDOM.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/DIFFSPC.$(FASLEXT): $(OUT)/DIFFDOM.$(FASLEXT) \
	strap-2/PI.$(FASLEXT)
$(OUT)/DIFFMOD.$(FASLEXT): $(OUT)/DIFFSPC.$(FASLEXT)
$(OUT)/PDDOM.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT)
$(OUT)/PDSPC.$(FASLEXT): $(OUT)/PDDOM.$(FASLEXT)
$(OUT)/DSEXT.$(FASLEXT): $(OUT)/DIFFSPC.$(FASLEXT) $(OUT)/PDSPC.$(FASLEXT) \
	strap-2/SYMBOL.$(FASLEXT)
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
	strap-2/NNI.$(FASLEXT) strap-2/INTFACT.$(FASLEXT) \
	strap-2/PRIMES.$(FASLEXT) strap-2/COMBINAT.$(FASLEXT) \
	strap-2/PATRES.$(FASLEXT) strap-2/PMINS.$(FASLEXT)

$(OUT)/CHARZ.$(FASLEXT): $(OUT)/RING.$(FASLEXT)
$(OUT)/CHARNZ.$(FASLEXT): $(OUT)/RING.$(FASLEXT)

$(OUT)/MAYBE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT) $(OUT)/RETRACT.$(FASLEXT)

$(OUT)/SRING.$(FASLEXT): $(OUT)/ABELMON.$(FASLEXT) $(OUT)/MONOID.$(FASLEXT)
$(OUT)/RING.$(FASLEXT): $(OUT)/RNG.$(FASLEXT) $(OUT)/SRING.$(FASLEXT) \
	$(OUT)/LMODULE.$(FASLEXT) $(OUT)/KOERCE.$(FASLEXT)
$(OUT)/MONOID.$(FASLEXT): $(OUT)/SGROUP.$(FASLEXT)
$(OUT)/ABELGRP.$(FASLEXT): $(OUT)/CABMON.$(FASLEXT) $(OUT)/LLINSET.$(FASLEXT)

$(OUT)/CABMON.$(FASLEXT): $(OUT)/ABELMON.$(FASLEXT)

$(OUT)/ABELMON.$(FASLEXT): $(OUT)/ABELSG.$(FASLEXT) \
	$(OUT)/REPSQ.$(FASLEXT)

$(OUT)/FIELD.$(FASLEXT): $(OUT)/EUCDOM.$(FASLEXT) $(OUT)/UFD.$(FASLEXT) \
	$(OUT)/DIVRING.$(FASLEXT) strap-2/FR.$(FASLEXT)

$(OUT)/EUCDOM.$(FASLEXT): $(OUT)/PID.$(FASLEXT)
$(OUT)/PID.$(FASLEXT): $(OUT)/GCDDOM.$(FASLEXT)

$(OUT)/DIVRING.$(FASLEXT): $(OUT)/ENTIRER.$(FASLEXT) \
	$(OUT)/ALGEBRA.$(FASLEXT) $(OUT)/REPSQ.$(FASLEXT) \
	strap-2/PI.$(FASLEXT) strap-2/FRAC.$(FASLEXT)

$(OUT)/ORDRING.$(FASLEXT): $(OUT)/OAGROUP.$(FASLEXT) $(OUT)/CHARZ.$(FASLEXT) \
	$(OUT)/MONOID.$(FASLEXT)
$(OUT)/COMRING.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/BMODULE.$(FASLEXT)
$(OUT)/DIFRING.$(FASLEXT): $(OUT)/RING.$(FASLEXT)
$(OUT)/ENTIRER.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/BMODULE.$(FASLEXT)
$(OUT)/INTDOM.$(FASLEXT): $(OUT)/COMRING.$(FASLEXT) $(OUT)/ALGEBRA.$(FASLEXT) \
	$(OUT)/ENTIRER.$(FASLEXT) strap-2/FIELD.$(FASLEXT)
$(OUT)/OINTDOM.$(FASLEXT): $(OUT)/INTDOM.$(FASLEXT) $(OUT)/ORDRING.$(FASLEXT)
$(OUT)/GCDDOM.$(FASLEXT): $(OUT)/INTDOM.$(FASLEXT) \
	strap-2/LIST.$(FASLEXT)
$(OUT)/UFD.$(FASLEXT): $(OUT)/GCDDOM.$(FASLEXT) strap-2/ES.$(FASLEXT)
$(OUT)/ES.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/CACHSET.$(FASLEXT) \
	$(OUT)/PATAB.$(FASLEXT) strap-2/IDENT.$(FASLEXT) \
	$(OUT)/MAYBE.$(FASLEXT) strap-2/PRIMARR.$(FASLEXT) \
	strap-2/REF.$(FASLEXT) strap-2/ALIST.$(FASLEXT) \
	strap-2/NONE.$(FASLEXT) strap-2/KERNEL.$(FASLEXT) \
	strap-2/COMMONOP.$(FASLEXT) strap-2/SYMBOL.$(FASLEXT) \
	strap-2/BOP.$(FASLEXT) strap-2/EQ.$(FASLEXT) \
	strap-2/ARITY.$(FASLEXT) strap-2/BOP1.$(FASLEXT)
$(OUT)/CACHSET.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)
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

$(OUT)/GROUP.$(FASLEXT): $(OUT)/MONOID.$(FASLEXT) $(OUT)/REPSQ.$(FASLEXT) \
	strap-2/PI.$(FASLEXT) strap-2/INT.$(FASLEXT)

$(OUT)/ORDFIN.$(FASLEXT): $(OUT)/ORDSET.$(FASLEXT) $(OUT)/FINITE.$(FASLEXT)

$(OUT)/RADCAT.$(FASLEXT): $(OUT)/TYPE.$(FASLEXT) \
	strap-2/FRAC.$(FASLEXT) strap-2/INT.$(FASLEXT)

$(OUT)/OSGROUP.$(FASLEXT): $(OUT)/ORDSET.$(FASLEXT) $(OUT)/SGROUP.$(FASLEXT)

$(OUT)/LINEXP.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/LMODULE.$(FASLEXT)

$(OUT)/RNS.$(FASLEXT): $(OUT)/FIELD.$(FASLEXT) $(OUT)/ORDRING.$(FASLEXT) \
	$(OUT)/RETRACT.$(FASLEXT) $(OUT)/RADCAT.$(FASLEXT) \
	$(OUT)/KONVERT.$(FASLEXT) $(OUT)/PATMAB.$(FASLEXT) \
	$(OUT)/CHARZ.$(FASLEXT)

$(OUT)/FPS.$(FASLEXT): $(OUT)/RNS.$(FASLEXT)

$(OUT)/POLYCAT.$(FASLEXT): $(OUT)/PDRING.$(FASLEXT) \
	$(OUT)/FAMR.$(FASLEXT) $(OUT)/EVALAB.$(FASLEXT) \
	$(OUT)/RETRACT.$(FASLEXT) $(OUT)/FLINEXP.$(FASLEXT) \
	strap-2/VECTOR.$(FASLEXT) strap-2/MATRIX.$(FASLEXT) \
	strap-2/LIST.$(FASLEXT) strap-2/SUP.$(FASLEXT) \
	strap-2/INFORM.$(FASLEXT) strap-2/GENPGCD.$(FASLEXT)  \
	strap-2/PFBR.$(FASLEXT) strap-2/MULTSQFR.$(FASLEXT) \
	strap-2/PSQFR.$(FASLEXT) strap-2/PMPLCAT.$(FASLEXT) \
	strap-2/POLYLIFT.$(FASLEXT) strap-2/UNISEG.$(FASLEXT)

$(OUT)/QFCAT.$(FASLEXT): $(OUT)/ALGEBRA.$(FASLEXT) \
	$(OUT)/RETRACT.$(FASLEXT) \
	$(OUT)/FEVALAB.$(FASLEXT) $(OUT)/DIFEXT.$(FASLEXT) \
	$(OUT)/FLINEXP.$(FASLEXT) $(OUT)/PATAB.$(FASLEXT) \
	$(OUT)/FPATMAB.$(FASLEXT) $(OUT)/ORDSET.$(FASLEXT) \
	$(OUT)/OINTDOM.$(FASLEXT) $(OUT)/REAL.$(FASLEXT) \
	$(OUT)/KONVERT.$(FASLEXT) $(OUT)/CHARZ.$(FASLEXT) \
	$(OUT)/CHARNZ.$(FASLEXT) $(OUT)/EUCDOM.$(FASLEXT) \
	$(OUT)/PFECAT.$(FASLEXT) $(OUT)/FIELD.$(FASLEXT) \
	strap-2/MCDEN.$(FASLEXT) strap-2/PMQFCAT.$(FASLEXT)


$(OUT)/PDRING.$(FASLEXT): $(OUT)/PDSPC.$(FASLEXT)
$(OUT)/DIFEXT.$(FASLEXT): $(OUT)/DSEXT.$(FASLEXT)
$(OUT)/PFECAT.$(FASLEXT): $(OUT)/UFD.$(FASLEXT) $(OUT)/CHARZ.$(FASLEXT) \
	strap-2/LPEFRAC.$(FASLEXT)

$(OUT)/FAMR.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/OAMON.$(FASLEXT) \
	$(OUT)/AMR.$(FASLEXT) $(OUT)/FRETRCT.$(FASLEXT)

$(OUT)/AMR.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/OAMON.$(FASLEXT) \
	$(OUT)/BMODULE.$(FASLEXT) $(OUT)/FUNCTOR.$(FASLEXT) \
	$(OUT)/FIELD.$(FASLEXT) $(OUT)/COMRING.$(FASLEXT) \
	$(OUT)/ALGEBRA.$(FASLEXT) $(OUT)/CHARZ.$(FASLEXT) \
	$(OUT)/CHARNZ.$(FASLEXT) $(OUT)/INTDOM.$(FASLEXT)

$(OUT)/FRETRCT.$(FASLEXT): $(OUT)/RETRACT.$(FASLEXT) \
	strap-2/INT.$(FASLEXT) strap-2/FRAC.$(FASLEXT)

$(OUT)/FLINEXP.$(FASLEXT): $(OUT)/LINEXP.$(FASLEXT) \
	strap-2/MATRIX.$(FASLEXT)

$(OUT)/FPATMAB.$(FASLEXT): $(OUT)/PATMAB.$(FASLEXT)

$(OUT)/DIFFSPC.$(FASLEXT): $(OUT)/DIFFDOM.$(FASLEXT) \
	strap-2/NNI.$(FASLEXT)

$(OUT)/PDSPC.$(FASLEXT): $(OUT)/PDDOM.$(FASLEXT) \
	strap-2/NNI.$(FASLEXT)

$(OUT)/DFLOAT.$(FASLEXT): $(OUT)/FPS.$(FASLEXT) \
	$(OUT)/DIFRING.$(FASLEXT) $(OUT)/TRANFUN.$(FASLEXT) \
	$(OUT)/KONVERT.$(FASLEXT) \
	strap-2/DFSFUN.$(FASLEXT)

$(OUT)/TRANFUN.$(FASLEXT): $(OUT)/RING.$(FASLEXT) $(OUT)/FIELD.$(FASLEXT) \
	strap-2/TRIGCAT.$(FASLEXT) \
	strap-2/ATRIG.$(FASLEXT) strap-2/HYPCAT.$(FASLEXT) \
	strap-2/AHYP.$(FASLEXT) strap-2/ELEMFUN.$(FASLEXT)

$(OUT)/SYMBOL.$(FASLEXT): $(OUT)/ORDSET.$(FASLEXT) \
	$(OUT)/KONVERT.$(FASLEXT) $(OUT)/KRCFROM.$(FASLEXT) \
	$(OUT)/RETRACT.$(FASLEXT) $(OUT)/KONVERT.$(FASLEXT) \
	$(OUT)/PATMAB.$(FASLEXT) strap-2/VOID.$(FASLEXT) \
	strap-2/BOOLEAN.$(FASLEXT) strap-2/ALIST.$(FASLEXT) \
	strap-2/PRIMARR.$(FASLEXT) strap-2/REF.$(FASLEXT) \
	strap-2/STRING.$(FASLEXT) strap-2/PMSYM.$(FASLEXT)

$(OUT)/COMOPC.$(FASLEXT): $(OUT)/BINOPC.$(FASLEXT)

$(OUT)/COMOP.$(FASLEXT): $(OUT)/COMOPC.$(FASLEXT) $(OUT)/KOERCE.$(FASLEXT)

oa_algebra_layer_0 = \
  TYPE     BINOPC   BINOP    IDEMOPC  SGPOPC   SGPOP    \
  MONOPC   FUNCTOR  COMOPC   COMOP \
  BASTYPE  BASTYPE- SETCAT   SETCAT-  SGROUP   SGROUP-  \
  LLINSET  RLINSET  LINSET   ABELSG   ABELSG-  CHARNZ   \
  ABELGRP  ABELGRP- ABELMON  ABELMON- ORDTYPE  ORDTYPE- \
  RMODULE  ALGEBRA  ALGEBRA- FRETRCT  FRETRCT-    \
  FINITE   FINITE-  MONOID   MONOID-  GROUP    GROUP-   \
  RING     RING-    OINTDOM  AMR      AMR-      \
  BMODULE  STEP     LMODULE  PFECAT   PFECAT-           \
  AHYP     CFCAT    ELTAB    KOERCE   KONVERT  \
  KRCFROM  KVTFROM  IEVALAB  IEVALAB- EVALAB   EVALAB-  \
  RETRACT  RETRACT- REPSQ    REPDB    FAMR     FAMR-    \
  PRIMCAT  PTRANFN  SPFCAT   UTYPE    HOMOTOP  DIFEXT \
  ORDSET   OASGP    DIFRING  SRING    OSGROUP  PDRING   \
  MODULE   MODULE-  PID      OAGROUP  OAGROUP- OCAMON   \
  OAMON    OAMON-   DIOID    INTDOM   INTDOM-  CACHSET  \
  RNG      RNG-     ORDFIN   OAMONS   CABMON   COMRING  \
  GCDDOM   GCDDOM-  UFD      UFD-     ES        ES-     \
  FIELD    FIELD-   VECTCAT  VECTCAT- RADCAT   RADCAT-  \
  ENTIRER  ENTIRER- ORDRING  FLINEXP  FLINEXP- \
  DIFFDOM  DIFFDOM- DIFFSPC  DIFFSPC- DIFFMOD  \
  LINEXP   PATMAB   REAL     CHARZ    LOGIC    LOGIC-   \
  PDDOM    PDDOM-   PDSPC    PDSPC-   FPATMAB  \
  DSEXT    DSEXT-   ORDSTRCT          \
  BOOLE    BOOLE-   SRING    MAYBE    TRANFUN  TRANFUN- \
  INS      INS-     DIVRING  DIVRING- EUCDOM   EUCDOM-  \
  FPS      FPS-     RNS      RNS-     PATAB    \
  POLYCAT  POLYCAT- QFCAT    QFCAT-   FEVALAB  FEVALAB-  \
  INT      NNI      PI       SINT     SYMBOL   DFLOAT

oa_algebra_layer_0_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_0))

oa_algebra_layer_0_objects = \
	$(addprefix $(OUT)/, \
           $(addsuffix .$(FASLEXT),$(oa_algebra_layer_0))) 

# FIXME:
$(OUT)/ES.$(FASLEXT): strap-2/INFORM.$(FASLEXT)

$(OUT)/VOID.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT)
$(OUT)/HOMOTOP.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT) $(OUT)/KRCFROM.$(FASLEXT)
$(OUT)/ITUPLE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT) $(OUT)/STREAM.$(FASLEXT)
$(OUT)/PPCURVE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT)
$(OUT)/PSCURVE.$(FASLEXT): $(OUT)/KOERCE.$(FASLEXT)

$(OUT)/PROPLOG.$(FASLEXT): $(OUT)/BOOLE.$(FASLEXT) $(OUT)/SETCAT.$(FASLEXT)

$(OUT)/BYTEORD.$(FASLEXT): $(OUT)/SETCAT.$(FASLEXT)

$(OUT)/PROPERTY.$(FASLEXT): $(OUT)/IDENT.$(FASLEXT) $(OUT)/PAIR.$(FASLEXT)
$(OUT)/BINDING.$(FASLEXT): $(OUT)/PROPERTY.$(FASLEXT)
$(OUT)/OPERCAT.$(FASLEXT): $(OUT)/ARITY.$(FASLEXT)
$(OUT)/STREAM.$(FASLEXT): $(OUT)/LZSTAGG.$(FASLEXT) \
	strap-2/STREAM2.$(FASLEXT) strap-2/STREAM3.$(FASLEXT)
$(OUT)/LZSTAGG.$(FASLEXT): $(OUT)/STAGG.$(FASLEXT) \
	strap-2/CSTTOOLS.$(FASLEXT)


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
  ITUPLE    IDPT    ITFUN2   SEGCAT   VOID      EXIT     \
  FILECAT  SMAGG    MKRECORD MKFUNC   \
  PPCURVE  PSCURVE  RESLATC            \
  IDENT    OUTFORM  BINDING           BOOLEAN            \
  IARRAY1  \
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
  ITFUN3   STREAM1  STREAM2  STREAM3  ANY1     \
  ALIST   LIST      RTVALUE  SYSPTR   ATTREG   REF       

oa_algebra_layer_1_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_1))

oa_algebra_layer_1_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_1)))

$(OUT)/OUTFORM.$(FASLEXT): strap-2/NUMFMT.$(FASLEXT)
$(OUT)/FLAGG.$(FASLEXT): strap-2/FLASORT.$(FASLEXT)

oa_algebra_layer_2 = \
  SYNTAX   INTRET   SEGXCAT CONTOUR  LIST3   MKUCFUNC \
  FNCAT    SCACHE   IFARRAY  FARRAY  SET     SIG \
  FUNDESC  DOMTMPLT MKBCFUNC RNGBIND SEG     OVERSET \
  CTORCAT  CTORCAT- CTOR

oa_algebra_layer_2_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_2))

oa_algebra_layer_2_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_2)))
oa_algebra_layer_3 = \
   SCOPE    MAPHACK1 MAPHACK2 MAPHACK3 MAPPKG1   SEGBIND \
   MAPPKG2   MAPPKG3  INTBIT   MONAD    MONAD-   SEG2    \
   BOP	     BOP1    COMMONOP CATCTOR  CTORCALL

oa_algebra_layer_3_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_3))

oa_algebra_layer_3_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_3)))

oa_algebra_layer_4 = \
  ANON     OSI      COMM     COMPPROP   SEGBIND2 \
  FAMONC   IDPC              NONE       FCTRDATA \
  COLOR             \
  PALETTE  PARPCURV PARPC2   PARSCURV PARSC2   PARSURF  \
  PARSU2            PATRES2  PATTERN1 \
  SPACEC   SPLNODE  IDPOAM \
  SUCH     YSTREAM           ENV      \
  ATRIG    ATRIG-   LALG     LALG-	RANDSRC


oa_algebra_layer_4_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_4))

oa_algebra_layer_4_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_4)))

oa_algebra_layer_5 = \
  CARD     DVARCAT  DVARCAT- ELEMFUN  \
  ELEMFUN- FCOMP    IDPAM    IDPO     \
  INCRMAPS KERNEL2           MODMONOM MONADWU  MONADWU- \
  NARNG    NARNG-   ODVAR    PATLRES  PMLSAGG  \
           ORDMON   PERMCAT  RFDIST   RIDIST   \
  SDVAR    TRIGCAT  TRIGCAT- \
  ELABEXPR KERNEL    IDPOAMS

oa_algebra_layer_5_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_5))

oa_algebra_layer_5_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_5)))

oa_algebra_layer_6 = \
  PROPFRML PROPFUN1 AUTOMOR  CHARPOL  PATMATCH \
  OVAR     ES1      ES2     GRMOD    GRMOD-   \
  HYPCAT   HYPCAT-           MODRING NASRING  NASRING- \
  SORTPAK  ZMOD     PROPFUN2 SAOS    INDE   FLAGG2   \
  KTVLOGIC TREE     BYTE    SYSINT   SYSNNI DIRPCAT  DIRPCAT- 

$(OUT)/PATMATCH.$(FASLEXT): strap-2/POLY.$(FASLEXT)


oa_algebra_layer_6_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_6))
oa_algebra_layer_6_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_6)))

oa_algebra_layer_7 = \
  BTCAT   BTCAT-  LMOPS    FMONCAT  FMCAT   DIRPROD   \
  IFAMON  GRALG   GRALG-   SMP      INTABL  HASHTBL  \
  INT8    INT16   INT32    INT64    UINT8   UINT16   \
  UINT32  UINT64  BTREE    FAGROUP  FGROUP  MODOP   \
  FMONOID GDMP    PARTPERM HDP      RMATCAT  RMATCAT-  \
  ICDEN    

oa_algebra_layer_7_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_7))

oa_algebra_layer_7_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_7)))

$(OUT)/SMP.$(FASLEXT): strap-2/PGCD.$(FASLEXT)


oa_algebra_layer_8 = \
  BSTREE   BTOURN   FACTFUNC TABLE    ONECOMP  \
  FR2      FRUTIL   MLO      NAALG    ORDCOMP  LO       \
  NAALG-   OP       UNISEG2  XALG     FST      RADIX \
  POLY     OFMONOID ARR2CAT  ARR2CAT- LINDEP   STACK    \
  PADICCT  MOEBIUS  PRTITION HDMP     MPC2     MPC3     DMP \
  GBINTERN VARIABLE SMATCAT  SMATCAT- IMATLIN  IMATQF   MODMON   \
  FINRALG  FINRALG- CVMP    LIST2    FINAALG  FINAALG-  

oa_algebra_layer_8_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_8))

oa_algebra_layer_8_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_8)))

$(OUT)/LINDEP.$(FASLEXT): strap-2/LSMP.$(FASLEXT) \
	strap-2/MATCAT2.$(FASLEXT) strap-2/VECTOR2.$(FASLEXT)

$(OUT)/POLY.$(FASLEXT): strap-2/INDE.$(FASLEXT)
# FIXME:
$(OUT)/FR2.$(FASLEXT): strap-2/COMPLEX.$(FASLEXT)

oa_algebra_layer_9 = \
  DLP      EAB      IPADIC   MAGMA    QUEUE    MATLIN   \
  FAMONOID CONTFRAC WP       PERM     PERMGRP  DDFACT   \
  FSERIES  FT       IDPAG    INFINITY LA       \
  OMLO     ORTHPOL  PRODUCT  POLTOPOL SQMATRIX \
  GB       RATRET   RADUTIL  UP       PINTERPA  \
  XFALG    ZLINDEP  BBTREE   TABLEAU  MATSTOR  \
  FRNAALG  FRNAALG- FRAMALG  FRAMALG- CPIMA    

# FIXME: Investigate circularity
$(OUT)/CPIMA.$(FASLEXT): strap-2/MONOGEN.$(FASLEXT) \
	strap-2/UPOLYC2.$(FASLEXT)

oa_algebra_layer_9_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_9))

oa_algebra_layer_9_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_9)))


oa_algebra_layer_10 = \
  BPADIC   ANY      LWORD    FR       \
  SEXOF    CRAPACK  DEQUEUE  DLIST    \
  FLASORT  PATRES \
  FM       FM1      FPC      FPC-     \
  IROOT    LIECAT   LIECAT-  LIST2MAP \
  SEX      MODFIELD \
  MRING    MTHING   NCNTFRAC NCODIV   \
  ODR      OREPCAT  OREPCAT- OWP      \
  PADIC    PATTERN2 \
  PBWLB    PENDTREE PGE      PGROEB   \
  PINTERP  PFR      PMDOWN   \
  PMINS    PMTOOLS  MONOGEN MONOGEN- EMR     \
  PSCAT    PSCAT-   QFORM    MTSCAT   GHENSEL  \
  STTAYLOR TABLBUMP UPSCAT   UPSCAT-  \
  UDPO     UNISEG   VSPACE  OREPCTO   GENEEZ   \
  VSPACE-  XPOLYC   XPR      BTAGG    BTAGG-  \
  RMATRIX  PTCAT    XDPOLY   XRPOLY  CINTSLPE  \
  MATRIX  MATCAT   MATCAT-  IARRAY2  FFIELDC  FFIELDC-

# FIXME: Investigate circularity
$(OUT)/CINTSLPE.$(FASLEXT): strap-2/COMPCAT.$(FASLEXT)
$(OUT)/GHENSEL.$(FASLEXT): strap-2/GENEEZ.$(FASLEXT)

oa_algebra_layer_10_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_10))

oa_algebra_layer_10_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_10)))

oa_algebra_layer_11 = \
  APPLYORE ARRAY1   ARRAY12  ARRAY2   ONECOMP2          \
  ASTACK   COMBINAT POINT    UDVO     \
  CSTTOOLS MRF2     ITAYLOR   ORDCOMP2  \
  FLALG    GALUTIL  HEAP    COMPLPAT  CPMATCH   \
  INTCAT   INTHEORY COMPCAT  COMPCAT- \
  IRREDFFX LFCAT    LODOCAT  LODOCAT- \
  ORESUP   OREUP    QEQUAT   \
  PR       PREASSOC PRIMARR2 REDORDER \
  SYMPOLY  TUPLE    XEXPPKG  HB       IBITS \
  XF       XF-               XPOLY    INFORM   INFORM1  

# FIXME: Investigate circularity
$(OUT)/COMPCAT.$(FASLEXT): strap-2/COMPLEX.$(FASLEXT)


oa_algebra_layer_11_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_11))

oa_algebra_layer_11_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_11)))
oa_algebra_layer_12 = \
  ULSCAT   TUBE     BITS     DIRPROD2 UPXSCAT  SETMN    POLYROOT \
  STTF     LPOLY    LSMP     LSMP1    MATCAT2  TRIMAT   POLYCATQ \
  STTFNC   SYSTEM   HOSTNAME PORTNUM  UPOLYC2  PFBRU    SGCF   \
  OUT      PSEUDLIN BYTEBUF  COMPLEX  FS       FS-      ALGMANIP \
  AF       EF       FSPECF   COMBF    LF      RATFACT   SUPFRACF         \
  COMMUPC  IAN      AN       IALGFACT SAE     SAEFACT  ALGFACT  \
  RFFACT  SAERFFC  

## FIXME:
$(OUT)/POLYCATQ.$(FASLEXT): strap-2/SUP2.$(FASLEXT)
$(OUT)/IAN.$(FASLEXT): strap-2/EXPR.$(FASLEXT) strap-2/AN.$(FASLEXT) \
	strap-2/EXPR2.$(FASLEXT)
$(OUT)/AF.$(FASLEXT): strap-2/ACF.$(FASLEXT)
$(OUT)/SUPFRACF.$(FASLEXT): strap-2/MULTFACT.$(FASLEXT)


oa_algebra_layer_12_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_12))

oa_algebra_layer_12_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_12)))

oa_algebra_layer_13 = \
  ASSOCEQ  CARTEN   UTSCAT  UTSCAT-   PNTHEORY NTPOLFN  \
  UPOLYC   UPOLYC-  MRATFAC  NPCOEF   MLIFT    PDECOMP \
  COORDSYS DBASE    DHMATRIX DIOSP    FACUTIL  EXPR     \
  ACF      ACF-     FAXF     FAXF-    LEADCDET COMPFACT  \
  FNLA     GRAY     IRSN     INNMFACT ACFS    ACFS-   \
  MHROWRED NUMODE   NUMQUAD  GENUFACT MULTFACT   \
  ODESYS   ODETOOLS ORDFUNS  PERMAN   \
  UPXSCCA  UPXSCCA- ULSCCAT  ULSCCAT- \
  PTPACK   REP2     MSET              \
  SYMFUNC  VECTOR2  VECTOR   CHAR     XPBWPOLY \
  INBCON   INBCON-  OUTBCON  OUTBCON- LEXP    

# FIXME: Investigate cyle.
$(OUT)/MRATFAC.$(FASLEXT): strap-2/MULTFACT.$(FASLEXT)
$(OUT)/MULTFACT.$(FASLEXT): strap-2/MPRFF.$(FASLEXT)
$(OUT)/EXPR.$(FASLEXT): strap-2/ACFS.$(FASLEXT)

oa_algebra_layer_13_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_13))

oa_algebra_layer_13_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_13)))

oa_algebra_layer_14 = \
  PLOT3D   CLIF     GALFACTU CDEN     UPCDEN   ICARD    BALFACT  \
  BEZOUT   BINARY   BRILL    CHVAR    OPQUERY  CYCLOTOM FFP      \
  IPF      PF       CYCLES   DECIMAL  DISPLAY  FFPOLY   FFX      \
  NORMRETR TWOFACT  MFINFACT DPMO     DPOLCAT  DPOLCAT- EQ       \
  ERROR    LGROBP   PUSHVAR  MPCPF    GENMFACT   \
  EVALCYC  FF       FFF      FFCGP    FFCG     GROEBSOL \
  FFCGX    FFPOLY2  FFHOM    INBFF    FFNBP   FFNB     FFNBX    \
  FFSLPE   FGLMICPK REAL0    PRS      SUBRESP  \
  FNAME    FILE     POLY2    CMPLXRT  INFSP    FLOATRP  FRAC     \
  GENPGCD  GALPOLYU GBEUCLID GBF      GMODPOL  GOSPER   \
  HEXADEC  MDDFACT  INMODGCD HEUGCD   IBPTOOLS IFF      IDEAL     \
  IDECOMP  INPSIGN  MONOTOOL INTHERTR IR       LAUPOL   INTTR    \
  RDETR    INTRAT   IR2      INTRF    INTSLPE  TANEXP   INTTOOLS \
  EFSTRUC  FS2UPS   EFUPXS   BOUNDZRO ODEPRIM  \
  DSMP     LODOOPS   LODO    LODO1     LODO2   UTSODE   UTSODETL \
  ODERAT   INTG0    FFCAT    FFCAT-   IBATOOL  FFINTBAS RADFF   \
  FDIVCAT  FDIVCAT- FRIDEAL  HELLFDIV FRMOD    FDIV     DBLRESP  \
  PFOTOOLS FSRED    MMAP     ALGFF    FORDER   FFCAT2   FRIDEAL2 \
  FDIV2    RDIV     PFO      INTHERAL FSUPFACT INTALG   INTAF    \
  ODERED   ODEPAL   INTPAF   PRIMELT  FSPRMELT RDEEF    SMITH    \
  RDETRS   RDEEFS   PMASSFS  FS2      ITRIGMNP TRIGMNIP \
  PMPREDFS INTPM    INTEF    IR2F     RULE     APPRULE  TRMANIP  \
  FSCINT   FSINT    UTS      EFULS    ULSCONS  ULS      EXPUPXS  \
  UPXSSING EXPEXPAN FS2EXPXP LIMITPS  \
  ISUMP    SIGNRF   SIGNEF   TOOLSIGN LIMITRF  LPEFRAC  \
  LSPP     MCDEN    MPOLY    MPRFF    MULTSQFR \
  NSUP     ODP      ODEPRRIC PADICRC  PADICRAT \
  PCOMP    PFBR     PFRPAC   PGCD     PLEQN    \
  PMPLCAT  PMQFCAT  POLUTIL  POLYLIFT POLY2UP  PSQFR    \
  QALGSET  QFCAT2   RCFIELD  RCFIELD- REAL0Q   REALSOLV \
  RESRING  SYSSOLP  RETSOL   RF       RFFACTOR RRCC     RRCC-    \
  SCPKG    SHDP     SHP      SMTS     SOLVEFOR SPLTREE  STINPROD \
  SUMRF    UPMP     SUP      TEX      TEXTFILE UNIFACT  \
  UPDIVP   UPDECOMP UPSQFREE VIEWDEF  \
  TS       WEIER    EQTBL    GSTBL    STBL     STRTBL\
  DOMCTOR  DOMAIN   SYMTAB   SYMS     IOBCON   

# FIXME:
$(OUT)/CMPLXRT.$(FASLEXT): strap-2/INFSP.$(FASLEXT)
$(OUT)/MPCPF.$(FASLEXT): strap-2/GENMFACT.$(FASLEXT)
$(OUT)/SIGNEF.$(FASLEXT): strap-2/TOOLSIGN.$(FASLEXT)
$(OUT)/FS2UPS.$(FASLEXT): strap-2/SIGNEF.$(FASLEXT)
$(OUT)/LIMITPS.$(FASLEXT): strap-2/UPXS.$(FASLEXT) \
	strap-2/TOOLSIGN.$(FASLEXT)
$(OUT)/ODERAT.$(FASLEXT): strap-2/UTS.$(FASLEXT)
$(OUT)/RULE.$(FASLEXT): strap-2/APPRULE.$(FASLEXT)
$(OUT)/EXPUPXS.$(FASLEXT): strap-2/UPXS.$(FASLEXT)


oa_algebra_layer_14_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_14))

oa_algebra_layer_14_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_14)))


oa_algebra_layer_15 = \
  UPXSCONS UPXS     TEX1     SUP2     ODPOL    NSUP2    UP2  \
  SUBSPACE SPACE3   DROPT    DROPT1   DROPT0   GRDEF    CLIP \
  VIEW2D   VIEW     PLOTTOOL GRIMAGE  PLOT     RMCAT2   ROIRC \
  SDPOL    FRAC2    TUBETOOL JVMBCODE MESH    

#FIXME:
$(OUT)/GRDEF.$(FASLEXT): strap-2/PLOT.$(FASLEXT)
$(OUT)/VIEW.$(FASLEXT): strap-2/GRIMAGE.$(FASLEXT)
$(OUT)/VIEW2D.$(FASLEXT): strap-2/GRIMAGE.$(FASLEXT)

oa_algebra_layer_15_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_15))

oa_algebra_layer_15_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_15)))

oa_algebra_layer_16 = \
  DPMM     UTS2    ULS2    CARTEN2  COMPLEX2 LMDICT      \
  INTFACT DEGRED   PMPRED  UPXS2   NUMTUBE   \
  NFINTBAS PMASS   PADE    PADEPAC  MKFLCFN PLOT1  \
  PTFUNC2  VIEW3D  DRAWCX  DRAWPT   


oa_algebra_layer_16_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_16))

oa_algebra_layer_16_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_16)))

oa_algebra_layer_17 = \
  CCLASS  FSAGG2   GALFACT  TOPSP   BPADICRT  \
  IBACHIN MMLFORM  NORMMA   OMSAGG    OPSIG    \
  PRIMES  WFFINTBS PWFFINTB RDIST   RPOLCAT  RPOLCAT-


oa_algebra_layer_17_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_17))

oa_algebra_layer_17_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_17)))

oa_algebra_layer_18 = \
  KAFILE  IPRNTPK TBCMPPK PSETCAT  PSETCAT- 


oa_algebra_layer_18_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_18))

oa_algebra_layer_18_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_18)))

oa_algebra_layer_19 = \
  ACPLOT   ANTISYM  DRAWCFUN DRAW    \
  EP       FLOAT    FPARFRAC FUNCTION HACKPI  \
  ISUPS    LIB      INEP    \
  NREP     NUMFMT   OC       OC-      PATTERN  \
  PMKERNEL PMSYM    QALGSET2 RECLOS   REP1     QUATCAT  QUATCAT- \
  ROMAN    RULECOLD SPECOUT  SPADPRSR PARSER   TSETCAT  TSETCAT-

oa_algebra_layer_19_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_19))

oa_algebra_layer_19_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_19)))

oa_algebra_layer_20 = \
  ALGMFACT ALGPKG   ALGSC    CRFP     CTRIGMNP \
  DERHAM   DFSFUN   DRAWCURV ELFUTS   EXPRODE  \
  EXPRTUBE EXPR2    FLOATCP  FRNAAF2    \
  GAUSSFAC GCNAALG  GENUPS   GTSET    GPOLSET   \
  INFPROD0 INPRODFF INPRODPF JORDAN    \
  NLINSOL  ODERTRIC KOVACIC  LIE      LODOF    LSQM     \
  NCEP     NSMP     NUMERIC  QUAT     OCT      \
  OCTCT2   PAN2EXPR PFOQ     PICOERCE PMFS     \
  PSETPK   QUATCT2  RSETCAT  RSETCAT- RULESET  SIMPAN  \
  DRAWHACK SOLVESER SUMFS    SUTS     WUTSET   

oa_algebra_layer_20_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_20))

oa_algebra_layer_20_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_20)))

oa_algebra_layer_21 = \
  DFINTTLS DEFINTEF IRRF2F   DEFINTRF GSERIES EXPR2UPS  INVLAPLA \
  LAPLACE  ODEINT   ODECONST LODEEF   ODEEF   NODE1      \
  SOLVERAD REP      SULS     SUPXS   

# FIXME:
$(OUT)/ODEEF.$(FASLEXT): strap-2/NODE1.$(FASLEXT)

oa_algebra_layer_21_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_21))

oa_algebra_layer_21_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_21)))
oa_algebra_layer_22 = \
  NTSCAT   QCMPACK  RSETGCD  RSDCMPK  REGSET   RGCHAIN  \
  SFRTCAT  SNTSCAT  SOLVETRA SFQCMPK  SFRGCD   SRDCMPK  \
  SREGSET  NORMPK   LEXTRIPK IRURPK   ZDSOLVE

oa_algebra_layer_22_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_22))

oa_algebra_layer_22_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_22)))

oa_algebra_layer_23 = \
  LAZM3PK  RURPK    INTRVL     CATEGORY

oa_algebra_layer_23_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_23))

oa_algebra_layer_23_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_23)))

oa_algebra_layer_24 =  \
	RINTERP	 ASTCAT   ASTCAT-  SASTCAT  PARAMAST HEADAST  \
	LITERAL           TYPEAST  IMPTAST  MAPPAST  ATTRAST  \
	JOINAST  IFAST    RPTAST   WHILEAST INAST    CLLCTAST \
	LSTAST   EXITAST  RETAST   SEGAST   PRTDAST  CRCEAST   \
	LETAST   RDUCEAST COLONAST ADDAST   CAPSLAST SUCHTAST \
	CASEAST  HASAST   ISAST    CATAST   WHEREAST COMMAAST \
	QQUTAST  DEFAST   MACROAST SPADXPT  SPADAST  SIGAST \
	INBFILE  OUTBFILE IOBFILE  RGBCMDL  RGBCSPC  STEPAST  \
        IP4ADDR  NETCLT   INETCLTS ITFORM   RSTRCAST SEQAST   \
	IRFORM   ELABOR   COMPILER MSYSCMD  PRINT      \
	TALGOP   YDIAGRAM LINFORM  LINBASIS DBASIS LINELT     \
        JVMOP    JVMCFACC JVMFDACC JVMMDACC \
	JVMCSTTG

$(OUT)/TYPEAST.$(FASLEXT): strap-2/SPADAST.$(FASLEXT)
$(OUT)/ITFORM.$(FASLEXT): strap-2/IRFORM.$(FASLEXT)
$(OUT)/LINFORM.$(FASLEXT): strap-2/LINELT.$(FASLEXT) \
	strap-2/DBASIS.$(FASLEXT)



oa_algebra_layer_24_nrlibs = \
	$(addsuffix .NRLIB/code.$(FASLEXT),$(oa_algebra_layer_24))

oa_algebra_layer_24_objects = \
	$(addprefix $(OUT)/, \
	   $(addsuffix .$(FASLEXT),$(oa_algebra_layer_24)))





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
	$(COMPILE_SPAD) && cp $*.NRLIB/code.$(FASLEXT) $@ \
	  && if test -f $*-.NRLIB/code.$(FASLEXT); then \
	       cp $*-.NRLIB/code.$(FASLEXT) $(OUT)/$*-.$(FASLEXT); \
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

$(builddir)/%.dvi: $(oa_target_texdir)/diagrams.tex \
		   $(oa_target_texdir)/axiom.sty

$(builddir)/%.dvi: $(builddir)/%.tex
	$(oa_hammer) --latex $<

$(builddir)/%.tex: $(srcdir)/%.pamphlet
	$(oa_hammer) --weave --output=$@ $<

$(oa_target_texdir)/diagrams.tex: $(oa_src_docdir)/diagrams.tex
	@cp -p $< $@

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
# sinclude extract.mk
