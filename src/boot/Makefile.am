# Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
# All rights reserved.
# Copyright (C) 2007-2017, Gabriel Dos Reis.
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
#     - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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

#
# Abstract:
#   OpenAxiom is built in layers. The first layer is contructed into
#   an image called `bootsys'. The `bootsys' image is used
#   to translate Boot code to Common Lisp code.  Since a Boot coded
#   interpreter is needed to translate the code for the Boot coded
#   interpreter we have a ``boot-strapping'' problem.  In order to get
#   the whole process to start we need certain files kept in 
#   common lisp form. This directory contains those files.
#

# We start with a Lisp image created in a previous build step (src/lisp).
# That image is used to build Boot translator executable through the
# entire bootstrap process.

DRIVER = $(top_builddir)/src/driver/open-axiom$(EXEEXT)
OA_LISPSYS = $(oa_target_bindir)/lisp$(EXEEXT)

# Command used to link FASLs to produce a final executable.
LISP_LINK = \
	$(DRIVER) --execpath=$(OA_LISPSYS) --make \
		--main="|AxiomCore|::|topLevel|" --system=$(oa_targetdir) \
		--prologue='(pushnew :open-axiom-boot *features*)'

# We use a noinst_ primary because we take care of installation
# procedure ourselves.
noinst_PROGRAMS = bootsys bemol

bootsys_SOURCES = \
	utility.boot \
	tokens.boot \
	includer.boot \
	scanner.boot \
	pile.boot \
	ast.boot \
	parser.boot \
	translator.boot

bemol_SOURCES = \
	bemol.cc

bemol_LDADD = \
	$(oa_target_libdir)/libOpenAxiom.a \
	$(oa_target_libdir)/libopen-axiom-core.a

oa_target_bootdir = $(oa_targetdir)/boot
if OA_ECL_RT
oa_bootsys_linkset = $(oa_target_bootdir)/linkset
else
oa_bootsys_linkset = 
endif

bootsys_DEPENDENCIES = stage2/bootsys$(EXEEXT) $(oa_bootsys_linkset)

# FASLs that comprises `bootsys'
bootsys_clisp = $(bootsys_SOURCES:.boot=.clisp)
bootsys_objects = $(bootsys_SOURCES:.boot=.$(LNKEXT))

# Garbage produced by GCL during compilation
bootsys_data = $(bootsys_SOURCES:.boot=.data)
bootsys_fn = $(bootsys_SOURCES:.boot=.fn)

STAMP = touch

AM_CXXFLAGS = \
	-I$(top_srcdir)/src/include \
	-I$(oa_target_includedir) \
	-I$(top_builddir)/config \
	-DOPENAXIOM_ROOT_DIRECTORY="\"$(open_axiom_installdir)\""


# Make rule toplevel entry points.
.PHONY: all-boot

all-am: all-boot
all-boot: $(oa_target_bindir)/bootsys$(EXEEXT) $(oa_bootsys_linkset)
all-boot: bemol$(EXEEXT)

# The final `bootsys' image.
$(oa_target_bindir)/bootsys$(EXEEXT): stage2/bootsys$(EXEEXT)
	cp -p $< $@

$(oa_bootsys_linkset): $(addprefix $(oa_target_bootdir)/, $(bootsys_objects))
	rm -f $@
	echo '($(patsubst %, "%", $(bootsys_objects)))' > $@

$(oa_target_bootdir)/%.$(LNKEXT): stage2/%.$(LNKEXT) $(oa_target_bootdir)
	cp $< $@

$(oa_target_bootdir):
	$(mkdir_p) $(oa_target_bootdir)

.PRECIOUS: strap/%.$(FASLEXT)
.PRECIOUS: strap/%.$(LNKEXT)

#
# The bootstrapping `bootsys' image.
#

# The bootstrapping Lisp code is cached in the subdirectory strap/.
# It is built first, followed by stage 1, and stage 2 `bootsys' images.
strap_bootsys_clisp = $(addprefix strap/, $(bootsys_clisp))

strap_bootsys_objects = \
	$(addprefix strap/, $(bootsys_objects))

strap/stamp: strap/bootsys$(EXEEXT)
	@rm -f $@
	@$(STAMP) $@

strap/bootsys$(EXEEXT): $(strap_bootsys_objects)
	$(LISP_LINK) --output=$@ \
		--load-directory=strap $(strap_bootsys_objects)

$(strap_bootsys_objects): $(OA_LISPSYS)

#
# Stage 1 `bootsys' image.
#

.PRECIOUS: stage1/%.$(FASLEXT)
.PRECIOUS: stage1/%.$(LNKEXT)
.PRECIOUS: stage1/%.clisp

stage1_bootsys_clisp = $(addprefix stage1/, $(bootsys_clisp))

stage1_bootsys_objects = \
	$(addprefix stage1/, $(bootsys_objects))

stage1/stamp: stage1/bootsys$(EXEEXT)
	rm -f $@
	$(STAMP) $@

stage1/bootsys$(EXEEXT): $(stage1_bootsys_objects)
	$(LISP_LINK) --output=$@ \
		--load-directory=stage1 $(stage1_bootsys_objects)

stage1/%.clisp: %.boot strap/stamp stage1/.started
	$(DRIVER) --execpath=strap/bootsys$(EXEEXT) --translate --import=skip --output=$@ $<


#
# Stage 2 `bootsys' image.
#

.PRECIOUS: stage2/%.$(FASLEXT)
.PRECIOUS: stage2/%.$(LNKEXT)
.PRECIOUS: stage2/%.clisp

stage2_bootsys_clisp = $(addprefix stage2/, $(bootsys_clisp))

stage2_bootsys_objects = \
	$(addprefix stage2/, $(bootsys_objects))

stage2/stamp: stage2/bootsys$(EXEEXT)
	@echo Building stage 2
	$(STAMP) $@

stage2/bootsys$(EXEEXT): $(stage2_bootsys_objects)
	$(LISP_LINK) --output=$@ \
		--load-directory=stage2 $(stage2_bootsys_objects)

stage2/%.clisp: %.boot stage1/stamp stage2/.started
	$(DRIVER) --execpath=stage1/bootsys$(EXEEXT) --translate --import=skip --output=$@ $<

#
# Generic rules for compiling FASLs
#

.PRECIOUS: %/.started
%/.started:
	$(mkdir_p) $*
	$(STAMP) $@

# Dependency for various modules.  
# FIXME: This should be automatically extracted from the
# Boot source file at packaging time.
LISP_COMPILE = \
	$(DRIVER) --execpath=$(OA_LISPSYS) --output=$@ --compile

%/utility.$(LNKEXT): %/utility.clisp %/.started
	$(LISP_COMPILE) --load-directory=$* $<

%/tokens.$(LNKEXT): %/tokens.clisp %/utility.$(LNKEXT)
	$(LISP_COMPILE) --load-directory=$* $<

%/includer.$(LNKEXT): %/includer.clisp %/tokens.$(LNKEXT)
	$(LISP_COMPILE) --load-directory=$* $<

%/scanner.$(LNKEXT): %/scanner.clisp %/tokens.$(LNKEXT) %/includer.$(LNKEXT)
	$(LISP_COMPILE) --load-directory=$* $<

%/pile.$(LNKEXT): %/pile.clisp %/scanner.$(LNKEXT) %/includer.$(LNKEXT)
	$(LISP_COMPILE) --load-directory=$* $<

%/ast.$(LNKEXT): %/ast.clisp %/includer.$(LNKEXT)
	$(LISP_COMPILE) --load-directory=$* $<

%/parser.$(LNKEXT): %/parser.clisp %/ast.$(LNKEXT) %/scanner.$(LNKEXT) \
		 %/includer.$(LNKEXT)
	$(LISP_COMPILE) --load-directory=$* $<

%/translator.$(LNKEXT): %/translator.clisp %/parser.$(LNKEXT) \
		%/ast.$(LNKEXT) %/pile.$(LNKEXT) %/scanner.$(LNKEXT) \
		%/includer.$(LNKEXT)
	$(LISP_COMPILE) --load-directory=$* $<


#
# Cleaning mess
#

mostlyclean-local:
	@rm -f $(oa_target_bindir)/bootsys$(EXEEXT)
	@rm -rf prev-stage
	@rm -rf strap stage1 stage2
	@rm -rf $(oa_target_bootdir)
	@rm -f *.data *.fn

clean-local: mostlyclean-local
	@rm -f *.clisp

distclean-local: clean-local
