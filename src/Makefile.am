# Copyright (C) 2010, Gabriel Dos Reis.
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

DIST_SUBDIRS = \
	algebra boot clef doc \
	driver etc graph hyper \
	include input interp lib \
	lisp scripts share sman \
	testsuite utils

subdirs = \
	algebra boot clef doc driver \
	etc graph hyper input interp \
	lib lisp share sman utils

oa_targetdir = $(top_builddir)/$(target)

if OA_BUILD_SMAN
OA_SMAN_TARGETS = all-sman all-clef
endif

if OA_BUILD_GRAPHICS
OA_GRAPHICS_TARGETS = all-graph
endif


all-local: all-src

.PHONY: all-src
all-src: all-lib all-utils all-driver all-lisp all-boot \
	all-interpsys all-algebra all-axiomsys all-share \
	all-asq all-input all-doc $(OA_SMAN_TARGETS) $(OA_GRAPHICS_TARGETS)

stamp-subdirs:
	rm -f stamp-subdirs
	$(MKDIR_P) $(oa_targetdir)/bin
	$(MKDIR_P) $(oa_targetdir)/lib
	$(MKDIR_P) $(oa_targetdir)/share
	$(MKDIR_P) $(oa_targetdir)/doc
	$(MKDIR_P) $(oa_targetdir)/doc/help
	$(MKDIR_P) $(oa_targetdir)/autoload
	$(MKDIR_P) algebra/strap
	touch stamp-subdirs

.PHONY: $(oa_all_targets)

all-driver: all-lib
	cd driver && $(MAKE) $(AM_MAKEFLAGS) $@

all-utils: stamp-subdirs
	cd utils && $(MAKE) $(AM_MAKEFLAGS) $@

all-clef: all-lib
	cd clef && $(MAKE) $(AM_MAKEFLAGS) $@

all-sman: all-lib all-driver
	cd sman && $(MAKE) $(AM_MAKEFLAGS) $@

all-hyper: all-lib
	cd hyper && $(MAKE) $(AM_MAKEFLAGS) $@

all-share:
	cd share && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: all-doc
all-doc: stamp-subdirs
	cd doc && $(MAKE) $(AM_MAKEFLAGS) $@

all-lib: stamp-subdirs
	cd lib && $(MAKE) $(AM_MAKEFLAGS) $@

all-lisp: all-lib all-driver
	cd lisp && $(MAKE) $(AM_MAKEFLAGS) $@

all-boot: all-lisp
	cd boot && $(MAKE) $(AM_MAKEFLAGS) $@

all-interpsys: all-boot all-hyper-pre all-utils
	cd interp && ${MAKE} $(AM_MAKEFLAGS) all-interpsys

all-axiomsys: all-asq
	cd interp && $(MAKE) $(AM_MAKEFLAGS) all-axiomsys

all-algebra: all-interpsys all-share all-hyper-pre
	cd algebra && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: all-hyper-pre
all-hyper-pre: all-lib
	cd hyper && $(MAKE) $(AM_MAKEFLAGS) all-hyper-pre

all-input: all-axiomsys all-doc
	cd input && $(MAKE) $(AM_MAKEFLAGS) $@

all-asq: all-hyper-post all-utils
	cd etc && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: all-databases
all-databases:  all-interp all-doc
	cd etc && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: all-hyper-post
all-hyper-post: all-algebra  all-doc
	cd hyper && $(MAKE) $(AM_MAKEFLAGS) all-hyper-post

all-graph: all-lib
	cd graph && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: all-check
all-check:
	cd input && $(MAKE) $(AM_MAKEFLAGS) all-check

mostlyclean-local:
	for d in $(subdirs); do \
	  (cd $$d && $(MAKE) $(AM_MAKEFLAGS) mostlyclean); \
	done
	-rm -f stamp-subdirs

clean-local:
	for d in $(subdirs); do \
	  (cd $$d && $(MAKE) $(AM_MAKEFLAGS) clean); \
	done

distclean-local:
	for d in $(subdirs); do \
	  (cd $$d && $(MAKE) $(AM_MAKEFLAGS) distclean); \
	done