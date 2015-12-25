# Copyright (C) 2012-2015, Gabriel Dos Reis.
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

# The build system is set up  so that if `make all' completes successfully
# then `make install' will not modify any file in the build directory.
# Consequently it is possible to issue `make all' as an one user
# to build the whole system, and install it as another user.  The typical
# case is: on Unix-like system, build as ordinary user, then install as
# superuser.

# The build flow of OpenAxiom seems, at first sight, a bit intricate to
# describe.  This description is STILL IN WORK IN PROGRESS.  Therefore
# it is incorrekt, incomplet, and iNconSiStenT.

ACLOCAL_AMFLAGS = -I config #--install --output=config/aclocal.m4

DIST_SUBDIRS = src

oa_installdir = @open_axiom_installdir@

# Configure options to GCL.
GCLOPTS=@GCLOPTS@

OA_SRC_TARGETS = \
	all-local \
	all-src \
	all-algebra \
	all-algstrap \
	all-initdb \
	all-asq \
	all-axiomsys \
	all-boot \
	all-share \
	all-clef \
	all-doc \
	all-graph \
	all-gui \
	all-hyper \
	all-input \
	all-interpsys \
	all-lib \
	all-lisp \
	all-sman \
	all-driver \
	all-databases \
	all-utils \
	all-rt

.PHONY: $(OA_SRC_TARGETS)

.PHONY: all-layer-%
all-layer-%:
	cd src && $(MAKE) $(AM_MAKEFLAGS)  $@

if OA_BUILD_GCL
all-recursive all-boot all-lisp all-interpsys all-axiomsys \
  all-algebra all-algstrap all-input all-src all-local: all-gcl
endif

all: all-local
$(OA_SRC_TARGETS):
	cd src && $(MAKE) $(AM_MAKEFLAGS) $@

.PHONY: all-gcl
all-gcl: $(oa_target_bindir)/gcl$(EXEEXT)

$(oa_target_bindir)/gcl$(EXEEXT):
	if test "$(srcdir)" != "$(builddir)" -a ! -d gcl; then \
	   cp -r "$(srcdir)"/gcl "$(builddir)"; \
	fi; cd gcl; \
	./configure --prefix="$(abs_top_builddir)/$(target)" ${GCLOPTS} && \
		$(MAKE) $(AM_MAKEFLAGS) && $(MAKE) $(AM_MAKEFLAGS) install

install:
	@echo Installing OpenAxiom in $(DESTDIR)$(prefix)
	@$(mkinstalldirs) "$(DESTDIR)$(oa_installdir)"
	cp -pr "$(builddir)"/$(target)/* "$(DESTDIR)$(oa_installdir)"
	rm -f "$(DESTDIR)$(bindir)"/open-axiom
	@$(mkinstalldirs) "$(DESTDIR)$(bindir)"
	$(INSTALL_PROGRAM) src/driver/open-axiom$(EXEEXT) "$(DESTDIR)$(bindir)"
	@echo OpenAxiom installation finished.

.PHONY: check
check:
	cd src && $(MAKE) $(AM_MAKEFLAGS) all-check

mostlyclean-local:
	cd src && $(MAKE) $(AM_MAKEFLAGS) mostlyclean

clean-local:
	cd src && $(MAKE) $(AM_MAKEFLAGS) clean

distclean-local:
	cd src && $(MAKE) $(AM_MAKEFLAGS) distclean
