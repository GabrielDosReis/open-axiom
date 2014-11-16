# Copyright (C) 2011-2014, Gabriel Dos Reis.
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

noinst_PROGRAMS = hammer$(EXEEXT)
hammer_SOURCES = hammer.cc
hammer_LDADD = -L. -lOpenAxiom

noinst_LIBRARIES = libOpenAxiom.a

libOpenAxiom_a_SOURCES = \
	storage.cc string-pool.cc command.cc \
	filesystem.cc \
	../io/Input.cc \
	../io/std-streams.cc

oa_public_headers = hash-table string-pool

## Where we store public header files
oa_target_headerdir = $(oa_target_includedir)/open-axiom

## The final directory where OpenAxiom is installed.  This is usually
## the directory deduced or specified at configuration time.
open_axiom_installdir = @open_axiom_installdir@

AM_CXXFLAGS = \
	-I. -I$(oa_target_includedir) \
	-I$(top_builddir)/config \
	-I$(top_srcdir)/src/include \
	-DOPENAXIOM_ROOT_DIRECTORY="\"$(open_axiom_installdir)\""

oa_target_oalib = $(oa_target_libdir)/libOpenAxiom.a

.PHONY: all-utils
SUFFIXES = .cc .cxx .H .hxx

all-utils: all-am 
all-am: $(oa_target_oalib)

$(oa_target_oalib): libOpenAxiom.a
	if [ ! -d $(oa_target_libdir) ]; then \
	   mkdir -p $(oa_target_libdir); \
	fi
	cp $< $@

mostlyclean-local:
	@rm -f $(oa_target_oalib)
