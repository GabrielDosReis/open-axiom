// -*- C++ -*-
// Copyright (C) 2013, Gabriel Dos Reis.
// All rights reserved.
// Written by Gabriel Dos Reis.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     - Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     - Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//
//     - Neither the name of OpenAxiom, nor the names of its contributors may
//       be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef OPENAXIOM_DEFAULTS_included
#define OPENAXIOM_DEFAULTS_included

namespace OpenAxiom {
   // -- The sole purpose of classes defines in this header is to
   // -- provoke the automatic generation of definitions for functions
   // -- defined within.  These functions are not really friend.  The
   // -- friendship is such a mechanism to convince the C++ compiler
   // -- to do something else.  This is standard technique, also
   // -- known as `based class parameterized by derived classes', or
   // -- `curiously recurring pattern'.
   namespace defaults {
      // generate definition for != assuming existence of ==.
      template<typename t>
      struct neq {
         friend bool operator!=(t x, t y) { return not(x == y); }
      };

      // generate definitions for ordering functions assuming <.
      template<typename t>
      struct ordering {
         friend bool operator<=(t x, t y) { return not(y < x); }
         friend bool operator>(t x, t y) { return y < x; }
         friend bool operator>=(t x, t y) { return not(x < y); }
      };
   }
}

#endif  // OPENAXIOM_DEFAULTS_included


