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

#ifndef OPENAXIOM_ITERATOR_included
#define OPENAXIOM_ITERATOR_included

#include <iterator>
#include <open-axiom/defaults>
#include "open-axiom.h"

namespace OpenAxiom {
   // -- Read-only iterator over a sequence with random access capabilities.
   namespace iterator {
      template<typename Seq>
      struct basic
         : std::iterator<std::random_access_iterator_tag,
                         const typename Seq::value_type>,
         defaults::neq<basic<Seq>>, defaults::ordering<basic<Seq>> {
         using reference = typename Seq::const_reference;
         using pointer = typename Seq::const_pointer;
         using difference_type = typename Seq::difference_type;

         reference operator*() const { return seq->at(idx); }
         pointer operator->() const { return &seq->at(idx); }
         reference operator[](Ordinal i) const { return seq->at(idx + i); }

         basic& operator++() { ++idx; return *this; }
         basic operator++(int) { auto t = *this; ++idx; return t; }
         basic& operator--() { --idx; return *this; }
         basic operator--(int) { auto t = *this; --idx; return t; }

         basic& operator+=(Cardinal n) { idx += n; return *this; }
         basic& operator-=(Cardinal n) { idx -= n; return *this; }
         basic operator+(Cardinal n) const { return { seq, idx + n }; }
         basic operator-(Cardinal n) const { return { seq, idx - n }; }
         difference_type operator-(basic that) const { return idx - that.idx; }
         

         bool operator==(basic that) const { return idx == that.idx; }
         bool operator<(basic that) const { return idx < that.idx; }
         
      private:
         friend Seq;
         const Seq* seq;
         Ordinal idx;
         constexpr basic(const Seq* s, Cardinal i) : seq(s), idx(i) { }
      };
   }
}


#endif  // OPENAXIOM_ITERATOR_included

