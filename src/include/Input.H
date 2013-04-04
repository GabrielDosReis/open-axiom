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

#ifndef OPENAXIOM_INPUT_included
#define OPENAXIOM_INPUT_included

#include <iterator>
#include <string>
#include <vector>
#include "open-axiom.h"
#include <open-axiom/structure>
#include <open-axiom/iterator>

namespace OpenAxiom {
   namespace Input {
      // -- Input text data
      using Text = std::string;

      // -- Type of Thingies that iterate over text values
      using TextIterator = Text::const_iterator;

      // -- Position of an input source line.
      using LineNumber = Ordinal;
      
      // -- We keep info only for non-empty input lines.
      struct NonEmptyLine : private structure::binary<LineNumber, Text> {
         NonEmptyLine(LineNumber, const Text&);
         LineNumber number() const { return first(); }
         const Text& text() const { return second(); }
         TextIterator begin() const { return text().begin(); }
         TextIterator end() const { return text().end(); }
         std::size_t size() const { return text().size(); }
      };

      struct Source;
      // -- Index into input source lines --
      using Index = std::size_t;

      // -- Line --
      struct Line : private structure::binary<const Source*, Index> {
         using value_type = Text::value_type;
         using const_reference = Text::const_reference;
         using const_pointer = Text::const_pointer;
         using difference_type = Text::difference_type;
         using iterator = iterator::basic<Line>;
         const NonEmptyLine& get() const;
         const Text& text() const { return get().text(); }
         LineNumber number() const { return get().number(); }
         iterator begin() const { return { this, 0 }; }
         iterator end() const { return { this, size() }; }
         Cardinal size() const { return text().size(); }
         const_reference at(Ordinal n) const { return text().at(n); }
      private:
         constexpr Line(const Source* s, Index i)
               : structure::binary<const Source*, Index>(s, i)
         { }
         const Source* source() const { return first(); }
         Index index() const { return second(); }
         
         friend Source;
      };

      // -- LineIterator --
      using LineIterator = Line::iterator;

      // -- Source --
      struct Source : std::vector<NonEmptyLine> {
         using iterator = OpenAxiom::iterator::basic<Source>;
         using Index = Ordinal; // line index
         Line line(Index i) const { return { this, i }; }
         iterator begin() const { return { this, 0 }; }
         iterator end() const { return { this, size() }; }
         Line line(LineNumber, const Text&);
      };

      // -- Span --
      struct Span : structure::binary<Ordinal, Cardinal> {
         constexpr Span(Ordinal c, Cardinal l)
               : structure::binary<Ordinal>(c, l) { }
         constexpr Ordinal column() const { return first(); }
         constexpr Cardinal size() const { return second(); }
      };
   }
}

#endif  // OPENAXIOM_INPUT_included
