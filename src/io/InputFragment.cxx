// -*- C++ -*-
// Copyright (C) 2014-2017, Gabriel Dos Reis.
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


#include <open-axiom/InputFragment>
#include <algorithm>
#include <iterator>
#include <istream>
#include <ostream>
#include <iostream>
#include <open-axiom/SourceInput>

namespace OpenAxiom {
   // Formatting program fragments.
   std::ostream& operator<<(std::ostream& os, const Fragment& f) {
      std::copy(f.begin(), f.end(),
                std::ostream_iterator<std::string>(os, "\n"));
      return os;
   }
   
   // Return the indentation level of a line.
   // FIXME: reject or expand tabs as appropriate.
   static ColumnIndex indentation(const Line& line) {
      ColumnIndex idx { };
      for (auto c : line) {
         if (not isblank(c))
            break;
         ++idx;
      }
      return idx;
   }

   // Remove trailing white-space characters from the line.
   static Line& trim_right(Line& line) {
      auto n = line.length();
      while (n > 0 and isblank(line[n-1]))
         --n;
      line.resize(n);
      return line;
   }
   
   // Clean up and dress up the line with indentation information.
   static Line& prop_up(Line& line) {
      line.indent = indentation(trim_right(line));
      line.kind = LineKind::Ordinary;
      return line;
   }

   // Return true if line is entirely a positive comment, i.e. a description.
   static bool positive_comment(const Line& line) {
      if (line.indent + 1 >= line.length())
         return false;
      return line[line.indent] == '+' and line[line.indent + 1] == '+';
   }

   // Return true if line is entirely a negative comment.
   static bool negative_comment(const Line& line) {
      if (line.indent + 1 >= line.length())
         return false;
      return line[line.indent] == '-' and line[line.indent + 1] == '-';
   }

   // Return true if line is either empty or a negative comment.
   static bool blank(const Line& line) {
      return line.empty() or negative_comment(line);
   }

   // Decompose the input souce file into fragments, and return one
   // fragment at a time.
   Fragment SourceInput::get() {
      Fragment fragment;
      std::stack<ColumnIndex> indents;

      if (not line.empty() and line.kind == LineKind::Ordinary) {
         indents.push(line.indent);
         fragment.push_back(line);
      }

      while (std::getline(input, line)) {
         ++line.number;
         prop_up(line);
         if (blank(line))
            continue;              // Don't bother with ignorable comments.
         else if (fragment.line_continuation())
            ;
         else if (indents.empty()) {
            if (fragment.empty() and line.indent != 0)
               std::cerr << "warning: white space at begining of fragment"
                         << " on line " << line.number << '\n';
            indents.push(line.indent);
         }
         else if (line.indent == 0 and not positive_comment(fragment.back()))
            break;                 // A completely new line; save for later.
         else if (line.indent > indents.top())
            indents.push(line.indent);
         else {
            while (line.indent < indents.top())
               indents.pop();
         }
         fragment.push_back(line);
      }
      return fragment;
   }
}
