// -- -*- C++ -*-
// Copyright (C) 2014-2024, Gabriel Dos Reis.
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


#include <ostream>
#include <iostream>
#include <open-axiom/Charset>
#include <open-axiom/InputFragment>
#include <open-axiom/LispValue>
#include <open-axiom/token>
#include <open-axiom/SyntaxTree>
#include <open-axiom/Parser>
#include <open-axiom/BootAst>

namespace OpenAxiom {

   std::ostream& operator<<(std::ostream& os, FragmentCursor cur) {
      return os << '{' << cur.line << '-' << cur.column << '}';
   }
   
   // -- Formatting program fragments.
   std::ostream& operator<<(std::ostream& os, const Fragment& f) {
      for (auto& line : f)
         os << std::u8string_view(line) << '\n';
      return os;
   }
   
   // -- Return the indentation level of a line.
   static ColumnIndex indentation(const Line& line) {
      ColumnIndex idx { };
      for (auto c : line) {
         if (not ascii_blank(c))
            break;
         ++idx;
      }
      return idx;
   }

   // -- Expand tab characters to spaces using 8-column tab stops.
   // Boot source files use tabs for continuation indentation in
   // several places, so proper expansion is needed to make
   // indentation comparisons correct.
   static Line& expand_tabs(Line& line) {
      if (line.find(u8'\t') == std::u8string::npos)
         return line;
      std::u8string result;
      result.reserve(line.size());
      ColumnIndex col = 0;
      for (char8_t c : line) {
         if (c == u8'\t') {
            ColumnIndex next_stop = (col / 8 + 1) * 8;
            result.append(next_stop - col, u8' ');
            col = next_stop;
         }
         else {
            result += c;
            ++col;
         }
      }
      static_cast<std::u8string&>(line) = std::move(result);
      return line;
   }

   // -- Remove trailing white-space characters and carriage returns from the line.
   static Line& trim_right(Line& line) {
      auto n = line.length();
      while (n > 0 and (ascii_blank(line[n-1]) or line[n-1] == u8'\r'))
         --n;
      line.resize(n);
      return line;
   }

   // -- Return true if line is entirely a positive comment, i.e. a description.
   static bool positive_comment(const Line& line) {
      if (line.indent + 1 >= line.length())
         return false;
      return line[line.indent] == u8'+' and line[line.indent + 1] == u8'+';
   }

   // -- Return true if line is entirely a negative comment.
   static bool negative_comment(const Line& line) {
      if (line.indent + 1 >= line.length())
         return false;
      return line[line.indent] == u8'-' and line[line.indent + 1] == u8'-';
   }
   
   // -- Clean up and dress up the line with indentation information.
   static Line& prop_up(Line& line) {
      line.indent = indentation(expand_tabs(trim_right(line)));
      if (negative_comment(line))
         line.kind = LineKind::Ignorable;
      else if (positive_comment(line))
         line.kind = LineKind::Description;
      else if (line.indent == 0 and not line.empty() and line.front() == u8')')
         line.kind = LineKind::Meta;
      else
         line.kind = LineKind::Ordinary;
      return line;
   }

   // -- A source input transforms a flat character buffer into a program
   // -- fragment stream, delivering a fragment one at a time.
   struct SourceInput {
      explicit SourceInput(std::u8string_view buf)
         : buffer{ buf }, pos{ }, line{ } { }
      // -- Return the next program fragment from this input source.
      Fragment get();
      
   private:
      std::u8string_view buffer;
      std::size_t pos;
      Line line;

      // -- Extract the next physical line from the buffer.
      // Returns false at end of input.
      bool getline();
   };

   bool SourceInput::getline() {
      if (pos >= buffer.size())
      {
         static_cast<std::u8string&>(line).clear();
         return false;
      }
      auto nl = buffer.find(u8'\n', pos);
      if (nl == std::u8string_view::npos)
      {
         static_cast<std::u8string&>(line).assign(buffer.data() + pos,
                                                  buffer.size() - pos);
         pos = buffer.size();
      }
      else
      {
         static_cast<std::u8string&>(line).assign(buffer.data() + pos,
                                                  nl - pos);
         pos = nl + 1;
      }
      return true;
   }

   // -- Decompose the input source file into fragments, and return one
   // fragment at a time.
   Fragment SourceInput::get() {
      Fragment fragment;
      std::stack<ColumnIndex> indents;

      if (not line.empty()) {
         if (line.kind == LineKind::Ordinary)
            indents.push(line.indent);
         fragment.push_back(line);
      }

      while (getline()) {
         ++line.number;
         prop_up(line);
         if (line.indent >= line.length())
            continue;              // Don't bother with entirely blank links.
         else if (fragment.line_continuation())
            ;                   // Line splicing does not change indentation.
         else if (line.kind == LineKind::Ignorable)
            ;                   // Likewise for ignorable lines.
         else if (line.kind == LineKind::Meta)
            ;                   // Likewise for reader lines.
         else if (line.kind == LineKind::Description and line.indent == 0)
            ;                   // Toplevel descriptions preceed items.
         else if (indents.empty()) {
            // -- No code lines collected yet.  If the fragment already
            // has non-code lines (Meta, Description), an indent-0
            // ordinary line starts a *new* top-level -- break.
            // Only when the fragment is entirely empty do we allow
            // an indent-0 line to begin it (e.g. Spad files that
            // start with 'import' before ')abbrev').
            if (line.indent == 0) {
               if (not fragment.empty())
                  break;
            }
            else if (fragment.empty())
               std::cerr << "warning: white space at begining of fragment"
                         << " on line " << line.number << '\n';
            indents.push(line.indent);
         }
         else if (line.indent == 0)
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

   Prose read_source(std::u8string_view source)
   {
      Prose text { };
      SourceInput src { source };
      while (auto f = src.get())
         text.push_back(f);
      return text;
   }

   // -- Reader with directive evaluation
   //
   // Processes )if / )elseif / )else / )endif conditionals:
   //
   //   )if <boot-expr>
   //     ... included if expr is true ...
   //   )elseif <boot-expr>
   //     ... included if previous was false and expr is true ...
   //   )else
   //     ... included if all previous branches were false ...
   //   )endif
   //
   // The condition <boot-expr> is tokenized as Boot, parsed,
   // lowered to Lisp AST, and evaluated against the features list.
   //
   // )abbrev lines are silently skipped.
   // )eval lines are silently skipped (not yet implemented).

   namespace {

      // -- Parse a reader directive line.
      //    Returns the directive keyword and the rest of the line.
      struct Directive {
         std::u8string_view keyword;  // u8"if", u8"elseif", u8"else", u8"endif",
                                      // -- u8"eval", u8"abbrev", or u8""
         std::u8string_view rest;     // text after the directive keyword
      };

      Directive parse_directive(const Line& line)
      {
         Directive d { {}, {} };
         if (line.empty() or line.front() != u8')')
            return d;
         // -- Skip the leading ')'.
         std::size_t i = 1;
         // -- Extract the directive keyword.
         auto start = i;
         while (i < line.size() and line[i] != u8' ' and line[i] != u8'\t')
            ++i;
         d.keyword = std::u8string_view(line).substr(start, i - start);
         // -- Skip whitespace.
         while (i < line.size() and (line[i] == u8' ' or line[i] == u8'\t'))
            ++i;
         d.rest = std::u8string_view(line).substr(i);
         return d;
      }

      // -- Evaluate a Boot condition expression.
      //    Tokenizes, parses, lowers, then evaluates against features.
      bool eval_boot_condition(std::u8string_view expr,
                               Lisp::Value features)
      {
         if (expr.empty())
            return false;

         // -- Build a one-line fragment from the condition text.
         Fragment frag;
         Line cond_line;
         static_cast<std::u8string&>(cond_line) = std::u8string(expr);
         cond_line.number = 0;
         cond_line.indent = 0;
         cond_line.kind = LineKind::Ordinary;
         frag.push_back(cond_line);

         try {
            auto toks = pile(words(frag, Language::Boot));
            if (toks.empty())
               return false;

            Syntax::SyntaxForest forest;
            auto forms = Boot::parse_boot(toks, forest);
            if (forms.empty())
               return false;

            Lisp::Arena arena;
            Boot::LoweringContext ctx{ forest, toks, frag, arena };
            auto lowered = Boot::lower(forms[0], ctx);
            return Lisp::eval_condition(lowered, features);
         }
         catch (...) {
            // -- If anything goes wrong, treat as false.
            return false;
         }
      }

      // -- Conditional inclusion state for one nesting level.
      struct CondState {
         bool any_taken;     // has any branch been included so far?
         bool active;        // is the current branch included?
      };

   } // anonymous namespace

   Prose read_source(std::u8string_view source,
                     Lisp::Value features)
   {
      Prose text { };
      SourceInput src { source };

      // -- Stack of conditional inclusion states (for nesting).
      std::vector<CondState> cond_stack;

      // -- Helper: are we currently including lines?
      auto including = [&]() -> bool {
         for (auto& cs : cond_stack)
            if (not cs.active)
               return false;
         return true;
      };

      while (auto f = src.get())
      {
         // -- Process the fragment: evaluate reader directives
         // ()if / )elseif / )else / )endif) and handle conditional
         // inclusion.  Other Meta lines pass through unchanged.
         Fragment filtered;
         for (auto& line : f)
         {
            if (line.kind == LineKind::Meta)
            {
               auto dir = parse_directive(line);

               if (dir.keyword == u8"if")
               {
                  bool result = including()
                     and eval_boot_condition(dir.rest, features);
                  cond_stack.push_back({ result, result });
                  continue;
               }
               if (dir.keyword == u8"elseif")
               {
                  if (not cond_stack.empty())
                  {
                     auto& cs = cond_stack.back();
                     if (cs.any_taken)
                        cs.active = false;
                     else
                     {
                        // -- Evaluate only if no branch taken yet
                        // and all enclosing levels are active.
                        bool outer_active = true;
                        for (std::size_t i = 0;
                             i + 1 < cond_stack.size(); ++i)
                           if (not cond_stack[i].active)
                           { outer_active = false; break; }
                        bool result = outer_active
                           and eval_boot_condition(dir.rest,
                                                   features);
                        cs.active = result;
                        if (result)
                           cs.any_taken = true;
                     }
                  }
                  continue;
               }
               if (dir.keyword == u8"else")
               {
                  if (not cond_stack.empty())
                  {
                     auto& cs = cond_stack.back();
                     cs.active = not cs.any_taken;
                  }
                  continue;
               }
               if (dir.keyword == u8"endif")
               {
                  if (not cond_stack.empty())
                     cond_stack.pop_back();
                  continue;
               }
               // -- Non-conditional Meta line ()abbrev, )eval, etc.):
               // include if currently active.
            }

            // -- Include the line only if all conditions are active.
            if (including())
               filtered.push_back(line);
         }

         if (filtered)
            text.push_back(std::move(filtered));
      }
      return text;
   }
}
