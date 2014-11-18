// Copyright (C) 2010-2014, Gabriel Dos Reis.
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
//     - Neither the name of The Numerical Algorithms Group Ltd. nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
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

#ifndef OPENAXIOM_SEXPR_included
#define OPENAXIOM_SEXPR_included

// --% Author: Gabriel Dos Reis.
// --% Description:
// --%   A simple support for s-expressions.  By design, no ambition
// --%   for full-fledged Common Lisp reader capability.  Rather,
// --%   the aim is a simple data structure for exchanging data
// --%   between several components of the OpenAxiom system.
// --%   Users interested in fullblown Lisp syntax should seek
// --%   to acquire Lisp systems, many of which are freely available.

#include <iosfwd>
#include <vector>
#include <open-axiom/storage>
#include <open-axiom/token>

namespace OpenAxiom {
   namespace Sexpr {
      // ------------
      // -- Lexeme --
      // ------------
      struct Lexeme {
         enum Type {
            unknown,                 // unidentified token
            semicolon,               // ";" for comment
            dot,                     // "."
            comma,                   // ","
            open_paren,              // "("
            close_paren,             // ")"
            apostrophe,              // "'"
            backquote,               // "`"
            backslash,               // "\\"
            sharp_open_paren ,       // "#("
            sharp_apostrophe,        // "#'"
            sharp_colon,             // "#:"
            sharp_plus,              // "#+"
            sharp_minus,             // "#-"
            sharp_dot,               // "#."
            comma_at,                // ",@"
            integer,                 // integer literal
            character,               // character literal
            string,                  // string literal
            identifier,              // plain identifier
            sharp_integer_equal,     // anchor definition, #n=<form>
            sharp_integer_sharp      // back reference, #n#
         };

         std::pair<const Byte*, const Byte*> boundary;
         Ordinal line;
         const Byte* begin() const { return boundary.first; }
         const Byte* end() const { return boundary.second; }
         Ordinal size() const { return end() - begin(); }
      };

      // ------------
      // -- Syntax --
      // ------------
      // Base class of syntax object classes.
      struct Syntax {
         struct Visitor;        // base class of syntax visitors
         virtual void accept(Visitor&) const = 0;
      };

      // ----------------
      // -- AtomSyntax --
      // ----------------
      // An atom is a syntax object consisting of exatly one token.
      // This should not be confused with the notion of atom
      // in Lisp languages. 
      struct AtomSyntax : Syntax {
         const Lexeme& lexeme() const { return lex; }
      protected:
         Lexeme lex;
         explicit AtomSyntax(const Lexeme&);
      };

      // -------------------
      // -- IntegerSyntax --
      // -------------------
      // Integer literal syntax objects
      struct IntegerSyntax : AtomSyntax {
         explicit IntegerSyntax(const Lexeme&);
         void accept(Visitor&) const;
      };

      // ---------------------
      // -- CharacterSyntax --
      // ---------------------
      // Character literal syntax objects.
      struct CharacterSyntax : AtomSyntax {
         explicit CharacterSyntax(const Lexeme&);
         void accept(Visitor&) const;
      };

      // ------------------
      // -- StringSyntax --
      // ------------------
      // Striing literal syntax objjects.
      struct StringSyntax : AtomSyntax {
         explicit StringSyntax(const Lexeme&);
         void accept(Visitor&) const;
      };

      // ------------------
      // -- SymbolSyntax --
      // ------------------
      struct SymbolSyntax : AtomSyntax {
         enum Kind {
            ordinary   = 0x0,   // an interned symbol
            uninterned = 0x1,   // uninterned symbol
            absolute   = 0x2,   // case-sensitive symbol
            keyword    = 0x4,   // a keyword symbol
         };
         SymbolSyntax(const Lexeme&, Kind);
         Kind kind() const { return sort; }
         const Byte* begin() const { return lexeme().begin(); }
         const Byte* end() const { return lexeme().end(); }
         std::size_t size() const { return lexeme().size(); }
         Byte operator[](std::size_t i) const { return begin()[i]; }
         void accept(Visitor&) const;
      private:
         const Kind sort;
      };

      // ---------------------
      // -- ReferenceSyntax --
      // ---------------------
      // Back reference object to a syntax object.
      struct ReferenceSyntax : AtomSyntax {
         ReferenceSyntax(const Lexeme&, Ordinal);
         size_t tag() const { return pos; }
         void accept(Visitor&) const;
      private:
         Ordinal pos;
      };

      // ------------------
      // -- AnchorSyntax --
      // ------------------
      // Base anchor syntax object.
      struct AnchorSyntax : Syntax {
         AnchorSyntax(size_t, const Syntax*);
         size_t ref() const { return tag; }
         const Syntax* value() const { return val; }
         void accept(Visitor&) const;
      private:
         const size_t tag;
         const Syntax* const val;
      };

      // -- Abstract over common implementation of unary special operators.
      template<typename T>
      struct unary_form : Syntax {
         const Syntax* body() const { return form; }
         void accept(Visitor&) const;
      protected:
         unary_form(const Syntax* f) : form(f) { }
      private:
         const Syntax* const form;
      };

      template<typename T>
      struct binary_form : Syntax {
         const Syntax* first() const { return rep.first; }
         const Syntax* second() const { return rep.second; }
         void accept(Visitor&) const;
      protected:
         binary_form(const Syntax* f, const Syntax* s) : rep(f, s) { }
      private:
         std::pair<const Syntax*, const Syntax*> rep;
      };

      // -----------------
      // -- QuoteSyntax --
      // -----------------
      // Quotation syntax object.
      struct QuoteSyntax : unary_form<QuoteSyntax> {
         explicit QuoteSyntax(const Syntax*);
      };
      
      // ---------------------
      // -- AntiquoteSyntax --
      // ---------------------
      // Quasi-quotation syntax object.
      struct AntiquoteSyntax : unary_form<AntiquoteSyntax> {
         explicit AntiquoteSyntax(const Syntax*);
      };
      
      // ------------
      // -- Expand --
      // ------------
      // Expansion request inside a quasi-quotation.
      struct Expand : unary_form<Expand> {
         explicit Expand(const Syntax*);
      };
      
      // ----------
      // -- Eval --
      // ----------
      // Read-time evaluation request syntax object.
      struct Eval : unary_form<Eval> {
         explicit Eval(const Syntax*);
      };
      
      // ------------
      // -- Splice --
      // ------------
      // Splice request syntax object inside a quasi-quotation.
      struct Splice : unary_form<Splice> {
         explicit Splice(const Syntax*);
      };
      
      // --------------
      // -- Function --
      // --------------
      // Function literal syntax object.
      struct Function : unary_form<Function> {
         explicit Function(const Syntax*);
      };

      // -------------
      // -- Include --
      // -------------
      // Conditional inclusion syntax object
      struct Include : binary_form<Include> {
         Include(const Syntax*, const Syntax*);
      };

      // -------------
      // -- Exclude --
      // -------------
      // Conditional exclusion syntax object
      struct Exclude : binary_form<Exclude> {
         Exclude(const Syntax*, const Syntax*);
      };

      // ----------------
      // -- ListSyntax --
      // ----------------
      // List syntax objects.
      struct ListSyntax : Syntax, private std::vector<const Syntax*> {
         typedef std::vector<const Syntax*> base;
         using base::const_iterator;
         using base::begin;
         using base::end;
         using base::rbegin;
         using base::rend;
         using base::size;
         using base::empty;
         using base::front;

         ListSyntax();
         ListSyntax(const base&, bool);
         ~ListSyntax();
         void accept(Visitor&) const;
         bool dotted() const { return dot; }
      private:
         bool dot;
      };
      
      // ------------------
      // -- VectorSyntax --
      // ------------------
      // VectorSyntax syntax objects.
      struct VectorSyntax : Syntax, private std::vector<const Syntax*> {
         typedef std::vector<const Syntax*> base;
         using base::const_iterator;
         using base::begin;
         using base::end;
         using base::size;
         using base::operator[];
         using base::empty;

         VectorSyntax();
         explicit VectorSyntax(const base&);
         ~VectorSyntax();
         void accept(Visitor&) const;
      };

      // ---------------------
      // -- Syntax::Visitor --
      // ---------------------
      struct Syntax::Visitor {
         virtual void visit(const IntegerSyntax&) = 0;
         virtual void visit(const CharacterSyntax&) = 0;
         virtual void visit(const StringSyntax&) = 0;
         virtual void visit(const SymbolSyntax&) = 0;
         virtual void visit(const ReferenceSyntax&) = 0;
         virtual void visit(const AnchorSyntax&) = 0;
         virtual void visit(const QuoteSyntax&) = 0;
         virtual void visit(const AntiquoteSyntax&) = 0;
         virtual void visit(const Expand&) = 0;
         virtual void visit(const Eval&) = 0;
         virtual void visit(const Splice&) = 0;
         virtual void visit(const Function&) = 0;
         virtual void visit(const Include&) = 0;
         virtual void visit(const Exclude&) = 0;
         virtual void visit(const ListSyntax&) = 0;
         virtual void visit(const VectorSyntax&) = 0;
      };

      template<typename T>
      void
      unary_form<T>::accept(Visitor& v) const {
         v.visit(static_cast<const T&>(*this));
      }

      template<typename T>
      void
      binary_form<T>::accept(Visitor& v) const {
         v.visit(static_cast<const T&>(*this));
      }

      // ---------------
      // -- Allocator --
      // ---------------
      // Allocator of syntax objects.
      struct Allocator {
         Allocator();
         ~Allocator();

         const IntegerSyntax* make_integer(const Lexeme&);
         const CharacterSyntax* make_character(const Lexeme&);
         const StringSyntax* make_string(const Lexeme&);
         const SymbolSyntax* make_symbol(SymbolSyntax::Kind, const Lexeme&);
         const ReferenceSyntax* make_reference(size_t, const Lexeme&);
         const AnchorSyntax* make_anchor(size_t, const Syntax*);
         const QuoteSyntax* make_quote(const Syntax*);
         const AntiquoteSyntax* make_antiquote(const Syntax*);
         const Expand* make_expand(const Syntax*);
         const Eval* make_eval(const Syntax*);
         const Splice* make_splice(const Syntax*);
         const Function* make_function(const Syntax*);
         const Include* make_include(const Syntax*, const Syntax*);
         const Exclude* make_exclude(const Syntax*, const Syntax*);
         const ListSyntax* make_list(const std::vector<const Syntax*>&, bool = false);
         const VectorSyntax* make_vector(const std::vector<const Syntax*>&);
         
      private:
         Memory::Factory<IntegerSyntax> ints;
         Memory::Factory<CharacterSyntax> chars;
         Memory::Factory<StringSyntax> strs;
         Memory::Factory<SymbolSyntax> syms;
         Memory::Factory<AnchorSyntax> ancs;
         Memory::Factory<ReferenceSyntax> refs;
         Memory::Factory<QuoteSyntax> quotes;
         Memory::Factory<AntiquoteSyntax> antis;
         Memory::Factory<Expand> exps;
         Memory::Factory<Function> funs;
         Memory::Factory<Include> incs;
         Memory::Factory<Exclude> excs;
         Memory::Factory<Eval> evls;
         Memory::Factory<Splice> spls;
         Memory::Factory<ListSyntax> lists;
         Memory::Factory<VectorSyntax> vectors;
         ListSyntax empty_list;
         VectorSyntax empty_vector;
      };

      // -- Reader --
      struct RawInput {
         const Byte* start;
         const Byte* end;
         Ordinal lineno;
      };

      struct Reader {
         struct State {
            RawInput bytes;
            const Byte* cur;
            const Byte* line;
            Allocator alloc;
         };

         explicit Reader(const RawInput&);
         Reader(const Byte*, const Byte*);
         const Byte* position(Ordinal);
         bool at_start() const { return st.cur == st.bytes.start; }
         const Syntax* read();
      private:
         State st;
      };
   }
}

#endif  // OPENAXIOM_SEXPR_included
