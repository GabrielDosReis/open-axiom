// Copyright (C) 2010-2013, Gabriel Dos Reis.
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
            semicolon        = token::value(";"), // comment
            dot              = token::value("."),
            comma            = token::value(","),
            open_paren       = token::value("("),
            close_paren      = token::value(")"),
            apostrophe       = token::value("'"),
            backquote        = token::value("`"),
            backslash        = token::value("\\"),
            sharp_open_paren = token::value("#("),
            sharp_apostrophe = token::value("#'"),
            sharp_colon      = token::value("#:"),
            sharp_plus       = token::value("#+"),
            sharp_minus      = token::value("#-"),
            sharp_dot        = token::value("#."),
            comma_at         = token::value(",@"),
            digraph_end      = token::value(0xff,0xff),
            integer,                // integer literal
            character,              // character literal
            string,                 // string literal
            identifier,             // plain identifier
            sharp_integer_equal,    // anchor definition, #n=<form>
            sharp_integer_sharp     // back reference, #n#
         };

         std::pair<const Byte*, const Byte*> boundary;
         Ordinal line;
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
         void accept(Visitor&) const;
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
            uninterned,         // uninterned symbol
            ordinary,           // an interned symbol
            absolute,           // case-sensitive symbol
            keyword             // a keyword symbol
         };
         SymbolSyntax(const Lexeme&, Kind);
         Kind kind() const { return sort; }
         void accept(Visitor&) const;
      private:
         const Kind sort;
      };

      // ---------------
      // -- Reference --
      // ---------------
      // Back reference object to a syntax object.
      struct Reference : AtomSyntax {
         Reference(const Lexeme&, Ordinal);
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
      struct Include : unary_form<Include> {
         explicit Include(const Syntax*);
      };

      // -------------
      // -- Exclude --
      // -------------
      // Conditional exclusion syntax object
      struct Exclude : unary_form<Exclude> {
         explicit Exclude(const Syntax*);
      };

      // ----------
      // -- ListSyntax --
      // ----------
      // List syntax objects.
      struct ListSyntax : Syntax, private std::vector<const Syntax*> {
         typedef std::vector<const Syntax*> base;
         using base::const_iterator;
         using base::begin;
         using base::end;
         using base::size;
         using base::empty;

         ListSyntax();
         ListSyntax(const base&, bool);
         ~ListSyntax();
         void accept(Visitor&) const;
         bool dotted() const { return dot; }
      private:
         bool dot;
      };
      
      // ------------
      // -- VectorSyntax --
      // ------------
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
         virtual void visit(const AtomSyntax&) = 0;
         virtual void visit(const IntegerSyntax&);
         virtual void visit(const CharacterSyntax&);
         virtual void visit(const StringSyntax&);
         virtual void visit(const SymbolSyntax&);
         virtual void visit(const Reference&);
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
         const Reference* make_reference(size_t, const Lexeme&);
         const AnchorSyntax* make_anchor(size_t, const Syntax*);
         const QuoteSyntax* make_quote(const Syntax*);
         const AntiquoteSyntax* make_antiquote(const Syntax*);
         const Expand* make_expand(const Syntax*);
         const Eval* make_eval(const Syntax*);
         const Splice* make_splice(const Syntax*);
         const Function* make_function(const Syntax*);
         const Include* make_include(const Syntax*);
         const Exclude* make_exclude(const Syntax*);
         const ListSyntax* make_list(const std::vector<const Syntax*>&, bool = false);
         const VectorSyntax* make_vector(const std::vector<const Syntax*>&);
         
      private:
         Memory::Factory<IntegerSyntax> ints;
         Memory::Factory<CharacterSyntax> chars;
         Memory::Factory<StringSyntax> strs;
         Memory::Factory<SymbolSyntax> syms;
         Memory::Factory<AnchorSyntax> ancs;
         Memory::Factory<Reference> refs;
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
      struct Reader {
         struct State {
            const Byte* start;
            const Byte* end;
            const Byte* cur;
            const Byte* line;
            Ordinal lineno;
            Allocator alloc;
         };

         Reader(const Byte*, const Byte*);
         const Syntax* read();
      private:
         State st;
      };
   }
}

#endif  // OPENAXIOM_SEXPR_included
