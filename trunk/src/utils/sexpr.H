// Copyright (C) 2010, Gabriel Dos Reis.
// All rights reserved.
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

#ifndef OPENAXIOM_SEXPR_INCLUDED
#define OPENAXIOM_SEXPR_INCLUDED

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
#include <set>
#include <open-axiom/string-pool>

// Helpers for defining token type values for lexeme with more
// than characters.
#define OPENAXIOM_SEXPR_TOKEN1(C)  (C)
#define OPENAXIOM_SEXPR_TOKEN2(C1,C2) (C1 * 256 + C2)

namespace OpenAxiom {
   namespace Sexpr {
      // -----------
      // -- Token --
      // -----------
      struct Token {
         enum Type {
            unknown,                 // unidentified token
            dot              = OPENAXIOM_SEXPR_TOKEN1('.'),      // "."
            comma            = OPENAXIOM_SEXPR_TOKEN1(','),      // ","
            open_paren       = OPENAXIOM_SEXPR_TOKEN1('('),      // "("
            close_paren      = OPENAXIOM_SEXPR_TOKEN1(')'),      // ")"
            apostrophe       = OPENAXIOM_SEXPR_TOKEN1('\''),     // "'"
            backquote        = OPENAXIOM_SEXPR_TOKEN1('`'),      // "`"
            backslash        = OPENAXIOM_SEXPR_TOKEN1('\\'),     // "\\"
            sharp_open_paren = OPENAXIOM_SEXPR_TOKEN2('#','('),  // "#("
            sharp_apostrophe = OPENAXIOM_SEXPR_TOKEN2('#','\''), // "#'"
            sharp_colon      = OPENAXIOM_SEXPR_TOKEN2('#',':'),  // "#:"
            digraph_end      = OPENAXIOM_SEXPR_TOKEN2(256,256),
            integer,                // integer literal
            string,                 // string literal
            identifier,             // plain identifier
            sharp_integer_equal,    // anchor definition, #n=<form>
            sharp_integer_sharp     // back reference, #n#
         };

         Type type;             // class of this token
         BasicString lexeme;    // characters making up this token
      };

      // Print a token object on an output stream.
      // Note: this function is for debugging purpose; in particular
      // it does not `prettyprint' tokens.
      std::ostream& operator<<(std::ostream&, const Token&);

      // -----------
      // -- Lexer --
      // -----------
      // An object of this type transforms a sequence of characters
      // into a sequence of tokens as defined above.
      // A lexer does not manage memory itself.  Rather, it delegates
      // storage allocation for lexemes and tokens to specialized
      // agents used to construct it.
      struct Lexer {
         Lexer(StringPool& pool, std::vector<Token>& toks)
               : strings(pool), tokens(toks) { }

         const char* tokenize(const char*, const char*);

      private:
         StringPool& strings;        // where to allocate lexemes from
         std::vector<Token>& tokens; // where to deposite tokens.
      };

      // ------------
      // -- Syntax --
      // ------------
      struct Syntax {
         struct Visitor;        // base class of syntax visitors
         virtual void accept(Visitor&) const = 0;
      };

      // ----------
      // -- Atom --
      // ----------
      // An atom is a syntax object consisting of exatly one token.
      // This should not be confused with the notion of atom
      // in Lisp languages. 
      struct Atom : Syntax {
         const Token& token() const { return tok; }
         BasicString lexeme() const { return tok.lexeme; }
         void accept(Visitor&) const;
      protected:
         const Token tok;
         Atom(const Token&);
      };

      // -------------
      // -- Integer --
      // -------------
      struct Integer : Atom {
         explicit Integer(const Token&);
         void accept(Visitor&) const;
      };

      // ------------
      // -- String --
      // ------------
      struct String : Atom {
         explicit String(const Token&);
         void accept(Visitor&) const;
      };

      // ------------
      // -- Symbol --
      // ------------
      struct Symbol : Atom {
         enum Kind {
            uninterned,         // uninterned symbol
            ordinary,           // an interned symbol
            keyword             // a keyword symbol
         };
         Symbol(const Token&, Kind);
         Kind kin() const { return sort; }
         void accept(Visitor&) const;
      private:
         const Kind sort;
      };

      // ---------------
      // -- Reference --
      // ---------------
      struct Reference : Atom {
         Reference(const Token&, size_t);
         size_t tag() const { return pos; }
         void accept(Visitor&) const;
      private:
         const size_t pos;
      };

      // ------------
      // -- Anchor --
      // ------------
      struct Anchor : Syntax {
         Anchor(size_t, const Syntax*);
         size_t ref() const { return tag; }
         const Syntax* value() const { return val; }
         void accept(Visitor&) const;
      private:
         const size_t tag;
         const Syntax* const val;
      };

      // -----------
      // -- Quote --
      // -----------
      struct Quote : Syntax {
         explicit Quote(const Syntax*);
         const Syntax* body() const { return form; }
         void accept(Visitor&) const;
      private:
         const Syntax* const form;
      };
      
      // --------------
      // -- Function --
      // --------------
      struct Function : Syntax {
         explicit Function(const Syntax*);
         const Syntax* code() const { return form; }
         void accept(Visitor&) const;
      private:
         const Syntax* const form;
      };

      // ----------
      // -- Pair --
      // ----------
      struct Pair : Syntax {
         Pair(const Syntax*, const Syntax*);
         const Syntax* first() const { return elts.first; }
         const Syntax* second() const { return elts.second; }
         void accept(Visitor&) const;
      private:
         const std::pair<const Syntax*, const Syntax*> elts;
      };

      // ----------
      // -- List --
      // ----------
      struct List : Syntax, private std::vector<const Syntax*> {
         typedef std::vector<const Syntax*> base;
         using base::const_iterator;
         using base::begin;
         using base::end;
         using base::size;
         using base::empty;

         List();
         explicit List(const base&);
         ~List();
         void accept(Visitor&) const;
      };
      
      // ------------
      // -- Vector --
      // ------------
      struct Vector : Syntax, private std::vector<const Syntax*> {
         typedef std::vector<const Syntax*> base;
         using base::const_iterator;
         using base::begin;
         using base::end;
         using base::size;
         using base::operator[];
         using base::empty;

         Vector();
         explicit Vector(const base&);
         ~Vector();
         void accept(Visitor&) const;
      };

      // ---------------------
      // -- Syntax::Visitor --
      // ---------------------
      struct Syntax::Visitor {
         virtual void visit(const Atom&) = 0;
         virtual void visit(const Integer&);
         virtual void visit(const String&);
         virtual void visit(const Symbol&);
         virtual void visit(const Reference&);
         virtual void visit(const Anchor&) = 0;
         virtual void visit(const Quote&) = 0;
         virtual void visit(const Function&) = 0;
         virtual void visit(const Pair&) = 0;
         virtual void visit(const List&) = 0;
         virtual void visit(const Vector&) = 0;
      };

      // ---------------
      // -- Allocator --
      // ---------------

      // The next two classes are helper classes for the main
      // allocation class Allocator.  We use std::set as allocator
      // that guarantee uuniqueness of atomic syntax object with
      // respect to the constituent token.  That container needs
      // a relational comparator.  In an ideal world, this class
      // should not exist.
      struct SyntaxComparator {
         bool operator()(const Atom& lhs, const Atom& rhs) const {
            return std::less<BasicString>()(lhs.lexeme(), rhs.lexeme());
         }

         bool operator()(const Quote& lhs, const Quote& rhs) const {
            return std::less<const void*>()(lhs.body(), rhs.body());
         }

         bool operator()(const Anchor& lhs, const Anchor& rhs) const {
            return std::less<size_t>()(lhs.ref(), rhs.ref());
         }

         bool operator()(const Function& lhs, const Function& rhs) const {
            return std::less<const void*>()(lhs.code(), rhs.code());
         }

         bool operator()(const Pair& lhs, const Pair& rhs) const {
            std::less<const void*> cmp;
            if (cmp(lhs.first(), rhs.first()))
               return true;
            if (cmp(rhs.first(), lhs.first()))
               return false;
            return cmp(lhs.second(), rhs.second());
         }
      };

      template<typename T>
      struct UniqueAllocator : std::set<T, SyntaxComparator> {
         typedef std::set<T, SyntaxComparator> base;
         typedef typename base::const_iterator const_iterator;

         template<typename U>
         const T* allocate(const U& u) {
            return &*this->insert(T(u)).first;
         }
         
         template<typename U, typename V>
         const T* allocate(const U& u, const V& v) {
            return &*this->insert(T(u, v)).first;
         }
      };
      
      // Allocator of syntax objects.
      struct Allocator {
         Allocator();
         ~Allocator();

         const Integer* make_integer(const Token&);
         const String* make_string(const Token&);
         const Symbol* make_symbol(const Token&, Symbol::Kind);
         const Reference* make_reference(const Token&, size_t);
         const Anchor* make_anchor(size_t, const Syntax*);
         const Quote* make_quote(const Syntax*);
         const Function* make_function(const Syntax*);
         const Pair* make_pair(const Syntax*, const Syntax*);
         const List* make_list(const std::vector<const Syntax*>&);
         const Vector* make_vector(const std::vector<const Syntax*>&);
         
      private:
         UniqueAllocator<Integer> ints;
         UniqueAllocator<String> strs;
         UniqueAllocator<Symbol> syms;
         UniqueAllocator<Anchor> ancs;
         UniqueAllocator<Reference> refs;
         UniqueAllocator<Quote> quotes;
         UniqueAllocator<Function> funs;
         UniqueAllocator<Pair> pairs;
         Memory::Arena<List> lists;
         Memory::Arena<Vector> vectors;
         List empty_list;
         Vector empty_vector;
      };

      // ------------
      // -- Parser --
      // ------------
      // An object of this type transforms a sequence of tokens
      // into a sequence of syntax objects.
      // A parser object does not manage memory itself.  Rather, it delegates
      // storage allocation for syntax objects to specialized
      // agents used to construct it.
      struct Parser {
         Parser(Allocator&, std::vector<const Syntax*>&);
         const Token* parse(const Token*, const Token*);
      private:
         Allocator& alloc;
         std::vector<const Syntax*>& syns;

         const Symbol* parse_symbol(const Token*&, const Token*);
         const Anchor* parse_anchor(const Token*&, const Token*);
         const Reference* parse_reference(const Token*&, const Token*);
         const Symbol* parse_uninterned(const Token*&, const Token*);
         const Function* parse_function(const Token*&, const Token*);
         const Quote* parse_quote(const Token*&, const Token*);
         const Vector* parse_vector(const Token*&, const Token*);
         const Syntax* parse_list_or_pair(const Token*&, const Token*);
         const Syntax* parse_syntax(const Token*&, const Token*);
      };
   }
}

#endif  // OPENAXIOM_SEXPR_INCLUDED
