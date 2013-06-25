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
#include <open-axiom/string-pool>
#include <open-axiom/token>

namespace OpenAxiom {
   namespace Sexpr {
      struct BasicError {
         explicit BasicError(const std::string& s) : msg(s) { }
         const std::string& message() const { return msg; }
      protected:
         std::string msg;
      };

      // -----------
      // -- Token --
      // -----------
      struct Token {
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

         const Byte* tokenize(const Byte*, const Byte*);
         BasicString intern(const Byte* s, size_t n) {
            return strings.intern(s, n);
         }

      private:
         StringPool& strings;        // where to allocate lexemes from
         std::vector<Token>& tokens; // where to deposite tokens.
      };

      // ------------
      // -- Syntax --
      // ------------
      // Base class of syntax object classes.
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
      // Integer literal syntax objects
      struct Integer : Atom {
         explicit Integer(const Token&);
         void accept(Visitor&) const;
      };

      // ---------------
      // -- Character --
      // ---------------
      // Character literal syntax objects.
      struct Character : Atom {
         explicit Character(const Token&);
         void accept(Visitor&) const;
      };

      // ------------
      // -- String --
      // ------------
      // Striing literal syntax objjects.
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
      // Back reference object to a syntax object.
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
      // Base anchor syntax object.
      struct Anchor : Syntax {
         Anchor(size_t, const Syntax*);
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

      // -----------
      // -- Quote --
      // -----------
      // Quotation syntax object.
      struct Quote : unary_form<Quote> {
         explicit Quote(const Syntax*);
      };
      
      // ---------------
      // -- Antiquote --
      // ---------------
      // Quasi-quotation syntax object.
      struct Antiquote : unary_form<Antiquote> {
         explicit Antiquote(const Syntax*);
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
      // -- DotTail --
      // -------------
      // Objects of this type represents the tail of syntactic
      // objects denoting dotted pair syntax `(a . b)'.
      struct DotTail : unary_form<DotTail> {
         explicit DotTail(const Syntax*);
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
      // -- List --
      // ----------
      // List syntax objects.
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
      // Vector syntax objects.
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
         virtual void visit(const Character&);
         virtual void visit(const String&);
         virtual void visit(const Symbol&);
         virtual void visit(const Reference&);
         virtual void visit(const Anchor&) = 0;
         virtual void visit(const Quote&) = 0;
         virtual void visit(const Antiquote&) = 0;
         virtual void visit(const Expand&) = 0;
         virtual void visit(const Eval&) = 0;
         virtual void visit(const Splice&) = 0;
         virtual void visit(const Function&) = 0;
         virtual void visit(const Include&) = 0;
         virtual void visit(const Exclude&) = 0;
         virtual void visit(const DotTail&) = 0;
         virtual void visit(const List&) = 0;
         virtual void visit(const Vector&) = 0;
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

         const Integer* make_integer(const Token&);
         const Character* make_character(const Token&);
         const String* make_string(const Token&);
         const Symbol* make_symbol(const Token&, Symbol::Kind);
         const Reference* make_reference(const Token&, size_t);
         const Anchor* make_anchor(size_t, const Syntax*);
         const Quote* make_quote(const Syntax*);
         const Antiquote* make_antiquote(const Syntax*);
         const Expand* make_expand(const Syntax*);
         const Eval* make_eval(const Syntax*);
         const Splice* make_splice(const Syntax*);
         const Function* make_function(const Syntax*);
         const Include* make_include(const Syntax*);
         const Exclude* make_exclude(const Syntax*);
         const DotTail* make_dot_tail(const Syntax*);
         const List* make_list(const std::vector<const Syntax*>&);
         const Vector* make_vector(const std::vector<const Syntax*>&);
         
      private:
         Memory::Factory<Integer> ints;
         Memory::Factory<Character> chars;
         Memory::Factory<String> strs;
         Memory::Factory<Symbol> syms;
         Memory::Factory<Anchor> ancs;
         Memory::Factory<Reference> refs;
         Memory::Factory<Quote> quotes;
         Memory::Factory<Antiquote> antis;
         Memory::Factory<Expand> exps;
         Memory::Factory<Function> funs;
         Memory::Factory<Include> incs;
         Memory::Factory<Exclude> excs;
         Memory::Factory<Eval> evls;
         Memory::Factory<Splice> spls;
         Memory::Factory<DotTail> tails;
         Memory::Factory<List> lists;
         Memory::Factory<Vector> vectors;
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
         const Character* parse_character(const Token*&, const Token*);
         const Anchor* parse_anchor(const Token*&, const Token*);
         const Reference* parse_reference(const Token*&, const Token*);
         const Symbol* parse_uninterned(const Token*&, const Token*);
         const Function* parse_function(const Token*&, const Token*);
         const Quote* parse_quote(const Token*&, const Token*);
         const Antiquote* parse_antiquote(const Token*&, const Token*);
         const Include* parse_include(const Token*&, const Token*);
         const Exclude* parse_exclude(const Token*&, const Token*);
         const Expand* parse_expand(const Token*&, const Token*);
         const Eval* parse_eval(const Token*&, const Token*);
         const Splice* parse_splice(const Token*&, const Token*);
         const Vector* parse_vector(const Token*&, const Token*);
         const List* parse_list(const Token*&, const Token*);
         const Syntax* parse_syntax(const Token*&, const Token*);
      };

      // ------------
      // -- Module --
      // ------------
      // Entire s-expression input file.
      struct Module : std::vector<const Syntax*> {
         explicit Module(const std::string&);
         const std::string& name() const { return nm; }
      private:
         const std::string nm;
         StringPool raw_strs;
         Allocator allocator;
      };
   }
}

#endif  // OPENAXIOM_SEXPR_included
