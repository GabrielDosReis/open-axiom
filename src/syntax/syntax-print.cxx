// -- -*- C++ -*-
// Copyright (C) 2026, Gabriel Dos Reis.
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

// --% Author: Gabriel Dos Reis
// --% Description:
// --%   S-expression printer for syntax trees.  Uses the node
// --%   descriptor infrastructure from SyntaxTree to drive a
// --%   generic, data-driven formatting pass.

#include <iostream>
#include <open-axiom/SyntaxPrint>
#include <open-axiom/Charset>

namespace OpenAxiom::Syntax {

    // -- Extract the source text of a token.
    std::u8string token_text(TokenIndex ti,
                             const std::vector<Token>& toks,
                             const Fragment& frag)
    {
        auto idx = std::uint32_t(ti);
        if (idx >= toks.size())
            return u8"???";
        auto& tok = toks[idx];
        // -- Single-line token: extract substring.
        if (tok.start.line == tok.end.line)
        {
            auto& line = frag[tok.start.line];
            auto col0 = tok.start.column;
            auto col1 = tok.end.column;
            if (col1 <= col0)
                return u8"";
            return std::u8string(line.data() + col0, col1 - col0);
        }
        // -- Multi-line token (e.g. string with continuation):
        // concatenate lines.
        std::u8string result;
        for (auto ln = tok.start.line; ln <= tok.end.line; ++ln)
        {
            auto& line = frag[ln];
            auto col0 = (ln == tok.start.line) ? tok.start.column : 0;
            auto col1 = (ln == tok.end.line) ? tok.end.column
                                             : static_cast<uint16_t>(line.size());
            result.append(line.data() + col0, col1 - col0);
        }
        return result;
    }

    // -- Emit a token's text, quoting it with |bars| if needed.
    void print_token(std::ostream& os, TokenIndex ti,
                     const std::vector<Token>& toks,
                     const Fragment& frag)
    {
        auto text = token_text(ti, toks, frag);
        if (needs_bar_quoting(text))
            print_bar_quoted(os, text);
        else
            os << std::u8string_view(text);
    }

    // -- Print a NodeField value to a stream.
    static void print_field(std::ostream& os,
                            const NodeField& f,
                            const SyntaxForest& forest,
                            const std::vector<Token>& toks,
                            const Fragment& frag)
    {
        switch (f.role)
        {
        case FieldRole::Child:
            print_sexpr(os, f.child, forest, toks, frag);
            break;
        case FieldRole::Span:
            for (std::uint32_t i = 0; i < f.span.count; ++i)
            {
                if (i > 0) os << ' ';
                print_sexpr(os, forest.child(f.span, i),
                            forest, toks, frag);
            }
            break;
        case FieldRole::WrappedSpan:
            os << '(';
            for (std::uint32_t i = 0; i < f.span.count; ++i)
            {
                if (i > 0) os << ' ';
                print_sexpr(os, forest.child(f.span, i),
                            forest, toks, frag);
            }
            os << ')';
            break;
        case FieldRole::Token:
            print_token(os, f.token, toks, frag);
            break;
        case FieldRole::Literal:
            os << token_text(f.token, toks, frag);
            break;
        }
    }

    // -- Print a syntax node as an S-expression.
    void print_sexpr(std::ostream& os,
                     NodeIndex node,
                     const SyntaxForest& forest,
                     const std::vector<Token>& toks,
                     const Fragment& frag)
    {
        if (not valid(node))
        {
            os << "NIL";
            return;
        }

        // -- Special case: Unparsed uses " .. " separator.
        if (kind(node) == Kind::Unparsed)
        {
            auto& n = forest.unparsed_node(index(node));
            os << "(%Unparsed ";
            print_token(os, n.first, toks, frag);
            os << " .. ";
            print_token(os, n.last, toks, frag);
            os << ')';
            return;
        }

        auto desc = describe_node(node, forest);

        // -- Leaf nodes: print single field directly.
        if (desc.tag == nullptr)
        {
            print_field(os, desc.fields[0], forest, toks, frag);
            return;
        }

        // -- Tagged compound node: (%Tag field1 field2 ...).
        os << "(%" << desc.tag;
        for (std::uint8_t i = 0; i < desc.count; ++i)
        {
            os << ' ';
            print_field(os, desc.fields[i], forest, toks, frag);
        }
        os << ')';
    }
}
