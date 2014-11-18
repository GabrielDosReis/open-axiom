// Copyright (C) 2014, Gabriel Dos Reis.
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
//     - Neither the name of OpenAxiom nor the names of its contributors
//       may be used to endorse or promote products derived from this
//       software without specific prior written permission.
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

#include <open-axiom/Lisp>
#include <iostream>
#include <string>

static OpenAxiom::Sexpr::RawInput
raw_input(const std::string& line, OpenAxiom::Ordinal lineno) {
   if (line.empty())
      return { nullptr, nullptr, lineno };
   auto p = reinterpret_cast<const OpenAxiom::Byte*>(&line[0]);
   return { p, p + line.size(), lineno };
}

int main(int argc, char* argv[]) {
   using namespace OpenAxiom;
   Lisp::Evaluator lisp;

   const char* prompt = "*> ";
   std::string line;
   Ordinal lineno { };
   while (true) {
      std::cout << prompt;
      if (!std::getline(std::cin, line))
         break;
      else if (line.empty())
         continue;
      Sexpr::Reader reader { raw_input(line, ++lineno) };
      while (true) {
         try {
            auto s = reader.read();
            if (s == nullptr)
               break;
            auto value = lisp.eval(s);
            Lisp::format(value, std::cout);
            std::cout << '\n';
         }
         catch (const OpenAxiom::Diagnostics::BasicError& e) {
            std::cerr << e.message() << std::endl;
         }
      }
   }

   std::cout << std::endl;
}

