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
//     - Neither the name of OpenAxiom. nor the names of its contributors
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
// --% Description:

#include <string.h>
#include <string>
#include <open-axiom/diagnostics>
#include <open-axiom/Parser>

namespace {
   const char* path_basename(const char* begin, const char* end) {
      while (end > begin and end[-1] != '/' and end[-1] != '\\')
         --end;
      return end;
   }

   const char* path_type(const char* begin, const char* end) {
      while (end > begin and end[-1] != '.')
         --end;
      return end;
   }

   int process_file(const char* path) {
      auto end = path + strlen(path);
      if (path == end) {
         std::cerr << "error: empty input file path\n";
         return -1;
      }
      
      auto base = path_basename(path, end);
      if (base == end) {
         std::cerr << "error: invalid input file path\n";
         return -1;
      }
      
      auto type = path_type(base, end);
      if (base == type) {
         std::cerr << "error: input file base without basename\n";
         return -1;
      }

      std::string output { base, type };
      output += "out";
      return OpenAxiom::boot_to_lisp(path, output.c_str());
   }
}

int main(int argc, char* argv[]) {
   for (int i = 1; i < argc; ++i) {
      process_file(argv[i]);
   }
}
