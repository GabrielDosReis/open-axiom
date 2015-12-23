// Copyright (C) 2010-2015, Gabriel Dos Reis.
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

#ifndef OPENAXIOM_STRING_POOL_INCLUDED
#define OPENAXIOM_STRING_POOL_INCLUDED

#include <open-axiom/hash-table>

// --% Author: Gabriel Dos Reis.
// --% Description:
// --%   Basic persistent string facility.
// --%   A stringpool for allocating long-living string objects.

namespace OpenAxiom {
   struct StringPool;

   // ----------------
   // -- StringItem --
   // ----------------
   // String data allocated from a stringpool.
   struct StringItem {
      const Byte* begin() const { return text; }
      const Byte* end() const { return text + length; }
      size_t size() const { return length; }
      bool equal(const Byte*, size_t) const;
   protected:
      const Byte* text;         // pointer to the byte sequence
      size_t length;            // number of bytes in this string
      friend StringPool;
      StringItem() : text(), length() { }
   };

   // ----------------
   // -- StringPool --
   // ----------------
   // A string-pool object is a repository of long-living string objects.
   // It contains no duplicates, therefore allows  fast equality 
   // comparison of string objects.
   struct StringPool : private BasicHashTable<StringItem> {
      using BasicHashTable<StringItem>::EntryType;

      StringPool();
      // Intern a NUL-terminated sequence of characters.
      EntryType* intern(const char*);

      // Intern a sequence of characters given by its start and length.
      EntryType* intern(const Byte*, size_t);
   private:
      Memory::Arena<Byte> strings; // character blub
      // Allocate a string from the internal arena.
      const Byte* make_copy(const Byte*, size_t);
   };

   typedef const StringPool::EntryType* InternedString;
}

#endif  // OPENAXIOM_STRING_POOL_INCLUDED
