// Copyright (C) 2010-2011, Gabriel Dos Reis.
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

// --% Author: Gabriel Dos Reis

#include <string.h>
#include <open-axiom/string-pool>

namespace OpenAxiom {
   // ----------------
   // -- StringItem --
   // ----------------
   bool
   StringItem::equal(const Byte* str, size_t sz) const {
      if (length != sz)
         return false;
      for (size_t i = 0; i < sz; ++i)
         if (text[i] != str[i])
            return false;
      return true;
   }


   // ----------------
   // -- StringPool --
   // ----------------
   StringPool::StringPool()
         : BasicHashTable<StringItem>(109),
           strings(2 * Memory::page_size())
   { }

   // Return a hash for the string starting from `str'
   // of length `sz'.
   static size_t
   hash(const Byte* str, size_t sz) {
      size_t h = 0;
      for(size_t i = 0; i < sz; ++i)
         h = str[i] + (h << 6) + (h << 16) - h;
      return h;
   }

   const Byte*
   StringPool::make_copy(const Byte* f, size_t sz) {
      Byte* s = strings.allocate(sz + 1);
      memcpy(s, f, sz);
      s[sz] = '\0';
      return s;
   }
   
   StringPool::EntryType*
   StringPool::intern(const Byte* src, size_t sz) {
      const size_t h = hash(src, sz);
      EntryType* e = hash_chain(h);
      if (sz == 0)
         return e;
      for (; e->text != 0; e = e->chain) {
         if (e->hash == h and e->equal(src, sz))
            return e;
         // If this is the last entry in this hash chain, allocate
         // a new bucket to hold the information we want to store.
         if (e->chain == 0)
            e->chain = new_bucket();
      }
      e->text = make_copy(src, sz);
      e->length = sz;
      e->hash = h;
      return e;
   }

   StringPool::EntryType*
   StringPool::intern(const char* s) {
      return intern(reinterpret_cast<const Byte*>(s), strlen(s));
   }
}
