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

#ifndef OPENAXIOM_HASH_TABLE_INCLUDED
#define OPENAXIOM_HASH_TABLE_INCLUDED

// --% Author: Gabriel Dos Reis.
// --% Description:
// --%   Simple hash table facility.  To be replaced by C++0x
// --%   hash tables when C++0x compilers become common place.

#include <open-axiom/storage>

namespace OpenAxiom {
   // --------------------
   // -- HashTableEntry --
   // --------------------
   // Datatype for entries in a parameterized hash table.
   // The type parameter is required to be a value-construcitble datatype.
   // A table bucket entry is required to be at least 8-byte aligned
   // so that an instance of it can be used directly as a VM value.
   // See <open-axiom/vm> for more description.
   template<typename T>
   struct alignas(8) HashTableEntry : T {
      HashTableEntry* chain;   // previous item in the same bucket chain
      size_t hash;             // hash code of stored data
   };

   // --------------------
   // -- BasicHashTable --
   // --------------------
   // A simple hash table data structure.  Ideally, we would like to use
   // standard C++ types, but hash tables were only in a C++ 2003 TR,
   // officially part of C++0x standard library.  We still don't have
   // wide-spread C++0x compilers.
   template<typename T>
   struct BasicHashTable : private Memory::Arena<HashTableEntry<T> > {
      typedef HashTableEntry<T> EntryType;
      explicit BasicHashTable(size_t n)
            : Memory::Arena<HashTableEntry<T> >(n),
              buckets(this->allocate(n)), nbuckets(n) { }

      EntryType* hash_chain(size_t h) const {
         return buckets + (h % nbuckets);
      }

      EntryType* new_bucket() {
         return this->allocate(1);
      }

   private:
      HashTableEntry<T>* const buckets;
      const size_t nbuckets;
   };
}

#endif  // OPENAXIOM_HASH_TABLE_INCLUDED
