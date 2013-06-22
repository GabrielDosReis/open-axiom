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
// --%   Memory management facility.  Acquire raw memory directly
// --%   from the host OS.  Provide random access read to
// --%   files through file mapping.

#ifndef OPENAXIOM_STORAGE_included
#define OPENAXIOM_STORAGE_included

#include <open-axiom/config>
#include <stddef.h>
#include <new>
#include <cmath>
#include <string>

namespace OpenAxiom {
   // Datatype for the unit of storage.
   using Byte = unsigned char;

   // -----------------
   // -- SystemError --
   // -----------------
   // Objects of (type derived from) this type are used to report
   // error orignating from the OpenAxiom core system.
   struct SystemError {
      explicit SystemError(const std::string&);
      virtual ~SystemError();
      // Return the text of the diagnostic message.
      virtual const std::string& message() const;
   protected:
      const std::string text;
   };

   // Report a file system error
   void filesystem_error(const std::string&);
   
   namespace Memory {
      // Datatype for pointers to data.
      using Pointer = void*;

      // Precision of the host OS storage page unit in byte count
      size_t page_size();

      // Acquire raw memory from the host OS.
      Pointer os_acquire_raw_memory(size_t);

      // Release raw storage to the hosting OS.  The first operand must
      // be a pointer value previously returned by `os_acquire_raw_memory'.
      // Otherwise, the result is undefined.
      void os_release_raw_memory(Pointer, size_t);

      // Acquire `n' pages of memory storage from the host OS.
      inline Pointer
      acquire_raw_pages(size_t n) {
         return os_acquire_raw_memory(n * page_size());
      }

      // Release `n' pages of storage starting the location `p'.
      inline void
      release_raw_pages(Pointer p, size_t n) {
         os_release_raw_memory(p, n * page_size());
      }
      
      // -------------
      // -- Storage --
      // -------------
      // This class provides low-level abstractions intented for use
      // to implement higher level storage abstractions.
      struct Storage {
         // Objects of this abstract datatype hold storage objects.
         struct Handle;
         
         // Return the storage pointed to by the operand.  It
         // must be a pointer value previously returned by `acquire'.
         // Otherwise, the result is undefined.
         static void release(Handle*);
         
         // Return the start address of storage area.  Clients would
         // want to add padding bytes necessary to accomodate alignment
         // requirements.  
         // Note: this should not be confused with the address of
         // the handle object.
         static Pointer begin(Handle*);

         // Courtesy conversion function from pointer to byte address.
         static Byte* byte_address(Pointer h) {
            return static_cast<Byte*>(h);
         }

         // Round up `n' to a multiple of `a', a power of 2.
         static size_t
         round_up(size_t n, size_t a) {
            return (n + a - 1) & ~(a - 1);
         }
      };

      // -------------------------
      // -- SinglyLinkedStorage --
      // -------------------------
      // This class implements a simple single-linked list of storage
      // objects.  Each storage object in the link is created with
      // a specific starting alignment requirements.
      struct SinglyLinkedStorage : Storage {
         // Return the previous handle in the link chain.
         static Handle*& previous(Handle*);
      };

      // -------------------------
      // -- DoublyLinkedStorage --
      // -------------------------
      // Like SinglyLinkedStorage, except that the chain of storage
      // object supports bidirectional travervsal.
      struct DoublyLinkedStorage : Storage {
         // Same as Storage::acquire, except that begin(h) returns an
         // address that satisfies the alignment requirement `a'.
         static Handle* acquire(size_t n, size_t a);
         
         // Return the previous handle in the link chain.
         static Handle*& previous(Handle*);
         
         // Return the next handle in the link chain.
         static Handle*& next(Handle*);
      };

      // ------------------
      // -- BlockStorage --
      // ------------------
      // This class implements a simple single-linked list of block storage.
      // Each block maintains information about the next allocatable
      // address within that block.
      struct BlockStorage : SinglyLinkedStorage {
         // Same as SinglyLinkedStorage::acquire(); initialize internal
         // bookkeepking machinery.
         static Handle* acquire(size_t n, size_t a);

         // Return the next allocatable address within the given block.
         static Pointer next_address(Handle*);

         // Return the amount of allocatable byte in the given block.
         static size_t room(Handle*);

         // Set `n' bytes aside with the given storage block.
         static Pointer book(Handle*, size_t);
      };

      // -----------
      // -- Arena --
      // -----------
      // Extensible storage holding objects of a given type.
      // The totality of all objects held in such a storage does not
      // necessarily constitute a contiguous block.  However,
      // it is guaranteed that objects allocated in a single call
      // to `allocate()' occupy a contiguous block of storage.
      template<typename T>
      struct Arena : protected BlockStorage {
         // Acquire storage capable of holding `n' objects of type `T'.
         explicit Arena(size_t);
         // Release all storage acquired by this object, upon end of life.
         ~Arena();
         // allocate storage for `n' more objects of type `T'.
         T* allocate(size_t);
         // Number of objects of type `T' allocated in this storage.
         size_t population() const;

      protected:
         // Address of the first object of type `T' in a storage.
         static T* first_object(Handle* h) {
            return static_cast<T*>(BlockStorage::begin(h));
         }

         // Address of one-past-the-end object of type `T' in this storage.
         static T* last_object(Handle* h) {
            return static_cast<T*>(BlockStorage::next_address(h));
         }

         // Number of objects allocated in a storage.
         static size_t object_count(Handle* h) {
            return last_object(h) - first_object(h);
         }

         BlockStorage::Handle* store; // active storage to allocate from
      };

      template<typename T>
      size_t
      Arena<T>::population() const {
         size_t n = 0;
         for (Handle* h = store; h != nullptr; h = previous(h))
            n += object_count(h);
         return n;
      }

      template<typename T>
      T*
      Arena<T>::allocate(size_t n) {
         const size_t sz = n * sizeof(T);
         if (BlockStorage::room(store) < sz) {
            // Not enough room left.  Make sure we allocate storage
            // at least as big as the current.
            Handle* h = acquire(std::max(n, object_count(store)),
                                openaxiom_alignment(T));
            previous(h) = store;
            store = h;
         }
         return static_cast<T*>(BlockStorage::book(store, sz));
      }

      template<typename T>
      Arena<T>::Arena(size_t n)
            : store(BlockStorage::acquire(n * sizeof (T),
                                          openaxiom_alignment (T)))
      { }
      
      template<typename T>
      Arena<T>::~Arena() {
         // Release storage in the reverse order of their
         // their allocation.
         while (store != nullptr) {
            Handle* current = store;
            store = BlockStorage::previous(store);
            BlockStorage::release(current);
         }
      }

      // -------------
      // -- Factory --
      // -------------
      template<typename T>
      struct Factory : Arena<T> {
         using Handler = typename Arena<T>::Handle;

         Factory() : Arena<T>(nominal_population()) { }
         ~Factory();

         // Allocate storage and value-construct an object of type `T'.
         T* make() {
            return new(this->allocate(1)) T();
         }

         // Allocate storage and construct an object of type `T'.
         template<typename... Args>
         T* make(const Args&... args) {
            return new(this->allocate(1)) T(args...);
         }

      private:
         // Return 1 or the number of objects that can fit in a page unit.
         static size_t nominal_population() {
            const size_t psz = page_size();
            if (sizeof (T) > psz)
               return 1;
            return psz / sizeof(T);
         }
      };

      // Destroy objects in the reverse order of their construction.
      template<typename T>
      Factory<T>::~Factory() {
         for (auto s = this->store; s != nullptr; s = Arena<T>::previous(s)) {
            T* last = Arena<T>::last_object(s);
            for (--last; last >= Arena<T>::first_object(s); --last)
               last->~T();
         }
      }
   }
}

#endif  // OPENAXIOM_STORAGE_included

