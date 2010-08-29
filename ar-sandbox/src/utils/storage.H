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

// --% Author: Gabriel Dos Reis
// --% Description:
// --%   Memory management facility.  Acquire raw memory directly
// --%   directly for the host OS.  Provide random access read to
// --%   files through file mapping.

#ifndef OPENAXIOM_STORAGE_INCLUDED
#define OPENAXIOM_STORAGE_INCLUDED

#include <stddef.h>
#include <string.h>
#include <new>
#include <cmath>
#include <string>

#include <open-axiom/config>

namespace OpenAxiom {
   // -----------------
   // -- SystemError --
   // -----------------
   // Objects of (type derived from) this type are used to report
   // error orignating from the OpenAxiom core system.
   struct SystemError {
      explicit SystemError(std::string);
      virtual ~SystemError();
      // Return the text of the diagnostic message.
      virtual const std::string& message() const;
   protected:
      const std::string text;
   };

   // Report a file system error
   void filesystem_error(std::string);
   
   namespace Memory {
      // Datatype for the unit of storage.
      typedef unsigned char Byte;

      // Datatype for pointers to data.
      typedef void* Pointer;

      // Precision of the host OS storage page unit in byte count
      size_t page_size();

      // Acquire raw memory from the host OS.
      Pointer os_acquire_raw_memory(size_t);

      // Release raw storage to the hosting OS.  The first operand must
      // be a pointer value previous returned by `os_acquire_raw_memory'.
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
      // This class is a low-level abstraction intented for use
      // to implement higher level storage abstraction.
      struct Storage {
         // Acquire storage chunk of `n' bytes, and align
         // the first allocatable address to `a' booundary.
         // The result is a pointer to a storage object.  That object
         // `result' is constructed such that `result->free' points
         // to the next allocatable address, with alignment `a'.
         static Storage* acquire(size_t a, size_t n);
         
         // Return the storage pointed to by the operand.  It
         // must be a pointer value previously returned by `acquire'.
         // Otherwise, the result is undefined.
         static void release(Storage*);

         // Count of bytes that can fit in this storage.
         size_t capacity() const { return limit_bot - limit_top; }

         // Count of avaliable allocatable bytes in this storage.
         size_t room() const { return limit_bot - free; }

         // Count of allocated storage in this storage.
         size_t occupancy() const { return free - limit_top; }

         // Align next allocatable address to a boundary (operand).
         // Return true on success.
         bool align_to(size_t);

         // Allocate `n' bytes of storage.  It is assumed that prior
         // to calling this function, `n' is less than `room()'.
         // The allocated storage is guaranteed to contain only zeros.
         void* allocate(size_t n) {
            void* result = free;
            free += n;
            return memset(result, 0, n);
         }

         // Next unused address
         void* next_available() { return free; }

         // address at offset `o' from the first allocatable address.
         void* at_offset(size_t o) {
            return limit_top + o;
         }

         // Round up `n' to a multiple of `a', a power of 2.
         static size_t
         round_up(size_t n, size_t a) {
            return (n + a - 1) & ~(a - 1);
         }

         // Next address after `p' in this storage that has alignment `a'.
         void*
         round_up(void* p, size_t a) {
            return base() + round_up(base() - static_cast<Byte*>(p), a);
         }
         
      protected:
         Byte* limit_top;       // first allocatable address
         Byte* limit_bot;       // one-past-the-end of valid allocatable
                                // address in this storage.
         Byte* free;            // first allocatable address suitably
                                // aligned at boundary specified at
                                // construction time.

         Storage() { }

         // Address of the host OS page holding this storage.
         Byte* base() {
            return reinterpret_cast<Byte*>(this);
         }

         size_t extent() {
            return size_t(limit_bot - base());
         }

      private:
         Storage(const Storage&); // not implemented
         Storage& operator=(const Storage&); // idem.
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
      struct Arena {
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
         static T* first_object(Storage* s) {
            return static_cast<T*>
               (s->round_up(&previous(s) + 1, openaxiom_alignment(T)));
         }

         // Address of one-past-the-end object of type `T' in this storage.
         static T* last_object(Storage* s) {
            return static_cast<T*>(s->next_available());
         }

         // Number of objects allocated in a storage.
         static size_t object_count(Storage* s) {
            return last_object(s) - first_object(s);
         }

         // The `previous' link in the chain of storage.
         static Storage*& previous(Storage* s) {
            return *static_cast<Storage**>(s->at_offset(0));
         }

         Storage* store;        // active storage to allocate from

      private:
         // Acquire storage large enough to hold `n' objects of type `T'.
         static Storage* acquire(size_t);
      };

      template<typename T>
      size_t
      Arena<T>::population() const {
         size_t n = 0;
         for (Storage* s = store; s != 0; s = previous(s))
            n += object_count(s);
         return n;
      }

      template<typename T>
      T*
      Arena<T>::allocate(size_t n) {
         const size_t sz = n * sizeof(T);
         if (store->room() < sz) {
            Storage* s = acquire(std::max(n, object_count(store)));
            previous(s) = store;
            store = s;
         }
         return static_cast<T*>(store->allocate(sz));
      }

      template<typename T>
      Arena<T>::Arena(size_t n) : store(acquire(n)) { }
      
      template<typename T>
      Arena<T>::~Arena() {
         // Destroy objects in the reverse order of their
         // their allocation.
         while (store != 0) {
            Storage* current = store;
            store = previous(store);
            Storage::release(current);
         }
      }

      template<typename T>
      Storage*
      Arena<T>::acquire(size_t n) {
         // We build single-linked list of Storage objects, so 
         // don't forget to account for the additional pointer,
         // and necessary padding.
         const size_t sz = n * sizeof(T)
            + Storage::round_up(sizeof(Storage*), openaxiom_alignment(T));
         Storage* s = Storage::acquire(openaxiom_alignment(Storage*), sz);
         s->allocate(sizeof(Storage*));
         previous(s) = 0;
         s->align_to(openaxiom_alignment(T));
         return s;
      }

      // -------------
      // -- Factory --
      // -------------
      template<typename T>
      struct Factory : Arena<T> {
         Factory() : Arena<T>(nominal_population()) { }
         ~Factory();

         // Allocate storage and value-construct an object of type `T'.
         T* make() {
            return new(this->allocate(1)) T();
         }

         // Allocate storage and construct an object of type `T'.
         template<typename U>
         T* make(const U& u) {
            return new(this->allocate(1)) T(u);
         }

         // Allocate storage and construct an object of type `T'.
         template<typename U, typename V>
         T* make(const U& u, const V& v) {
            return new(this->allocate(1)) T(u, v);
         }

         // Allocate storage and construct an object of type `T'.
         template<typename U, typename V, typename W>
         T* make(const U& u, const V& v, const W& w) {
            return new(this->allocate(1)) T(u, v, w);
         }

      private:
         // Return 1 or the number of objects that can fit in a page unit.
         static size_t nominal_population() {
            const size_t overhead =
               Storage::round_up(sizeof(Storage),
                                  openaxiom_alignment(Storage*))
               + Storage::round_up(sizeof(Storage*), openaxiom_alignment(T));
            const size_t psz = page_size();
            if (overhead + sizeof (T) > psz)
               return 1;
            return (psz - overhead) / sizeof(T);
         }
      };

      // Destroy objects in the reverse order of their construction.
      template<typename T>
      Factory<T>::~Factory() {
         for (Storage* s = this->store; s != 0; s = Arena<T>::previous(s)) {
            T* last = Arena<T>::last_object(s);
            for (--last; last >= Arena<T>::first_object(s); --last)
               last->~T();
         }
      }

      // -----------------
      // -- FileMapping --
      // -----------------
      struct FileMapping {
         explicit FileMapping(std::string);
         ~FileMapping();
         const char* begin() const { return static_cast<const char*>(start); }
         const char* end() const { return begin() + extent; }
         std::size_t size() const { return extent; }
      protected:
         Pointer start;         // address at the mapped storage
         size_t extent;         // length (in bytes) of the storage
      private:
         FileMapping(const FileMapping&); // not implemented
         FileMapping& operator=(const FileMapping&); // idem
      };
      
   }
}

#endif  // OPENAXIOM_STORAGE_INCLUDED
