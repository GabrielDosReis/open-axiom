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

// --%: Gabriel Dos Reis.

#include <open-axiom/storage>
#ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
#  include <sys/stat.h>
#endif
#include <sys/types.h>
#ifdef HAVE_FCNTL_H
#  include <fcntl.h>
#endif
#ifdef HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#  include <sys/mman.h>
#endif
#ifdef OPENAXIOM_MS_WINDOWS_HOST
#  include <windows.h>
#endif
#include <errno.h>
#include <stdlib.h>
#include <string.h>

namespace OpenAxiom {
   // ----------------
   // -- SystemError --
   // ----------------
   SystemError::SystemError(const std::string& s) : text(s) { }

   SystemError::~SystemError() { }

   const std::string&
   SystemError::message() const {
      return text;
   }

   void
   filesystem_error(const std::string& s) {
      throw SystemError(s);
   }

   namespace Memory {
      // Return storage page allocation unit in byte count.
      size_t page_size() {
#if defined(OPENAXIOM_MS_WINDOWS_HOST)
         SYSTEM_INFO si = { };
         GetSystemInfo(&si);
         return si.dwPageSize;
#elif defined(HAVE_UNISTD_H)
         return sysconf(_SC_PAGESIZE);
#else
         // Well, we have to return a number.
         return 4096;
#endif         
      }

      // Subroutine of os_acquire_raw_memory.  Attempt to acquire
      // storage from the host OS.  Return null on failure.
      static inline Pointer
      os_allocate_read_write_raw_memory(size_t nbytes) {
#if defined(OPENAXIOM_MS_WINDOWS_HOST)
         return VirtualAlloc(Pointer(), nbytes,
                             MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
#elif defined(HAVE_SYS_MMAN_H)
         Pointer p = mmap(Pointer(), nbytes, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | OPENAXIOM_MM_ANONYMOUS_MAP_FLAG,
                          -1, 0);
         return p == MAP_FAILED ? Pointer() : p;
#else         
         return malloc(byte_count);
#endif         
      }

      Pointer
      os_acquire_raw_memory(size_t nbytes) {
         Pointer p = os_allocate_read_write_raw_memory(nbytes);
         if (p == nullptr)
            throw SystemError("cannot acquire more memory");
         return memset(p, nbytes, 0);
      }

      void
      os_release_raw_memory(Pointer p, size_t n) {
#if defined(OPENAXIOM_MS_WINDOWS_HOST)
         VirtualFree(p, 0, MEM_RELEASE);
#elif defined(HAVE_SYS_MMAN_H)
         munmap(p, n);
#else
         free(p);
#endif            
      }

      // -------------
      // -- Storage --
      // -------------
      struct Storage::Handle {
         size_t extent;         // count of allocated bytes
         void* start;           // beginning of usable address.
      };

      static inline Pointer
      storage_end(Storage::Handle* h) {
         return Storage::byte_address(h) + h->extent;
      }

      // Acquire storage chunk of at least `n' bytes.
      // The result is a pointer to a storage object.  That object
      // `result' is constructed such that `begin(result)' points
      // to the next allocatable address.
      template<typename T>
      static T*
      acquire_storage_with_header(size_t n) {
         n = Storage::round_up(n, page_size());
         T* h = static_cast<T*>(os_acquire_raw_memory(n));
         h->extent = n;
         h->start = h + 1;
         return h;
      }

      void
      Storage::release(Handle* h) {
         os_release_raw_memory(h, h->extent);
      }

      Pointer
      Storage::begin(Handle* h) {
         return h->start;
      }

      // -------------------------
      // -- SinglyLinkedStorage --
      // -------------------------
      struct OneWayLinkHeader : Storage::Handle {
         Handle* previous;
      };
      
      SinglyLinkedStorage::Handle*&
      SinglyLinkedStorage::previous(Handle* h) {
         return static_cast<OneWayLinkHeader*>(h)->previous;
      }

      // -------------------------
      // -- DoublyLinkedStorage --
      // -------------------------
      struct TwoWayLinkHeader : Storage::Handle {
         Handle* previous;
         Handle* next;
      };

      static inline TwoWayLinkHeader*
      two_way_link(Storage::Handle* h) {
         return static_cast<TwoWayLinkHeader*>(h);
      }

      DoublyLinkedStorage::Handle*&
      DoublyLinkedStorage::previous(Handle* h) {
         return two_way_link(h)->previous;
      }

      DoublyLinkedStorage::Handle*&
      DoublyLinkedStorage::next(Handle* h) {
         return two_way_link(h)->next;
      }

      DoublyLinkedStorage::Handle*
      DoublyLinkedStorage::acquire(size_t n, size_t a) {
         // Add enough padding space for specified alignment.
         const size_t overhead = round_up(sizeof (TwoWayLinkHeader), a);
         TwoWayLinkHeader* h =
            acquire_storage_with_header<TwoWayLinkHeader>(overhead + n);
         h->start = byte_address (h) + overhead;
         h->previous = nullptr;
         h->next = nullptr;
         return h;
      }

      // ------------------
      // -- BlockStorage --
      // ------------------
      struct BlockHeader : OneWayLinkHeader {
         Byte* available;
      };

      static inline BlockHeader*
      block_header(BlockStorage::Handle* h) {
         return static_cast<BlockHeader*>(h);
      }

      BlockStorage::Handle*
      BlockStorage::acquire(size_t n, size_t a) {
         const size_t overhead = round_up(sizeof (BlockHeader), a);
         BlockHeader* h =
            acquire_storage_with_header<BlockHeader>(overhead + n);
         // Remember the next available address to allocate from.
         h->available = byte_address(h) + overhead;
         // That is also where the actual object storage starts.
         h->start = h->available;
         h->previous = nullptr;
         return h;
      }

      Pointer
      BlockStorage::next_address(Handle* h) {
         return block_header(h)->available;
      }

      size_t
      BlockStorage::room(Handle* h) {
         return byte_address(storage_end(h)) - block_header(h)->available;
      }

      Pointer
      BlockStorage::book(Handle* h, size_t n) {
         BlockHeader* block = block_header(h);
         void* const p = block->available;
         block->available += n;
         return p;
      }
      

      // -----------------
      // -- FileMapping --
      // -----------------
      FileMapping::FileMapping(std::string path)
            : start(), extent() {
#if defined(OPENAXIOM_MS_WINDOWS_HOST)
         HANDLE file = CreateFile(path.c_str(), GENERIC_READ, 0, 0,
                                  OPEN_EXISTING,
                                  FILE_ATTRIBUTE_NORMAL, 0);
         if (file == INVALID_HANDLE_VALUE)
            filesystem_error("could not access file " + path);
         HANDLE mapping = CreateFileMapping(file, 0, PAGE_READONLY, 0, 0, 0);
         if (mapping == 0)
            filesystem_error("could not map file " + path);
         start = MapViewOfFile(mapping, FILE_MAP_READ, 0, 0, 0);
         extent = GetFileSize(file, 0);
         CloseHandle(mapping);
         CloseHandle(file);
#elif defined(HAVE_SYS_STAT_H) && defined(HAVE_SYS_MMAN_H) && defined(HAVE_FCNTL_H)
         struct stat s;
         errno = 0;
         if (stat(path.c_str(), &s) < 0)
            filesystem_error("could not access file " + path);
         else if (!S_ISREG(s.st_mode))
            filesystem_error(path + " is not a regular file");
         int fd = open(path.c_str(), O_RDONLY);
         if (fd < 0)
            filesystem_error("could not open " + path);
         start = mmap(Pointer(), s.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
         close(fd);
         if (start == MAP_FAILED)
            filesystem_error("could not map file " + path);
         extent = s.st_size;
#else
#  error "Don't know how to map a file on this platform"         
#endif  // OPENAXIOM_MS_WINDOWS_HOST
      }

      FileMapping::FileMapping(FileMapping&& f)
            : start(f.start), extent(f.extent) {
         f.start = nullptr;
         f.extent = 0;
      }

      FileMapping::~FileMapping() {
         if (start != nullptr) {
#if defined(OPENAXIOM_MS_WINDOWS_HOST)
            UnmapViewOfFile(start);
#elif defined(HAVE_SYS_MMAN_H)
            munmap(start, extent);
#else
#  error "Don't know how to unmap a file on this platform"
#endif
         }
      }
   }
}
