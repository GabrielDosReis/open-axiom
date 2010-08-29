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

// --%: Gabriel Dos Reis.

#include <open-axiom/config>

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
#include <new>                  // for placement new.
#include <open-axiom/storage>

namespace OpenAxiom {
   // ----------------
   // -- SystemError --
   // ----------------
   SystemError::SystemError(std::string s) : text(s) { }

   SystemError::~SystemError() { }

   const std::string&
   SystemError::message() const {
      return text;
   }

   void
   filesystem_error(std::string s) {
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
         if (p == 0)
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
      Storage*
      Storage::acquire(size_t alignment, size_t byte_count) {
         // Adjust for overhead, and page boundary.
         byte_count = round_up(byte_count + sizeof(Storage), page_size());
         Storage* mem = new(os_acquire_raw_memory(byte_count)) Storage;
         mem->limit_top = mem->base() + round_up(sizeof(Storage), alignment);
         mem->limit_bot = mem->base() + byte_count;
         mem->free = mem->limit_top;
         return mem;
      }

      void
      Storage::release(Storage* store) {
         os_release_raw_memory(store, store->extent());
      }

      bool
      Storage::align_to(size_t alignment) {
         if (alignment == 0)    // protect against nuts
            return true;
         if (alignment == 1)    // no preferred alignment at all
            return true;
         Byte* b = base();
         const size_t offset = round_up(free - b, alignment);
         if (offset < size_t(limit_bot - b)) {
            free = b + offset;
            return true;
         }
         return false;          // not enough room left
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

      FileMapping::~FileMapping() {
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
