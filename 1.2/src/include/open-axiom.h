/*
  Copyright (C) 2007-2008, Gabriel Dos Reis.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

      - Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

      - Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.

      - Neither the name of The Numerical Algorithms Group Ltd. nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef OPENAXIOM_included
#define OPENAXIOM_included

#include "openaxiom-c-macros.h"

/* Cope with MS-platform oddities.  */
#ifdef __WIN32__
#  ifdef  DLL_EXPORT
#    define OPENAXIOM_EXPORT  __declspec(dllexport)
#  elif defined(OPENAXIOM_DLL_IMPORT)
#    define OPENAXIOM_EXPORT  __declspec(dllimport)
#  endif  /* DLL_EXPORT */
#endif	/* __WIN32__ */
#ifndef OPENAXIOM_EXPORT
#  define OPENAXIOM_EXPORT  /* nothing */
#endif /* OPENAXIOM_EXPORT */

#if defined(HAVE_STDINT_H)
#  include <stdint.h>
#elif defined (HAVE_INTTYPES_H)
#  include <inttypes.h>
#endif
typedef uint8_t openaxiom_byte;

/* The opaque datatype.  */
#ifdef __WIN32__
#include <windows.h>
typedef HANDLE openaxiom_handle;
#else
typedef void* openaxiom_handle;
#endif

#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif   

/* Do we have graphics support?  */
#ifdef X_DISPLAY_MISSING
#  define OPENAXIOM_HAVE_GRAPHICS 0
#else
#  define OPENAXIOM_HAVE_GRAPHICS 1
#endif


/* Return the address of the data buffer `BUF'.  */

#define oa_buffer_address(BUF) ((openaxiom_byte*)&BUF[0])


/* The function sleep() is not available under Windows.  Instead, they
   have Sleep(); with capital S, please.  Furthermore, it does not
   take argument in second, but in milliseconds, three order
   of magnitude of difference when compared to the Unix world.
   We abstract over that difference here.  */

static inline void
openaxiom_sleep(int n)
{
#ifdef __WIN32__
   Sleep(n * 1000);
#else
   sleep(n);
#endif   
}


#ifdef __cplusplus
}
#endif   
   
#endif /* OPENAXIOM_included */
