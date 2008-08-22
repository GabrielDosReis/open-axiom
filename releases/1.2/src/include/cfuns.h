#ifndef OPENAXIOM_CFUNS_included
#define OPENAXIOM_CFUNS_included

/*
  Copyright (C) 1991-2002, The Numerical ALgorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis.
  All rights resrved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

      - Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

      - Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.

      - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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

#include "open-axiom.h"

OPENAXIOM_EXPORT int addtopath(char*);
OPENAXIOM_EXPORT int directoryp(char*);
OPENAXIOM_EXPORT int make_path_from_file(char*, char*);
OPENAXIOM_EXPORT int writeablep(char*);
OPENAXIOM_EXPORT int readablep(char*);
OPENAXIOM_EXPORT long findString(char*, char*);
OPENAXIOM_EXPORT int copyEnvValue(char*, char*);
OPENAXIOM_EXPORT int oa_chdir(const char*);
OPENAXIOM_EXPORT int oa_unlink(const char*);
OPENAXIOM_EXPORT int oa_rename(const char*, const char*);
OPENAXIOM_EXPORT int oa_mkdir(const char*);
OPENAXIOM_EXPORT int oa_system(const char*);
OPENAXIOM_EXPORT char* oa_getenv(const char*);
OPENAXIOM_EXPORT char* oa_getcwd(void);
OPENAXIOM_EXPORT int oa_access_file_for_read(const char*);
OPENAXIOM_EXPORT const char* oa_get_tmpdir(void);
OPENAXIOM_EXPORT double plus_infinity(void);
OPENAXIOM_EXPORT double minus_infinity(void);
OPENAXIOM_EXPORT double NANQ(void);

#endif /* OPENAXIOM_CFUNS_included */
