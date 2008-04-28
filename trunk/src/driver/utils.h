/*
   Copyright (C) 2008, Gabriel Dos Reis.
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

#ifndef OPENAXIOM_UTILS_INCLUDED
#define OPENAXIOM_UTILS_INCLUDED

#include "openaxiom-c-macros.h"
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef __WIN32__
#  include <windows.h>
#endif

/* A list of drivers for OpenAxiom.  */
typedef enum openaxiom_driver {
   openaxiom_unknown_driver,
   openaxiom_sman_driver,
   openaxiom_core_driver,
   openaxiom_script_driver,
   openaxiom_compiler_driver
} openaxiom_driver;

/* A list of runtime support systems for OpenAxiom. */
typedef enum openaxiom_runtime {
   openaxiom_unknown_runtime,
   openaxiom_gcl_runtime,
   openaxiom_sbcl_runtime,
   openaxiom_clisp_runtime,
   openaxiom_ecl_runtime,
   openaxiom_bemol_runtime
} openaxiom_runtime;

/* A description of external command to be executed.  */
typedef struct openaxiom_command {
   char** core_argv;            /* arguments for the actual executable. */
   int core_argc;               /* number of such arguments. */
   char** rt_argv;              /* arguments to the base RT, if any.  */
   int rt_argc;                 /* number of such arguments.  */
   const char* root_dir;        /* path to the OpenAxiom system. */
} openaxiom_command;

const char* openaxiom_get_systemdir(int argc, char*[]);
const char* openaxiom_make_path_for(const char*, openaxiom_driver);

int openaxiom_execute_core(const openaxiom_command*, openaxiom_driver);
void openaxiom_build_rts_options(openaxiom_command*, openaxiom_driver);

#endif /* OPENAXIOM_UTILS_INCLUDED */
