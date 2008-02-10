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

/* This program is a driver for the OpenAxiom core executable.
   It pretends to be the OpenAxiom interpreter when, in fact, the actual
   work is done by the Core Executable.  It also occasionally masquerades
   as the seesion manager.  */


#include "utils.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>


#define OPENAXIOM_GLOBAL_ENV   "AXIOM"

/* Publish the system exec prefix for use by sub-processes.  */
static void
publish_systemdir(const char* dir)
{
#ifdef __WIN32__
   if (SetEnvironmentVariable(OPENAXIOM_GLOBAL_ENV, dir) == 0) {
      perror("SetEnvironmentVariable");
      abort();
   }
#else  /* __WIN32__ */
   const int env_length = sizeof (OPENAXIOM_GLOBAL_ENV)
      + 1                       /* room for '='  */
      + strlen(dir);
   char* env = (char*) malloc (env_length);
   strcpy(env, OPENAXIOM_GLOBAL_ENV);
   env[sizeof OPENAXIOM_GLOBAL_ENV - 1] = '=';
   strcpy(env + sizeof(OPENAXIOM_GLOBAL_ENV), dir);
   if (putenv(env) != 0) abort();
#endif /* __WIN32__ */
}


int
main(int argc, char* argv[])
{
   openaxiom_command command = { };
   openaxiom_driver driver =
      openaxiom_preprocess_arguments(&command, argc, argv);

   switch (driver) {
   case openaxiom_core_driver:
   case openaxiom_script_driver:
   case openaxiom_compiler_driver:
      return openaxiom_execute_core(&command, driver);

   case openaxiom_sman_driver:
      break;

   default:
      abort();
   }

#ifdef __WIN32__
   /* Should not happen on MS platforms.  */
   abort();
#else  /* __WIN32__ */
   publish_systemdir(command.root_dir);
   execv(openaxiom_make_path_for(command.root_dir, openaxiom_sman_driver),
         argv);
   perror(strerror(errno));
   return -1;
#endif /* __WIN32__ */
}
