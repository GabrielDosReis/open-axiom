/*
   Copyright (C) 2008-2010, Gabriel Dos Reis.
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
   as the session manager.  */


#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <locale.h>

#include "cfuns.h"
#include "open-axiom.h"

#define OPENAXIOM_GLOBAL_ENV   "AXIOM"

namespace OpenAxiom {

   // Publish the system exec prefix for use by sub-processes.
   static void
   publish_systemdir(const char* dir)
   {
      if (!oa_setenv(OPENAXIOM_GLOBAL_ENV, dir)) {
         perror("publish_systemdir");
         abort();
      }
   }
   
   static void
   augment_variable(const char* name, const char* value) {
      const char* oldval = oa_getenv(name);
      const int value_length = strlen(value);
      const int oldval_length = oldval == 0 ? 0 : strlen(oldval);
      const int newval_length = value_length + 1 + oldval_length;
      char* newval = (char*) malloc(newval_length + 1);
      
      strcpy(newval,value);
      if (oldval != 0) {
         newval[value_length] = openaxiom_ifs;
         strcpy(newval + value_length + 1, oldval);
      }
      
      if (!oa_setenv(name, newval))
         perror("oa_augment_environment_variable");
   }

   static void
   upgrade_environment(const Command* command) {
      const char* sysdir = command->root_dir;
      augment_variable("TEXINPUTS",
                       oa_concatenate_string(sysdir, OPENAXIOM_TEXINPUTS_PATH));
      augment_variable("BIBINPUTS",
                       oa_concatenate_string(sysdir, OPENAXIOM_BIBINPUTS_PATH));
      const char* ldd_path = option_value(command, "--syslib");
      if (ldd_path == 0)
         ldd_path = oa_concatenate_string(sysdir, "/lib");
#ifdef OPENAXIOM_MS_WINDOWS_HOST
      augment_variable("PATH", ldd_path);
#else      
      augment_variable("LD_LIBRARY_PATH", ldd_path);
#endif      
      publish_systemdir(sysdir);
   }

   // Print configuration info.
   static int
   print_configuration_info(Command* command) {
      int i;
      for (i = 1; i < command->core.argc; ++i) {
         if (strcmp(command->core.argv[i], "include") == 0)
            printf("\"%s\"/include ", command->root_dir);
         else if (strcmp(command->core.argv[i], "lib") == 0)
            printf(" -L\"%s\"/lib -lOpenAxiom", command->root_dir);
         else {
            fprintf(stderr, "open-axiom: invalid request %s\n",
                    command->core.argv[i]);
            return 1;
         }
      }
      fflush(stdout);
      return 0;
   }
}

int
main(int argc, char* argv[])
{
   using namespace OpenAxiom;
   Command command;
   Driver driver = preprocess_arguments(&command, argc, argv);
   upgrade_environment(&command);

   switch (driver) {
   case null_driver:
      return 0;                 /* Bye.  */

   case config_driver:
      return print_configuration_info(&command);

   case core_driver:
   case script_driver:
   case compiler_driver:
   case translator_driver:
   case linker_driver:
   case gui_driver:
      putenv("LC_ALL=C");
      setlocale(LC_ALL, "");
      return execute_core(&command, driver);

   case execute_driver:
      return oa_spawn(&command.core,
                      SpawnFlags(spawn_search_path | spawn_replace));

   case sman_driver:
      break;

   default:
      abort();
   }

#ifdef __WIN32__
   /* Should not happen on MS platforms.  */
   abort();
#else  /* __WIN32__ */
   execv(make_path_for(command.root_dir, sman_driver), argv);
   perror(strerror(errno));
   return -1;
#endif /* __WIN32__ */
}
