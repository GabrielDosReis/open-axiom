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


#include "axiom-c-macros.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef __WIN32__
#  include <windows.h>
#endif

/* The basename of the file holding the OpenAxiom core executable.  */
#define OPENAXIOM_CORE_EXECUTABLE \
   "AXIOMsys" OPENAXIOM_EXEEXT

/* The basename of the file holding the session manager executable.  */
#define OPENAXIOM_SMAN_EXECUTABLE \
   "sman" OPENAXIOM_EXEEXT

/* Path to the OpenAxiom executable core, relative to
   OPENAXIOM_ROOT_DIRECTORY, or to the system root directory as specified
   on command line.  */
#define OPENAXIOM_CORE_PATH \
   "/bin/" OPENAXIOM_CORE_EXECUTABLE

/* Path to the session manager, relative to OPENAXIOM_ROOT_DIRECTORY,
   or to the system root directory as specified on command line.  */
#define OPENAXIOM_SMAN_PATH \
   "/bin/" OPENAXIOM_SMAN_EXECUTABLE

/* Return a path to the running system, either as specified on command
   line through --system=, or as specified at configuration time.  */
static const char*
get_systemdir(int argc, char* argv[])
{
   int i;

   /* Root directory specified on command line takes precedence
      over location specified at configuration time.  */
   for (i = 1; i < argc; ++i)
      if (strcmp("--", argv[i]) == 0)
         break;
      else if (strncmp("--system=", argv[i], sizeof("--system=") - 1) == 0) {
         return argv[i] + sizeof ("--system=") - 1;
      }

   /* Command line did not change the system location to use.
      Return what was computed at configuration time.  */
   return OPENAXIOM_ROOT_DIRECTORY;
}


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


/* Return a path for PROG specified as a relative path to PREFIX.  */
static const char*
make_path_for(const char* prefix, const char* prog)
{
   const int prefix_length = strlen(prefix);
   char* execpath = (char*) malloc(prefix_length + strlen(prog) + 1);
   strcpy(execpath, prefix);
   strcpy(execpath + prefix_length, prog);
   return execpath;
}


int
main(int argc, char* argv[])
{
   const char* root_dir = get_systemdir(argc, argv);
   const char* execpath =
      make_path_for(root_dir,
                    OPENAXIOM_USE_SMAN ?
         OPENAXIOM_SMAN_PATH : OPENAXIOM_CORE_PATH);
#ifdef __WIN32__
   char* command_line;
   int cur = strlen(argv[0]);
   int command_line_length = 0;
   int i;
   PROCESS_INFORMATION procInfo;
   STARTUPINFO startupInfo = { 0 };
   DWORD status;                /* Exit code for this program masqueraded as
                                   the child created below.  */

   command_line_length += cur;
   for (i = 1; i < argc; ++i)
      command_line_length += 1  /* blank char as separator */
         + strlen(argv[i]);     /* room for each argument */

   /* Don't forget room for the doubledash string.  */
   command_line_length += sizeof("--") - 1;

   command_line = (char*) malloc(command_line_length + 1);

   strcpy(command_line, argv[0]);
   command_line[++cur] = ' ';

   /* Now start arguments to the core executable.  */
   command_line[++cur] = '-';
   command_line[++cur] = '-';

   /* Concatenate the arguments into a single string.  */
   for (i = 1; i < argc; ++i) {
      const int arg_length = strlen(argv[i]);
      command_line[++cur] = ' ';
      /* Note that strcpy will terminate `command_line' with a NUL
         character, and since the next iteration will write the
         blank precisely where the NUL character is, the whole command
         line string will be a proper C-style string when the loop
         normally exits.  */
      strcpy(command_line + cur, argv[i]);
      cur += arg_length;
   }
         
   publish_systemdir(root_dir);
   if(CreateProcess(/* lpApplicationName */ execpath,
                    /* lpCommandLine */ command_line,
                    /* lpProcessAttributes */ NULL,
                    /* lpThreadAttributes */ NULL,
                    /* bInheritHandles */ TRUE,
                    /* dwCreationFlags */ 0,
                    /* lpEnvironment */ NULL,
                    /* lpCurrentDirectory */ NULL,
                    /* lpstartupInfo */ &startupInfo,
                    /* lpProcessInformation */ &procInfo) == 0) {
      fprintf(stderr, GetLastError());
      abort();
   }
   WaitForSingleObject(procInfo.hProcess, INFINITE);
   GetExitCodeProcess(procInfo.hProcess, &status);
   CloseHandle(procInfo.hThread);
   CloseHandle(procInfo.hProcess);
   return status;
                        
#else  /* __WIN32__ */
   int i;
   char** args = (char**) malloc(sizeof (char*) * (argc + 2));
   publish_systemdir(root_dir);

   /* Pretend that we are still running the OpenAxiom driver, even if
      it is actually the session manager or the core executable running.
      We don't want to expose implementation details to users.  */
   args[0] = argv[0];
   /* If we are running the core executable, we need to insert a
      doubledash to indicate beginning of arguments.  The session manager
      does not need that temporary necessary obfuscation, and will
      be confused.  */
   if (!OPENAXIOM_USE_SMAN)
      args[1] = "--";
   for (i = 1; i < argc; ++i)
      args[i + !OPENAXIOM_USE_SMAN] = argv[i];
   args[argc + !OPENAXIOM_USE_SMAN] = NULL;

   execv(execpath, args);
   perror(strerror(errno));
   return -1;
#endif /* __WIN32__ */
}
