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

#include "openaxiom-c-macros.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "utils.h"
#include <stdio.h>

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

/* Name of the entry point for Lisp-based OpenAxiom.  */
#define OPENAXIOM_LISP_CORE_ENTRY_POINT \
   "|AxiomCore|::|topLevel|"


/* Return a path to the running system, either as specified on command
   line through --system=, or as specified at configuration time.  */
const char*
openaxiom_get_systemdir(int argc, char* argv[])
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

/* Return the path to `driver'. */
static const char*
get_driver_name(openaxiom_driver driver)
{
   switch (driver) {
   case openaxiom_sman_driver:
      return OPENAXIOM_SMAN_PATH;

   case openaxiom_script_driver:
   case openaxiom_compiler_driver:
   case openaxiom_core_driver:
      return OPENAXIOM_CORE_PATH;

   default:
      abort();
   }
}


/* Return a path for PROG specified as a relative path to PREFIX.  */
const char*
openaxiom_make_path_for(const char* prefix, openaxiom_driver driver)
{
   const int prefix_length = strlen(prefix);
   const char* prog = get_driver_name(driver);
   char* execpath = (char*) malloc(prefix_length + strlen(prog) + 1);
   strcpy(execpath, prefix);
   strcpy(execpath + prefix_length, prog);
   return execpath;
}

/* Build arguments, if any, to be supplied to the runtime system
   of `driver'.  */
void
openaxiom_build_rts_options(openaxiom_command* command,
                            openaxiom_driver driver)
{
   switch (driver) {
   case openaxiom_sman_driver:
   case openaxiom_unknown_driver:
      break;

   case openaxiom_core_driver:
      break;

   case openaxiom_compiler_driver:
   case openaxiom_script_driver:
      switch (OPENAXIOM_BASE_RTS) {
      case openaxiom_gcl_runtime:
         command->rt_argc = 3;
         command->rt_argv = (char **)
            malloc(command->rt_argc * sizeof (char*));
         command->rt_argv[0] = "-batch";
         command->rt_argv[1] = "-eval";
         command->rt_argv[2] = "(" OPENAXIOM_LISP_CORE_ENTRY_POINT ")";
         break;

      case openaxiom_sbcl_runtime:
         command->rt_argc = 3;
         command->rt_argv = (char **)
            malloc(command->rt_argc * sizeof (char*));
         command->rt_argv[0] = "--noinform";
         command->rt_argv[1] = "--end-runtime-options";
         command->rt_argv[2] = "--noprint";
         command->rt_argv[3] = "--end-toplevel-options";
         break;
         
      default:
         abort();
      }
      break;

   default:
      abort();
   }
}

#ifdef __WIN32__
#  define OPENAXIOM_DEFAULT_DRIVER openaxiom_core_driver
#elif OPENAXIOM_USE_SMAN
#  define OPENAXIOM_DEFAULT_DRIVER openaxiom_sman_driver
#else
#  define OPENAXIOM_DEFAULT_DRIVER openaxiom_unknown_driver
#endif


/* Determine driver to be used for executing `command'.  */
openaxiom_driver
openaxiom_preprocess_arguments(openaxiom_command* command,
                               int argc, char** argv)
{
   int i;
   openaxiom_driver driver = OPENAXIOM_DEFAULT_DRIVER;

   command->core_argc = argc;
   command->core_argv = argv;
   command->root_dir = openaxiom_get_systemdir(argc, argv);
   for (i = 1; i < argc; ++i)
      if (strcmp(argv[i], "--script") == 0)
         driver = openaxiom_script_driver;
      else if(strcmp(argv[i], "--compile") == 0)
         driver = openaxiom_compiler_driver;
      else if(strcmp(argv[i], "--no-server") == 0)
         driver = openaxiom_core_driver;
      else if (strcmp(argv[i], "--server") == 0)
         driver = openaxiom_sman_driver;
   

   openaxiom_build_rts_options(command, driver);
   return driver;
}

   


/* Execute the Core Executable as described by `command'.  On
   POSIX systems, this is a non-return function on success.
   See execv().  */
int
openaxiom_execute_core(const openaxiom_command* command,
                       openaxiom_driver driver)
{
   const char* execpath =
      openaxiom_make_path_for(command->root_dir, driver);
#ifdef __WIN32__
   char* command_line;
   int cur = strlen(command->core_argv[0]);
   int command_line_length = 0;
   int i;
   PROCESS_INFORMATION procInfo;
   STARTUPINFO startupInfo = { 0 };
   DWORD status;                /* Exit code for this program masqueraded as
                                   the child created below.  */

   /* How long is the final command line for the MS system? */
   command_line_length += cur;
   for (i = 0; i < command->rt_argc; ++i)
      command_line_length += 1  /* blank char as separator */
	 + 2			/* quotes around every argument.  */
         + strlen(command->rt_argv[i]); /* room for each argument */
   /* Don't forget room for the doubledash string.  */
   command_line_length += sizeof("--") - 1;
   /* And arguments to the actual command.  */
   for (i = 1; i < command->core_argc; ++i)
      command_line_length += 1 + 2 + strlen(command->core_argv[i]);

   /* Now, build the actual command line.  This is done by
      concatenating the arguments into a single string. */
   command_line = (char*) malloc(command_line_length + 1);
   strcpy(command_line, command->core_argv[0]);
   for (i = 0; i < command->rt_argc; ++i) {
      const int arg_length = strlen(command->rt_argv[i]);
      command_line[cur++] = ' ';
      command_line[cur++] = '"';
      strcpy(command_line + cur, command->rt_argv[i]);
      cur += arg_length;
      command_line[cur++] = '"';
   }
   command_line[cur++] = ' ';
   command_line[cur++] = '-'; /*  start arguments to the core executable.  */
   command_line[cur++] = '-';
   for (i = 1; i < command->core_argc; ++i) {
      const int arg_length = strlen(command->core_argv[i]);
      command_line[cur++] = ' ';
      command_line[cur++] = '"';
      strcpy(command_line + cur, command->core_argv[i]);
      cur += arg_length;
      command_line[cur++] = '"';
   }
   command_line[cur] = '\0';	/* The command line is done.  */

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
      fprintf(stderr, "error %d\n", GetLastError());
      abort();
   }
   WaitForSingleObject(procInfo.hProcess, INFINITE);
   GetExitCodeProcess(procInfo.hProcess, &status);
   CloseHandle(procInfo.hThread);
   CloseHandle(procInfo.hProcess);
   return status;
                        
#else  /* __WIN32__ */
   int i;
   char** args = (char**)
      malloc(sizeof (char*) * (command->rt_argc + command->core_argc + 2));
   args[0] = command->core_argv[0];
   for (i = 0; i < command->rt_argc; ++i)
      args[i + 1] = command->rt_argv[i];
   if (command->core_argc > 1) {
      /* Insert a doubledash to indicate beginning of arguments.  */
      args[command->rt_argc + 1] = "--";
      for (i = 1; i < command->core_argc; ++i)
         args[command->rt_argc + i + 1] = command->core_argv[i];
      args[command->rt_argc + command->core_argc + 1] = NULL;
   }
   else
      args[command->rt_argc + command->core_argc] = NULL;

   execv(execpath, args);
   perror(strerror(errno));
   return -1;
#endif /* __WIN32__ */
}
