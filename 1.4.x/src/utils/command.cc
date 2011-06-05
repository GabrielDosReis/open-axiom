/*
   Copyright (C) 2008-2011, Gabriel Dos Reis.
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

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include "open-axiom.h"

namespace OpenAxiom {

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

#define OPENAXIOM_GUI_SUBPATH \
   "/bin/" OPENAXIOM_GUI_EXECUTABLE

#define OPENAXIOM_GUI_EXECUTABLE \
   "gui" OPENAXIOM_EXEEXT

/* Path to the session manager, relative to OPENAXIOM_ROOT_DIRECTORY,
   or to the system root directory as specified on command line.  */
#define OPENAXIOM_SMAN_PATH \
   "/bin/" OPENAXIOM_SMAN_EXECUTABLE

/* Name of the entry point for Lisp-based OpenAxiom.  */
#define OPENAXIOM_LISP_CORE_ENTRY_POINT \
   "|AxiomCore|::|topLevel|"

   // -- Arguments --
   Arguments::Arguments(int n) : std::vector<char*>(n > 0 ? n + 1 : 0)
   { }

   int Arguments::size() const {
      return empty() ? 0 : std::vector<char*>::size() - 1;
   }
   
   void Arguments::allocate(int n) {
      resize(n + 1);
   }

   char* const* Arguments::data() const {
      return &*begin();
   };

   // -- Command --
   Command::Command()
         : core(),
           rt_args(),
           root_dir(),
           exec_path()
   { }

   static const char*
   get_suffix(const char* first, const char* last, const char* seq) {
      for (; first < last; ++first, ++seq)
         if (*first != *seq)
            return 0;
      return seq;
   }
   
   // -- Return non-null if `lhs' is a prefix of `rhs'.  When non-null
   // -- the pointer points to the '=' character that starts of the
   // -- value supplied to the argument.
   template<int N>
   const char* is_prefix(const char (&lhs)[N], const char* rhs) {
      for (int i = 0; i < N - 1; ++i)
         if (lhs[i] != rhs[i])
            return 0;
      return rhs + N - 1;
   }

   const char*
   option_value(const Command* command, const char* opt) {
      const int n = strlen(opt);
      for (int i = 1; i < command->core.argc; ++i) {
         const char* arg = command->core.argv[i];
         if (strlen(arg) < n)
            continue;
         if(const char* val = get_suffix(opt, opt + n, arg)) {
            if (*val++ == '=')
               return val;
            break;
         }
      }
      return 0;
   }
   
/* Return a path to the running system, either as specified on command
   line through --system=, or as specified at configuration time.  */
const char*
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

/* Return the path to `driver'. */
static const char*
get_driver_subpath(Driver driver)
{
   switch (driver) {
   case sman_driver:
      return OPENAXIOM_SMAN_PATH;

   case gui_driver:
      return OPENAXIOM_GUI_SUBPATH;

   case script_driver:
   case compiler_driver:
   case core_driver:
   case translator_driver:
   case linker_driver:
      return OPENAXIOM_CORE_PATH;

   default:
      abort();
   }
}


/* Return a path for PROG specified as a relative path to PREFIX.  */
const char*
make_path_for(const char* prefix, Driver driver)
{
   const int prefix_length = strlen(prefix);
   const char* prog = get_driver_subpath(driver);
   char* execpath = (char*) malloc(prefix_length + strlen(prog) + 1);
   strcpy(execpath, prefix);
   strcpy(execpath + prefix_length, prog);
   return execpath;
}

/* Build arguments, if any, to be supplied to the runtime system
   of `driver'.  */
void
build_rts_options(Command* command, Driver driver)
{
   switch (driver) {
   case config_driver:
   case sman_driver:
   case gui_driver:
   case execute_driver:
   case unknown_driver:
      break;

   case core_driver:
      break;

   case compiler_driver:
   case script_driver:
   case translator_driver:
   case linker_driver:
      switch (OPENAXIOM_BASE_RTS) {
      case gcl_runtime:
         command->rt_args.allocate(3);
         command->rt_args[0] = (char*) "-batch";
         command->rt_args[1] = (char*) "-eval";
         command->rt_args[2] =
            (char*) ("(" OPENAXIOM_LISP_CORE_ENTRY_POINT ")");
         break;

      case sbcl_runtime:
         command->rt_args.allocate(4);
         command->rt_args[0] = (char*) "--noinform";
         command->rt_args[1] = (char*) "--end-runtime-options";
         command->rt_args[2] = (char*) "--noprint";
         command->rt_args[3] = (char*) "--end-toplevel-options";
         break;

      case clozure_runtime:
         command->rt_args.allocate(2);
         command->rt_args[0] = (char*) "--quiet";
         command->rt_args[1] = (char*) "--batch";
         break;

      case clisp_runtime:
         command->rt_args.allocate(2);
         command->rt_args[0] = (char*) "--quiet";
         command->rt_args[1] = (char*) "-norc";
         break;
         
      case ecl_runtime:
         command->rt_args.allocate(2);
         command->rt_args[0] = (char*) "-q";
         command->rt_args[1] = (char*) "-norc";
         break;

      default:
         abort();
      }
      break;

   default:
      abort();
   }
}

// Return a description of the driver to invokve by default.
static Driver default_driver(bool explicit_no_gui) {
   if (OPENAXIOM_USE_SMAN)
      return sman_driver;
   else if (OPENAXIOM_USE_GUI and not explicit_no_gui)
      return gui_driver;
   else
      return core_driver;
}


static void print_line(const char* line) {
   fputs(line, stdout);
   fputc('\n', stdout);
}

/* Print OpenAxiom version information.  */
static void print_version(void) {
   print_line(PACKAGE_STRING);
}

/* Print OpenAxiom invokation syntax (e.g. options) on standard
   output stream.  */

static void print_usage(void) {
   print_line("Usage: open-axiom [options] [file]");
   print_line("General options:");
   print_line("  --help           Print this information and exit.");
   print_line("  --version        Print OpenAxiom version and exit.");
   print_line("  --script         Execute the file argument as a Spad script.");
   print_line("                   If specified, this option should be last before file argument.");
   print_line("  --compile        Invoke the compiler on the file argument.");
   print_line("                   If specified, this option should be last before file argument.");
   print_line("  --server         Start the Superman as master process.");
   print_line("  --no-server      Do not start Superman as master process.");
   print_line("");
   print_line("Superman options:");
   print_line("  --no-gui         Do not start the Graphics or HyperDoc components.");
   print_line("  --graph          Start the Graphics component.  This option is meaningful");
   print_line("                   only if OpenAxiom was built with graphics support.");
   print_line("  --no-graph       Do not start the Graphics component.");
   print_line("  --hyperdoc       Start the HyperDoc component.  This option is meaningful");
   print_line("                   only if OpenAxiom was built with graphics support.");
   print_line("  --no-hyperdoc    Do not start the HyperDoc component.");
   print_line("  --execute cmd args  execute `cmd' with arguments `args'");
              
   print_line("");
   print_line("Compiler options:");
   print_line("  --optimize=<n>   Set compiler optimization level to <n>, a natural number.");
   print_line("");
   print_line("If invoked without options and without an input file "
              "OpenAxiom will start as an interative program with Superman"
              " as the master process, the majority of uses.  If invoked "
              "with a file as single argument, OpenAxiom assumes the file is a Spad "
              "script and will attempt to execute it as such.");
   print_line("");
   print_line("Submit bug report to " PACKAGE_BUGREPORT);
}

   // Map a option to the driver that implement that action.
   struct DriverMap {
      const char* action;
      const Driver driver;
   };

   static const DriverMap driver_table[] = {
      { "--script", script_driver },
      { "--compile", compiler_driver },
      { "--translate", compiler_driver },
      { "--build-databases", compiler_driver },
      { "--make", linker_driver },
   };

   // Obtain the driver that implement a specific action requested
   // on command line.
   static Driver
   option_driver(const char* opt) {
      for (int i = 0; i < length(driver_table); ++i)
         if (strcmp(opt, driver_table[i].action) == 0)
            return driver_table[i].driver;
      return unknown_driver;
   }
   
/* Determine driver to be used for executing `command'.  */
Driver
preprocess_arguments(Command* command, int argc, char** argv)
{
   int i;
   int other = 1;
   int files = 0;
   Driver driver = unknown_driver;
   bool explicit_no_gui = false; // True if --no-gui explicitly specified.

   command->root_dir = get_systemdir(argc, argv);
   for (i = 1; i < argc; ++i)
      if(strcmp(argv[i], "--no-server") == 0)
         driver = core_driver;
      else if (strcmp(argv[i], "--server") == 0)
         driver = sman_driver;
      else if (strcmp(argv[i], "--config") == 0)
         driver = config_driver;
      else if (strcmp(argv[i], "--execute") == 0) {
         driver = execute_driver;
         break;
      }
      else if (strcmp(argv[i], "--help") == 0) {
         print_usage();
         driver = null_driver;
         break;
      }
      else if (strcmp(argv[i], "--version") == 0) {
         print_version();
         driver = null_driver;
         break;
      }
      else if (const char* val = is_prefix("--execpath=", argv[i])) {
            command->exec_path = val;
      }
      else {
         /* Apparently we will invoke the Core system; we need to
            pass on this option.  */
         if (const Driver d = option_driver(argv[i]))
            driver = d;
         else {
            /* Maybe option for the driver.  */
	    if (argv[i][0] == '-') {
               // this is awkward to handle here since it supposed
               // to go to Superman.  FIXME when sman is gone.
               if (strcmp(argv[i], "--no-gui") == 0)
                  explicit_no_gui = true;
	    }
            else if (strlen(argv[i]) > 0)
               /* Assume a file.  */
               ++files;
            else 
               /* Silly.  */
               continue;
         }
         /* Save it for the core executable.  */
         argv[other++] = argv[i];
      }

   /* Determine argument vector.  */
   if (driver == execute_driver) {
      command->core.argc = argc - i - 1;
      command->core.argv = argv + i + 1;
   }
   else {
      command->core.argc = other;
      command->core.argv = argv;
   }

   if (driver != null_driver) {
      /* If we have a file but not instructed to compile, assume
         we are asked to interpret a script.  */
      if (files > 0)
         switch (driver) {
         case unknown_driver:
         case sman_driver:
         case gui_driver:
            command->core.argc += 1;
            command->core.argv =
               (char**) malloc((other + 2) * sizeof(char*));
            command->core.argv[0] = argv[0];
            command->core.argv[1] = (char*) "--script";
            for (i = 0; i < other; ++i)
               command->core.argv[2 + i] = argv[1 + i];
            driver = script_driver;
            break;
         default:
            /* Driver specified by user.  */
            break;
         }
      else if (driver == unknown_driver)
         driver = default_driver(explicit_no_gui);
      command->core.argv[command->core.argc] = NULL;
      
      build_rts_options(command, driver);
   }
   return driver;
}

   // Return a pointer to the path to the program to execute, as
   // specified by `command' and `driver'.
   static const char*
   executable_path(const Command* command, Driver driver) {
      return command->exec_path != 0
         ? command->exec_path
         : make_path_for(command->root_dir, driver);
   }


/* Execute the Core Executable as described by `command'.  On
   POSIX systems, this is a non-return function on success.
   See execv().  */
int
execute_core(const Command* command, Driver driver)
{
   char* execpath = (char*) executable_path(command, driver);
#ifdef __WIN32__
   char* command_line;
   int cur = strlen(command->core.argv[0]);
   int command_line_length = 0;
   int i;
   PROCESS_INFORMATION procInfo;
   STARTUPINFO startupInfo = { 0 };
   DWORD status;                /* Exit code for this program masqueraded as
                                   the child created below.  */

   /* How long is the final command line for the MS system? */
   command_line_length += cur;
   for (i = 0; i < command->rt_args.size(); ++i)
      command_line_length += 1  /* blank char as separator */
	 + 2			/* quotes around every argument.  */
         + strlen(command->rt_args[i]); /* room for each argument */
   /* Don't forget room for the doubledash string.  */
   command_line_length += sizeof("--") - 1;
   /* And arguments to the actual command.  */
   for (i = 1; i < command->core.argc; ++i)
      command_line_length += 1 + 2 + strlen(command->core.argv[i]);

   /* Now, build the actual command line.  This is done by
      concatenating the arguments into a single string. */
   command_line = (char*) malloc(command_line_length + 1);
   strcpy(command_line, command->core.argv[0]);
   for (i = 0; i < command->rt_args.size(); ++i) {
      const int arg_length = strlen(command->rt_args[i]);
      command_line[cur++] = ' ';
      command_line[cur++] = '"';
      strcpy(command_line + cur, command->rt_args[i]);
      cur += arg_length;
      command_line[cur++] = '"';
   }
   command_line[cur++] = ' ';
   command_line[cur++] = '-'; /*  start arguments to the core executable.  */
   command_line[cur++] = '-';
   for (i = 1; i < command->core.argc; ++i) {
      const int arg_length = strlen(command->core.argv[i]);
      command_line[cur++] = ' ';
      command_line[cur++] = '"';
      strcpy(command_line + cur, command->core.argv[i]);
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
   Arguments args(command->rt_args.size() + command->core.argc + 2);
   /* GCL has this oddity that it wants to believe that argv[0] has
      something to tell about what GCL's own runtime is.  Silly.  */
   if (OPENAXIOM_BASE_RTS == gcl_runtime)
      args[0] = (char*) "";
   /* And CLISP wants to believe that argv[0] is where it hides stuff
      from the saved image.  */
   else if (OPENAXIOM_BASE_RTS == clisp_runtime)
      args[0] = execpath;
   else
      args[0] = command->core.argv[0];
   /* Now, make sure we copy whatever arguments are required by the
      runtime system.  */
   for (i = 0; i < command->rt_args.size(); ++i)
      args[i + 1] = command->rt_args[i];

   if (command->core.argc > 1) {
      /* We do have arguments from the command line.  We want to
         differentiate this from the base runtime system arguments.
         We do this by inserting a doubledash to indicate beginning
         of arguments.  */
      args[command->rt_args.size() + 1] = (char*) "--";
      /* Then, copy over the arguments received from the command line.  */
      for (i = 1; i < command->core.argc; ++i)
         args[command->rt_args.size() + i + 1] = command->core.argv[i];
      args[command->rt_args.size() + command->core.argc + 1] = NULL;
   }
   else
      args[command->rt_args.size() + command->core.argc] = NULL;

   execv(execpath, args.data());
   perror(execpath);
   return -1;
#endif /* __WIN32__ */
}

}