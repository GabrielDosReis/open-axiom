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

#ifndef OPENAXIOM_UTILS_INCLUDED
#define OPENAXIOM_UTILS_INCLUDED

#include "open-axiom.h"
#if HAVE_UNISTD_H
#  include <unistd.h>
#endif
#ifdef __WIN32__
#  include <windows.h>
#endif

#include <vector>

namespace OpenAxiom {
   // A list of drivers for OpenAxiom. 
   enum Driver {
      unknown_driver,    // unknown driver
      null_driver,       // do nothing
      config_driver,     // print out configuration information
      sman_driver,       // start Superman as master process
      core_driver,       // start the core system as master process
      script_driver,     // start the core system in script mode.
      compiler_driver,   // start the core system in compiler mode.
      execute_driver,    // Execute a command.
      translator_driver, // Start the core system in translator mode.
      linker_driver      // start the core system in linking mode.
   };
   
   // A list of runtime support systems for OpenAxiom.
   enum Runtime {
      unknown_runtime,
      gcl_runtime,       // GCL-based runtime 
      sbcl_runtime,      // SBCL-based runtime
      clisp_runtime,     // CLISP-based runtime
      ecl_runtime,       // ECL-based runtime
      clozure_runtime,   // Clozure CL-based runtime
      bemol_runtime      // Bemol-based runtime
   };

   // Command line arguments.
   // When non empty, this vector really is of length one more than
   // what size() reports, as it is always null-terminated, to comply
   // with POSIX-style operating system requirements.
   struct Arguments : std::vector<char*> {
      explicit Arguments(int n = 0);
      int size() const;
      void allocate(int);
      char* const* data() const;
   };
   
   // A description of external command to be executed. 
   struct Command {
      Process core;           // arguments for actual executable.
      Arguments rt_args;      // arguments to the base RT, if any. 
      const char* root_dir;   // path to the OpenAxiom system.
      const char* exec_path;  // path to the program to execute.
      Command();
   };

   const char* get_systemdir(int argc, char*[]);
   const char* make_path_for(const char*, Driver);

   int execute_core(const Command*, Driver);
   void build_rts_options(Command*, Driver);
   
   Driver preprocess_arguments(Command*, int, char**);
}

#endif /* OPENAXIOM_UTILS_INCLUDED */
