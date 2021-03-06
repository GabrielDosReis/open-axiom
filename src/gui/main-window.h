// Copyright (C) 2011-2013, Gabriel Dos Reis.
// All rights reserved.
// Written by Gabriel Dos Reis.
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
//     - Neither the name of OpenAxiom. nor the names of its contributors
//       may be used to endorse or promote products derived from this
//       software without specific prior written permission.
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

#ifndef OPENAXIOM_GUI_MAIN_WINDOW_INCLUDED
#define OPENAXIOM_GUI_MAIN_WINDOW_INCLUDED

#include <QMainWindow>
#include <QTabWidget>
#include "open-axiom.h"
#include "server.h"
#include "debate.h"

namespace OpenAxiom {
   // -- Main application window --
   class MainWindow : public QMainWindow {
      Q_OBJECT;
   public:
      MainWindow(int, char*[]);
      ~MainWindow();

      Server* server() { return &srv; }
      void display_error(const std::string&);

   private slots:
      void done(int, QProcess::ExitStatus);
      void display_error();
      
   private:
      Server srv;
      QTabWidget tabs;

      void read_databases();
   };
}


#endif  // OPENAXIOM_GUI_MAIN_WINDOW_INCLUDED


// Local Variables:
// mode: c++
// End:
