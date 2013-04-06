// Copyright (C) 2011-2013, Gabriel Dos Reis.
// All rights reserved.
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

#include <QMenuBar>
#include <QAction>

#include "debate.h"
#include "main-window.h"

namespace OpenAxiom {

   // Attempt to resize the main window so that on the first exposure
   // the exchanges in this debate have the preferred geometry and the
   // horizontal scroll bar is not needed.
   static void
   try_to_honor_widget_size(MainWindow* w, Debate* debate) {
      // Force a show first, to provoke conversation size.
      // That helps getting the sizes right.  On the other hand, it
      // makes for several resize event roundring leading to
      // an awkward looking brief window showing.  FIXME.
      w->show();
      QSize diff = debate->exchanges()->size() - debate->viewport()->size();
      if (diff.width() < 0)
         diff.setWidth(0);
      if (diff.height() < 0)
         diff.setHeight(0);
      w->resize(w->size() + diff);
   }

   MainWindow::MainWindow(Command& cmd) : fs(cmd.root_dir), srv(cmd), tabs(this) {
      setCentralWidget(&tabs);
      Debate* debate = new Debate(&tabs, cmd);
      tabs.addTab(debate, "Main Frame");
      QMenu* file = menuBar()->addMenu(tr("&File"));
      QAction* action = new QAction(tr("Quit"), this);
      file->addAction(action);
      action->setShortcut(tr("Ctrl+Q"));
      connect(action, SIGNAL(triggered()), this, SLOT(close()));
      try_to_honor_widget_size(this, debate);
   }
   
   MainWindow::~MainWindow() {
   }
}
