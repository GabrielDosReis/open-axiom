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

#include <QApplication>
#include <QScrollBar>
#include "debate.h"

namespace OpenAxiom {

   static void
   start_interpreter(Conversation* conv, Command& cmd) {
      QStringList args;
      for (int i = 0; i < cmd.rt_args.size(); ++i)
         args << cmd.rt_args[i];
      args << "--" << "--role=server";
      for (int i = 1; i < cmd.core.argc; ++i)
         args << cmd.core.argv[i];
      conv->oracle()->start(make_path_for(cmd.root_dir, Driver::core), args);
      // When invoked in a --role=server mode, OpenAxiom would
      // wait to be pinged before displaying a prompt.  This is
      // an unfortunate result of a rather awkward hack.
      conv->submit_query("");
   }

  Debate::Debate(QWidget* parent, Command& cmd)
        : QScrollArea(parent), conv(*this) {
      setWidget(&conv);
      setViewportMargins(0, 0, 0, 0);
      viewport()->setAutoFillBackground(true);
      viewport()->setBackgroundRole(conv.backgroundRole());
      setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
      setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOn);
      adjustSize();
      start_interpreter(exchanges(), cmd);
      connect(conv.oracle(),
              SIGNAL(finished(int,QProcess::ExitStatus)),
              this, SLOT(done(int)));
   }

   Debate::~Debate() { }

   void Debate::resizeEvent(QResizeEvent* e) {
      QScrollArea::resizeEvent(e);
      if (conv.length() != 0)
         conv.resize(viewport()->size());
      setSizePolicy(QSizePolicy::MinimumExpanding,
                    QSizePolicy::MinimumExpanding);
   }

   void Debate::done(int exit_code) {
      // For the time being, shut done the whole application
      // if the interpreter quits.  FIXME.
      QApplication::exit(exit_code);
   }
   
}
