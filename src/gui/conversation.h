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

#ifndef OPENAXIOM_CONVERSATION_INCLUDED
#define OPENAXIOM_CONVERSATION_INCLUDED

#include <vector>
#include <QFrame>
#include <QLineEdit>
#include <QTextEdit>
#include <QFont>
#include <QEvent>
#include <QResizeEvent>
#include <QPaintEvent>
#include "server.h"

namespace OpenAxiom {
   // A conversation is a set of exchanges.  An exchange is a question
   // followed by an answer.  A conversation that takes place in a
   // a certain frame is a debate.
   class Debate;
   class Conversation;
   class Exchange;
   class Question;
   class Answer;

   class MainWindow;

   // --------------------
   // -- OutputTextArea --
   // --------------------
   // An output text area is a widget where we output text.
   // The texts are accumulated, as opposed to overwritten.
   class OutputTextArea : public QTextEdit {
      typedef QTextEdit Base;
   public:
      explicit OutputTextArea(QWidget*);
      // the metrics of this output area
      QSize sizeHint() const;
      // Add a new paragraph to existing texts.  Paragraghs are
      // separated by the newline character.
      void add_paragraph(const QString&);
      // Add accumulate new text.
      void add_text(const QString&);
   };

   // ---------------
   // -- Question --
   // ---------------
   // A question is just a one-liner query area.
   struct Question : QLineEdit {
      explicit Question(Exchange*);

   protected:
      void enterEvent(QEvent*);
      void focusInEvent(QFocusEvent*);
   };

   // ------------
   // -- Answer --
   // ------------
   struct Answer : OutputTextArea {
      explicit Answer(Exchange*);
   };

   // --------------
   // -- Exchange --
   // --------------
   class Exchange : public QFrame {
      Q_OBJECT;
   public:
      Exchange(Conversation*, int);

      // The widget holding the query area
      Question* question() { return &query; }
      const Question* question() const { return &query; }
      Server* server() const;

      // The widget holding the reply area.
      Answer* answer() { return &reply; }
      const Answer* answer() const { return &reply; }

      // Conversion number
      int number() const { return no; }

      // Reimplement position management.
      QSize sizeHint() const;

   protected:
      void resizeEvent(QResizeEvent*);

   private:
      Conversation* const win;
      const int no;
      Question query;
      Answer reply;

   private slots:
      void reply_to_query();
   };

   // Conversation banner, welcome greatings.
   class Banner : public OutputTextArea {
      typedef OutputTextArea Base;
   public:
      explicit Banner(Conversation*);
   };

   // -- Set of conversations that make up a session.
   // -- We remember and number each topic so that we
   // -- can go back in the conversation set and reevaluate
   // -- queries.
   class Conversation : public QWidget {
      Q_OBJECT;
   public:
      explicit Conversation(Debate*);
      ~Conversation();

      Debate* debate() const { return win; }

      // Holds if this conversation just started.
      bool fresh() const { return children.empty(); }

      // Number of exchanges in this conversation
      int length() const { return children.size(); }

      // Return the `i'-th conversation in this set, if any.
      Exchange* operator[](int) const;

      // Return the bottom left corner of the rectangle enclosing the
      // the set of exchanges in this conversation.
      QPoint bottom_left() const;
      
      // Start a new conversation topic.
      Exchange* new_topic();
      
      // Override QWidegt::sizeHint.  Return the cumulative sizes
      // of all conversations so far.
      QSize sizeHint() const;

      // Return a pointer to the current exchange, if any.
      Exchange* exchange() { return cur_ex; }

   public slots:
      // Return the topic following a given topic in this set of conversations
      Exchange* next(Exchange*);
      
   protected:
      void resizeEvent(QResizeEvent*);
      void paintEvent(QPaintEvent*);

   private slots:
      void read_reply();

   private:
      typedef std::vector<Exchange*> Children;
      Debate* const win;
      Banner greatings;
      Children children;
      Exchange* cur_ex;
      OutputTextArea* cur_out;
   };
}

#endif  // OPENAXIOM_CONVERSATION_INCLUDED


// Local Variables:
// mode: c++
// End:
