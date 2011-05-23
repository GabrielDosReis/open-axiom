// Copyright (C) 2011, Gabriel Dos Reis.
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

#include <string>
#include <sstream>
#include <iostream>

#include <QScrollBar>
#include "conversation.h"
#include "debate.h"

namespace OpenAxiom {
   // -- Question --
   Question::Question(Exchange& e) : QLineEdit(&e) { }

   // -- Answer --
   Answer::Answer(Exchange& e) : QLabel(&e) { }
   

   // -- Exchange --
   // Amount of pixel spacing between the query and reply areas.
   const int spacing = 0;

   // Margin around query and reply areas.
   const int margin = 2;
   
   QSize Exchange::sizeHint() const {
      QSize sz = question()->frameSize();
      sz.rwidth() += 2 * margin;
      sz.rheight() += answer()->frameSize().height() + spacing + 2 * margin;
      return sz;
   }

   QSize Exchange::minimumSizeHint() const {
      QSize sz = question()->frameSize();
      sz.rwidth() += 2 * margin;
      if (not answer()->isHidden())
         sz.rheight() += answer()->frameSize().height() + spacing;
      sz.rheight() += 2 * margin;
      return sz;
   }

   // Return a monospace font
   QFont
   monospace_font() {
      QFont f("Courier");
      f.setStyleHint(QFont::TypeWriter);
      return f;
   }

   // Measurement in pixel of the em unit in the given font metrics.
   static QSize
   em_metrics(const QFontMetrics& fm) {
      return QSize(fm.width(QLatin1Char('m')), fm.height());
   }
   
   // Measurement in pixel of the em unit in the given font `f'.
   QSize em_metrics(const QFont& f) {
      return em_metrics(QFontMetrics(f));
   }

   // Dress the query area with initial properties.
   static void
   prepare_query_widget(QLineEdit* w) {
      w->setFrame(false);
      w->setFont(monospace_font());
      QSize em = em_metrics(w->fontMetrics());
      w->setGeometry(margin, margin,
                     question_columns * em.width(), em.height());
   }

   // Dress the reply aread with initial properties.
   static void
   prepare_reply_widget(QLabel* w, const QRect& below) {
      w->setFont(monospace_font());
      w->setGeometry(below.x(), below.height() + spacing,
                     below.width(), 2 * w->fontMetrics().height());
      w->hide();                // nothing to show yet
   }
   
   // -- Exchange --
   Exchange::Exchange(Conversation& conv, int n)
         : QFrame(&conv), no(n), query(*this), reply(*this) {
      // 1. Construct the query area.
      prepare_query_widget(question());

      // 2. Construct the response area.
      prepare_reply_widget(answer(), question()->geometry());

      // 3. Construct  the whole frame
      QSize qs = question()->frameSize();
      QSize rs = answer()->frameSize();
      resize(qs.width() + 2 * margin, qs.height() + rs.height() + 2 * margin);
      setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
      setLineWidth(1);
      // 4.
      connect(&query, SIGNAL(returnPressed()),
              this, SLOT(reply_to_query()));
   }

   static QString
   parenthesize(int n) {
      std::ostringstream os;
      os << '(' << n << ')';
      return QString::fromStdString(os.str());
   }

   Conversation* Exchange::conversation() {
      return &dynamic_cast<Conversation&>(*parentWidget());
   }

   Debate* Exchange::debate() {
      return conversation()->debate();
   }
   
   void
   Exchange::reply_to_query() {
      QString input = question()->text().trimmed();
      if (input.isEmpty())
         return;
      QString ans = parenthesize(number()) + " " + input;
      answer()->show();
      answer()->setText(ans);
      debate()->ensureWidgetVisible(answer());
      debate()->horizontalScrollBar()->setValue(0);
      conversation()->next(this)->question()
         ->setFocus(Qt::OtherFocusReason);
   }

   // ------------------
   // -- Conversation --
   // -------------------

   static void resize_if_necessary(Conversation& conv) {
      QSize sz = conv.sizeHint();
      if (sz.height() > conv.height())
         conv.resize(conv.width(), sz.height());
   }

   static void debug_engine(Conversation& conv) {
      std::cerr << "# conversations: " << conv.length()
                << std::endl;
      QSize sz = conv.sizeHint();
      std::cerr << "size: " << sz.width() << ", " << sz.height()
                << std::endl;
   }

   Conversation::Conversation(Debate& parent)
         : QWidget(&parent), group(parent) {
      setSizePolicy(QSizePolicy::Preferred, QSizePolicy::MinimumExpanding);
      QSize sz = new_topic()->frameSize();
      setMinimumSize(sz.width(), 10 * sz.height());
   }

   Conversation::~Conversation() {
      for (int i = children.size() -1 ; i >= 0; --i)
         delete children[i];
   }

   QSize Conversation::sizeHint() const {
      QSize sz(0,0);
      for (int i = 0; i < length(); ++i) {
         QSize s = children[i]->sizeHint();
         if (s.width() > sz.width())
            sz.setWidth(s.width());
         sz.rheight() += s.height();
      }
      return sz;
   }

   Exchange*
   Conversation::new_topic() {
      QPoint loc(0,0);
      if (not fresh()) {
         Exchange* last = children.back();
         loc = last->geometry().bottomLeft();
      }
      Exchange* w = new Exchange(*this, length() + 1);
      w->setVisible(true);
      w->move(loc);
      children.push_back(w);
      resize_if_necessary(*this);
      debug_engine(*this);
      return w;
   }


   Exchange*
   Conversation::next(Exchange* w) {
      if (w == 0 or w->number() == length())
         return new_topic();
      return children[w->number()];
   }
}
