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

#include <cmath>
#include <string>
#include <sstream>
#include <iostream>

#include <QScrollBar>
#include "conversation.h"
#include "debate.h"

namespace OpenAxiom {
   // Default number of characters per question line.
   const int columns = 80;

   // Measurement in pixel of the em unit in the given font `f'.
   static QSize em_metrics(const QWidget* w) {
      const QFontMetrics fm = w->fontMetrics();
      return QSize(fm.width(QLatin1Char('m')), fm.height());
   }

   // -- Question --
   Question::Question(Exchange& e) : QLineEdit(&e), parent(&e) { }

   void Question::enterEvent(QEvent* e) {
      QLineEdit::enterEvent(e);
      setFocus(Qt::OtherFocusReason);
   }

   // -- Answer --
   Answer::Answer(Exchange& e) : QLabel(&e), parent(&e) { }

   // -- Exchange --
   // Amount of pixel spacing between the query and reply areas.
   const int spacing = 0;

   // Return a monospace font
   static QFont monospace_font() {
      QFont f("Courier");
      f.setStyleHint(QFont::TypeWriter);
      return f;
   }

   static int margin(const Exchange*e) {
      return 2 + e->lineWidth();
   }
   
   QSize Exchange::sizeHint() const {
      const int m = margin(this);
      QSize sz = question()->size() + QSize(2 * m, 2 * m);
      if (not answer()->isHidden())
         sz.rheight() += answer()->height() + spacing;
      return sz;
   }

   // Dress the query area with initial properties.
   static void
   prepare_query_widget(Conversation& conv, Exchange* e) {
      Question* q = e->question();
      q->setFrame(false);
      q->setFont(conv.font());
      const int m = margin(e);
      q->setGeometry(m, m, conv.width() - 2 * m, q->height());
   }

   // Dress the reply aread with initial properties.
   static void
   prepare_reply_widget(Exchange* e, Answer* a) {
      a->setFont(e->font());
      Question* q = e->question();
      const QPoint pt = e->question()->frameGeometry().bottomLeft();
      // Place the reply widget right below the frame containing
      // the query widget; make both of the same width, of course.
      a->setGeometry(pt.x(), pt.y() + spacing,
                     q->width(), 2 * q->height());
      a->setBackgroundRole(q->backgroundRole());
      a->hide();                // nothing to show yet
   }

   static void prepare_exchange_widget(Conversation& conv, Exchange* e) {
      e->setFont(conv.font());
      e->setAutoFillBackground(true);
      e->setBackgroundRole(e->question()->backgroundRole());
      QPoint pt = conv.bottom_left();
      const QSize sz = e->sizeHint();
      e->setGeometry(pt.x(), pt.y(), sz.width(), sz.height());
   }
   
   Exchange::Exchange(Conversation& conv, int n)
         : QFrame(&conv), parent(&conv), no(n), query(*this), reply(*this) {
      setLineWidth(1);
      prepare_query_widget(conv, this);
      prepare_reply_widget(this, answer());
      prepare_exchange_widget(conv, this);
      connect(&query, SIGNAL(returnPressed()),
              this, SLOT(reply_to_query()));
   }

   static QString
   parenthesize(int n) {
      std::ostringstream os;
      os << '(' << n << ')';
      return QString::fromStdString(os.str());
   }

   static void ensure_visibility(Debate* debate, Exchange* e) {
      const int y = e->frameGeometry().y();
      QScrollBar* vbar = debate->verticalScrollBar();
      const int value = vbar->value();
      int new_value = y - vbar->pageStep();
      if (y < value)
         vbar->setValue(std::max(new_value, 0));
      else if (new_value > value) {
         new_value += 3 * vbar->singleStep();
         vbar->setValue(std::min(new_value, vbar->maximum()));
      }
      e->question()->setFocus(Qt::OtherFocusReason);
   }
   
   void
   Exchange::reply_to_query() {
      QString input = question()->text().trimmed();
      if (input.isEmpty())
         return;
      QString ans = parenthesize(number()) + " " + input;
      answer()->show();
      answer()->setText(ans);
      adjustSize();
      update();
      updateGeometry();
      ensure_visibility(conversation()->debate(), conversation()->next(this));
   }

   void Exchange::resizeEvent(QResizeEvent* e) {
      std::cerr << "resizing exchange to "
                << width() << ", " << height() << std::endl;
      QFrame::resizeEvent(e);
      const int w = width() - 2 * margin(this);
      if (w > question()->width()) {
         question()->resize(w, question()->height());
         answer()->resize(w, answer()->height());
      }
   }

   // ------------------
   // -- Conversation --
   // -------------------
   
   static QSize
   minimum_preferred_size(const Conversation* conv) {
      const QSize em = em_metrics(conv);
      return QSize(columns * em.width(), 25 * em.height());
   }
   
   Conversation::Conversation(Debate& parent) : group(parent) {
      setFont(monospace_font());
      setMinimumSize(minimum_preferred_size(this));
      setSizePolicy(QSizePolicy::MinimumExpanding,
                    QSizePolicy::MinimumExpanding);
   }

   Conversation::~Conversation() {
      for (int i = children.size() -1 ; i >= 0; --i)
         delete children[i];
   }

   QPoint Conversation::bottom_left() const {
      if (length() == 0)
         return QPoint(0, 0);
      return children.back()->geometry().bottomLeft();
   }

   QSize Conversation::sizeHint() const {
      QSize sz(0,0);
      for (int i = 0; i < length(); ++i) {
         QSize s = children[i]->sizeHint();
         if (s.width() > sz.width())
            sz.setWidth(s.width());
         sz.rheight() += s.height();
      }
      const int view_height = debate()->viewport()->width();
      const int n = (sz.height() + view_height) / view_height;
      return QSize(sz.width(), n * view_height);
   }

   void Conversation::resizeEvent(QResizeEvent* e) {
      const QSize sz = size();
      if (e->oldSize() == sz)
         return;
      for (int i = 0; i < length(); ++i) {
         Exchange* e = children[i];
         e->resize(sz.width(), e->height());
      }
      debate()->updateGeometry();
   }

   void Conversation::paintEvent(QPaintEvent* e) {
      QWidget::paintEvent(e);
      if (length() == 0)
         new_topic();
   }

   Exchange*
   Conversation::new_topic() {
      Exchange* w = new Exchange(*this, length() + 1);
      w->show();
      children.push_back(w);
      adjustSize();
      update();
      debate()->updateGeometry();
      return w;
   }

   Exchange*
   Conversation::next(Exchange* w) {
      if (w == 0 or w->number() == length())
         return new_topic();
      return children[w->number()];
   }
}
