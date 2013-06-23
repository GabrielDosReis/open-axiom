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
#include <QApplication>
#include "conversation.h"
#include "debate.h"

namespace OpenAxiom {
   // Largest width and line spacing, in pixel, of the font metrics
   // associated with `w'.
   static QSize font_units(const QWidget* w) {
      const QFontMetrics fm = w->fontMetrics();
      return QSize(fm.maxWidth(), fm.lineSpacing());
   }

   // Return true if the QString `s' is morally an empty string.
   // QT makes a difference between a null string and an empty string.
   // That distinction is largely pedantic and without difference
   // for most of our practical purposes.
   static bool
   empty_string(const QString& s) {
      return s.isNull() or s.isEmpty();
   }
   
   // Return a resonable margin for this frame.
   static int our_margin(const QFrame* f) {
      return 2 + f->frameWidth();
   }

   // --------------------
   // -- OutputTextArea --
   // --------------------
   OutputTextArea::OutputTextArea(QWidget* p)
         : Base(p) {
      setReadOnly(true);          // this is a output only area.
      setLineWrapMode(NoWrap);    // for the time being, mess with nothing.
      setFont(p->font());
      setViewportMargins(0, 0, 0, 0);
      setSizePolicy(QSizePolicy::Preferred, QSizePolicy::MinimumExpanding);
      // We do not want to see scroll bars.  Usually disallowing vertical
      // scroll bars and allocating enough horizontal space is sufficient
      // to ensure that we don't see any horizontal scrollbar.
      setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
      setLineWidth(1);
   }

   // This overriding implementation is so that we can control the
   // amount of vertical space in the read-only editor viewport allocated
   // for the display of output text.  In particular we do not want
   // scrollbars.
   QSize
   OutputTextArea::sizeHint() const {
      const QSize s = font_units(this);
      return QSize(width(), (1 + document()->lineCount()) * s.height());
   }

   // Concatenate two paragraphs.
   static QString
   accumulate_paragaphs(const QString& before, const QString& after) {
      if (empty_string(before))
         return after;
      return before + "\n" + after;
   }
   
   void OutputTextArea::add_paragraph(const QString& s) {
      setPlainText(accumulate_paragaphs(toPlainText(), s));
      QSize sz = sizeHint();
      sz.setWidth(parentWidget()->width() - our_margin(this));
      resize(sz);
      show();
      updateGeometry();
   }

   void OutputTextArea::add_text(const QString& s) {
      setPlainText(toPlainText() + s);
      QSize sz = sizeHint();
      const int w = parentWidget()->width() - 2 * frameWidth();
      if (w > sz.width())
         sz.setWidth(w);
      resize(sz);
      show();
      updateGeometry();
   }

   // --------------
   // -- Question --
   // --------------
   Question::Question(Exchange* e) : QLineEdit(e) {
      setBackgroundRole(QPalette::AlternateBase);
      setFrame(true);
   }

   void Question::focusInEvent(QFocusEvent* e) {
      setFrame(true);
      update();
      QLineEdit::focusInEvent(e);
   }

   void Question::enterEvent(QEvent* e) {
      setFrame(true);
      update();
      QLineEdit::enterEvent(e);
   }

   // ------------
   // -- Answer --
   // ------------
   Answer::Answer(Exchange* e) : OutputTextArea(e) {
      setFrameStyle(StyledPanel | Raised);
   }

   // --------------
   // -- Exchange --
   // --------------
   // Amount of pixel spacing between the query and reply areas.
   const int spacing = 2;

   // Return a monospace font
   static QFont monospace_font() {
      QFont f("Monaco", 11);
      f.setStyleHint(QFont::TypeWriter);
      return f;
   }

   // The layout within an exchange is as follows:
   //   -- input area (an editor) with its own decoation accounted for.
   //   -- an optional spacing
   //   -- an output area with its own decoration accounted for.
   QSize Exchange::sizeHint() const {
      const int m = our_margin(this);
      QSize sz = question()->size() + QSize(2 * m, 2 * m);
      if (not answer()->isHidden())
         sz.rheight() += answer()->height() + spacing;
      return sz;
   }

   Server* Exchange::server() const {
      return win->debate()->server();
   }

   // Dress the query area with initial properties.
   static void
   prepare_query_widget(Conversation* conv, Exchange* e) {
      Question* q = e->question();
      q->setFrame(false);
      q->setFont(conv->font());
      const int m = our_margin(e);
      q->setGeometry(m, m, conv->width() - 2 * m, q->height());
   }

   // Dress the reply aread with initial properties.
   // Place the reply widget right below the frame containing
   // the query widget; make both of the same width, of course.
   static void
   prepare_reply_widget(Conversation* conv, Exchange* e) {
      Answer* a = e->answer();
      Question* q = e->question();
      const QPoint pt = e->question()->geometry().bottomLeft();
      const int m = our_margin(a);
      a->setGeometry(pt.x(), pt.y() + spacing, 
		     conv->width() - 2 * m, q->height());
      a->setBackgroundRole(q->backgroundRole());
      a->hide();                // nothing to show yet
   }

   static void
   finish_exchange_make_up(Conversation* conv, Exchange* e) {
      e->setAutoFillBackground(true);
      e->move(conv->bottom_left());
   }
   
   Exchange::Exchange(Conversation* conv, int n)
         : QFrame(conv), win(conv), no(n), query(this), reply(this) {
      setLineWidth(1);
      setFont(conv->font());
      prepare_query_widget(conv, this);
      prepare_reply_widget(conv, this);
      finish_exchange_make_up(conv, this);
      connect(question(), SIGNAL(returnPressed()),
              this, SLOT(reply_to_query()));
   }

   // Transfter focus to this input area.
   static void 
   give_focus_to(Question* q) {
     q->setFocus(Qt::OtherFocusReason);
   }

   static void ensure_visibility(Debate* debate, Exchange* e) {
      const int y = e->y() + e->height();
      QScrollBar* vbar = debate->verticalScrollBar();
      const int value = vbar->value();
      int new_value = y - vbar->pageStep();
      if (y < value)
         vbar->setValue(std::max(new_value, 0));
      else if (new_value > value)
         vbar->setValue(std::min(new_value, vbar->maximum()));
      give_focus_to(e->question());
   }
   
   void
   Exchange::reply_to_query() {
      QString input = question()->text().trimmed();
      if (empty_string(input))
         return;
      question()->setReadOnly(true); // Make query area read only.
      server()->input(input);
      QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
   }

   void Exchange::resizeEvent(QResizeEvent* e) {
      QFrame::resizeEvent(e);
      const int w = width() - 2 * our_margin(this);
      if (w > question()->width()) {
         question()->resize(w, question()->height());
         answer()->resize(w, answer()->height());
      }
   }

   // ------------
   // -- Banner --
   // ------------
   Banner::Banner(Conversation* conv) :  Base(conv) {
      setFrameStyle(StyledPanel | Raised);
      setBackgroundRole(QPalette::Base);
   }

   // ------------------
   // -- Conversation --
   // -------------------
   
   // Default number of characters per question line.
   const int columns = 80;
   const int lines = 40;

   static QSize
   minimum_preferred_size(const Conversation* conv) {
      const QSize s = font_units(conv);
      return QSize(columns * s.width(), lines * s.height());
   }

   // Set a minimum preferred widget size, so no layout manager
   // messes with it.  Indicate we can make use of more space.
   Conversation::Conversation(Debate* d)
         : QWidget(d), win(d), greatings(this), cur_ex(), cur_out(&greatings) {
      setFont(monospace_font());
      setBackgroundRole(QPalette::Base);
      greatings.setFont(font());
      setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
   }

   Conversation::~Conversation() {
      for (int i = children.size() -1 ; i >= 0; --i)
         delete children[i];
   }

   QPoint Conversation::bottom_left() const {
      if (length() == 0)
         return greatings.geometry().bottomLeft();
      return children.back()->geometry().bottomLeft();
   }

   static QSize
   round_up_height(const QSize& sz, int height) {
      if (height < 1)
         height = 1;
      const int n = (sz.height() + height) / height;
      return QSize(sz.width(), n * height);
   }
   
   QSize Conversation::sizeHint() const {
      const int n = length();
      if (n == 0)
         return minimum_preferred_size(this);
      const int view_height = debate()->viewport()->height();
      QSize sz = greatings.size();
      for (int i = 0; i < n; ++i)
         sz.rheight() += children[i]->height();
      return round_up_height(sz, view_height);
   }

   void Conversation::resizeEvent(QResizeEvent* e) {
      QWidget::resizeEvent(e);
      setMinimumSize(size());
      const QSize sz = size();
      if (e->oldSize() == sz)
         return;
      greatings.resize(sz.width(), greatings.height());
      for (int i = 0; i < length(); ++i) {
         Exchange* e = children[i];
         e->resize(sz.width(), e->height());
      }
   }

   void Conversation::paintEvent(QPaintEvent* e) {
      QWidget::paintEvent(e);
      if (length() == 0)
         greatings.update();
   }

   Exchange*
   Conversation::new_topic() {
      Exchange* w = new Exchange(this, length() + 1);
      w->show();
      children.push_back(w);
      adjustSize();
      updateGeometry();
      cur_out = w->answer();
      return cur_ex = w;
   }

   Exchange*
   Conversation::next(Exchange* w) {
      if (w == 0 or w->number() == length())
         return new_topic();
      return cur_ex = children[w->number()];
   }

   struct OracleOutput {
      QString result;
      QString prompt;
   };

   static OracleOutput
   read_output(Server* server) {
      OracleOutput output;
      QStringList strs = QString::fromLocal8Bit(server->readAll()).split('\n');
      QStringList new_list;
      QRegExp rx("\\(\\d+\\)\\s->");
      while (not strs.isEmpty()) {
         QString s = strs.takeFirst();
         if (empty_string(s))
            continue;
         if (rx.indexIn(s) != -1) {
            output.prompt = s;
            break;
         }
         new_list.append(s);
      }
     output.result =new_list.join("\n");
     return output;
   }

   static bool
   empty(const OracleOutput& out) {
      return empty_string(out.result) and empty_string(out.prompt);
   }

   void
   Conversation::read_reply() {
      OracleOutput output = read_output(debate()->server());
      if (empty(output))
         return;
      if (not empty_string(output.result)) {
         cur_out->add_paragraph(output.result);
         adjustSize();
      }
      if (length() == 0) {
         if (not empty_string(output.prompt))
            ensure_visibility(debate(), new_topic());
      }
      else {
         exchange()->adjustSize();
         exchange()->update();
         exchange()->updateGeometry();
         if (empty_string(output.prompt))
            ensure_visibility(debate(), exchange());
         else {
            ensure_visibility(debate(), next(exchange()));
            QApplication::restoreOverrideCursor();
	 }
      }
   }
}
