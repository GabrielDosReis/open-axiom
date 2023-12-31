/*
   Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
   All rights reserved.
   Copyright (C) 2007-2023, Gabriel Dos Reis.
   All right reserved.

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

#ifndef _TOKEN_H_
#define _TOKEN_H_ 1

#include "openaxiom-c-macros.h"
#include <stdio.h>
#include <type_traits>

/*
 Here are a couple of flags added for whitespace stuff. They tell
      punctuation if there was space in front of it or not
*/
#define FRONTSPACE 0001
#define BACKSPACE  0002

namespace OpenAxiom {
  // The category of enumeration types.
  template<typename T>
  concept EnumType = std::is_enum_v<T>;

  // Return the underlying integer value of an enumeration value.
  template<EnumType T>
  constexpr auto rep(T t)
  {
     return static_cast<std::underlying_type_t<T>>(t);
  }

   // User tokens. ie, these can be found on a page
   enum class TokenType : int {
      Word = 1,
      Page = 2,
      Lispcommandquit = 3,
      BoldFace = 4,
      Link = 5,
      Downlink = 6,
      Beginscroll = 7,
      Spadcommand = 8,
      NoLines = 9,
      Env = 10,
      Par = 11,
      Center = 12,
      Begin = 13,
      Beginitems = 14,
      Item = 15,
      Table = 16,
      Box = 17,
      Tab = 18,
      Space = 19,
      Indent = 20,
      Horizontalline = 21,
      Newline = 22,
      Enditems = 23,
      Returnbutton = 24,
      Memolink = 25,
      Upbutton = 26,
      Endscroll = 27,
      Thispage = 28,
      Returnto = 29,
      Free = 30,
      Bound = 31,
      Lisplink = 32,
      Unixlink = 33,
      Mbox = 34,
      Inputstring = 35,
      StringValue = 36,
      Spadlink = 37,
      Inputbitmap = 38,
      Inputpixmap = 39,
      Unixcommand = 40,
      Emphasize = 41,
      Lispcommand = 42,
      LispMemoLink = 43,
      LispDownLink = 44,
      Spadcall = 45,
      Spadcallquit = 46,
      Spaddownlink = 47,
      Spadmemolink = 48,
      Qspadcall = 49,
      Qspadcallquit = 50,
      SimpleBox = 51,
      Radioboxes = 52,
      BoxValue = 53,
      VSpace = 54,
      HSpace = 55,
      NewCommand = 56,
      WindowId = 57,
      Beep = 58,
      Quitbutton = 59,
      Begintitems = 60,
      Titem = 61,
      End = 62,
      It = 63,
      Sl = 64,
      Tt = 65,
      Rm = 66,
      Ifcond = 67,
      Else = 68,
      Fi = 69,
      Newcond = 70,
      Setcond = 71,
      Button = 72,
      Windowlink = 73,
      Haslisp = 74,
      Hasup = 75,
      Hasreturn = 76,
      Hasreturnto = 77,
      Lastwindow = 78,
      Endtitems = 79,
      Lispwindowlink = 80,
      Beginpile = 81,
      Endpile = 82,
      Nextline = 83,
      Pastebutton = 84,
      Color_token = 85,
      Helppage = 86,
      Patch = 87,
      Radiobox = 88,
      Ifrecond = 89,
      Math = 90,
      Mitem = 91,
      Pagename = 92,
      Examplenumber = 93,
      Replacepage = 94,
      Inputimage = 95,
      Spadgraph = 96,
      Indentrel = 97,
      Controlbitmap = 98,
      NumberUserTokens = 98,
      
      // Here are the system tokens. These are used internally to help
      // with parsing and displaying of text
      SystemTokens = 1001,
      Lbrace = 1001,
      Rbrace = 1002,
      Macro = 1003,
      Group = 1004,
      Scrollbar = 1005,
      Pound = 1006,
      Lsquarebrace = 1007,
      Rsquarebrace = 1008,
      Punctuation = 1009,
      Dash = 1010,
      Tableitem = 1011,
      Scrollingnode = 1012,
      Headernode = 1013,
      Footernode = 1014,
      Verbatim  = 1015,
      Scroll = 1016,
      Dollar = 1017,
      Percent = 1018,
      Carrot = 1019,
      Underscore = 1020,
      Tilde = 1021,
      Cond =  1022,
      Noop = 1023,
      Description = 1024,
      Icorrection = 1025,
      Boxcond = 1026,
      Unkeyword = 1027,
      Titlenode = 1028,
      Paste = 1029,
      Spadsrc = 1030,
      Helpbutton = 1031,
      Spadsrctxt = 1032,
      Scrollupbutton = 1033,
      Scrolldownbutton = 1034,

      // Here are the tokens used to mark the end to some sort of group of
      // tokens. ie, the tokens found in a centerline command
      Endtokens = 2000,
      End1 = 2001,
      End2 = 2002,
      Endbutton = 2003,
      Endlink = 2004,
      Endheader = 2005,
      Endfooter = 2006,
      Endscrolling = 2007,
      Endgroup = 2008,
      Endarg = 2009,
      Endbox = 2010,
      Endmbox = 2011,
      Endspadcommand = 2012,
      Endpix = 2013,
      Endmacro = 2014,
      Endparameter = 2015,
      Endtable = 2016,
      Endtableitem = 2017,
      End3 = 2018,
      Endif = 2019,
      Enddescription = 2020,
      Endinputbox = 2021,
      Endtitle = 2022,
      Endpastebutton = 2023,
      Endtypes = 3000,
      Endpage = 3002,
      EndScroll =  3007, // had to use a S because Endscroll is already a keyword
      Endcenter = 3012,
      EndItems = 3014, // Same thing here as EndScroll except with the i
      EndTitems = 3060, // Ibid for the T
      Endpatch = 3087,
      Endverbatim = 4015,
      Endmath = 4016,
      Endpaste = 4029,
      Endspadsrc = 4030,

      // Types of HyperDoc pages
      UlUnknownPage = 9993, /*I hate this hack, but I have to know whether*/
      UnknownPage = 9994, /*this page has been loaded or not.           */
      ErrorPage = 9995,
      Unixfd = 9996,
      SpadGen = 9997,
      Normal = 9998,
      UnloadedPageType = 9999,
   };

   // Commands from the server.
   // See also interp/hypertex.boot and interp/nhyper.boot
   enum class HyperCommand : int {
      SpadError = 90,
      ReplaceNamedPage = 91,
      ReplacePage = 92,
      KillPage = 93,
      PopUpNamedPage = 94,
      PopUpPage = 95,          /* A pop-up page          */
      LinkToPage = 96,
      StartPage = 97,          /* A normal HyperDoc page */
      SendLine = 98,
      EndOfPage = 99,
      PageStuff = 100,
  };
}

/* HyperDoc parser tokens */
struct Token {
   OpenAxiom::TokenType type;              /* token type.  One of those listed below */
   const char *id;                  /* string value if type == Identifier */
};

extern const char *token_table[];

/* places from which input may be read */
enum class SourceInputKind {
   Error = -1,
      
   File = 1,
   String = 2,
   SpadSocket = 3,
   UnixFD = 4
};

namespace OpenAxiom {
  // Basic error type in the Hyper component.
  struct HyperError { };
}

extern FILE *unixfd;

#endif
