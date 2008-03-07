/*
   Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
   All rights reserved.
   Copyright (C) 2007-2008, Gabriel Dos Reis.
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

/*
 Here are a couple of flags added for whitespace stuff. They tell
      punctuation if there was space in front of it or not
*/

#define FRONTSPACE 0001
#define BACKSPACE  0002

/* HyperDoc parser tokens */

typedef struct toke {
  int type;             /* token type.  One of those listed below */
  char *id;             /* string value if type == Identifier */
} Token;

/*
    User tokens. ie, these can be found on a page
*/

#define Word                  1
#define Page                  2
#define Lispcommandquit       3
#define BoldFace              4
#define Link                  5
#define Downlink              6
#define Beginscroll           7
#define Spadcommand           8
#define NoLines               9
#define Env                  10
#define Par                  11
#define Center               12
#define Begin                13
#define Beginitems           14
#define Item                 15
#define Table                16
#define Box                  17
#define Tab                  18
#define Space                19
#define Indent               20
#define Horizontalline       21
#define Newline              22
#define Enditems             23
#define Returnbutton         24
#define Memolink             25
#define Upbutton             26
#define Endscroll            27
#define Thispage             28
#define Returnto             29
#define Free                 30
#define Bound                31
#define Lisplink             32
#define Unixlink             33
#define Mbox                 34
#define Inputstring          35
#define StringValue          36
#define Spadlink             37
#define Inputbitmap          38
#define Inputpixmap          39
#define Unixcommand          40
#define Emphasize            41
#define Lispcommand          42
#define LispMemoLink         43
#define LispDownLink         44
#define Spadcall             45
#define Spadcallquit         46
#define Spaddownlink         47
#define Spadmemolink         48
#define Qspadcall            49
#define Qspadcallquit        50
#define SimpleBox            51
#define Radioboxes           52
#define BoxValue             53
#define VSpace               54
#define HSpace               55
#define NewCommand           56
#define WindowId             57
#define Beep                 58
#define Quitbutton           59
#define Begintitems          60
#define Titem                61
#define End                  62
#define It                   63
#define Sl                   64
#define Tt                   65
#define Rm                   66
#define Ifcond               67
#define Else                 68
#define Fi                   69
#define Newcond              70
#define Setcond              71
#define Button               72
#define Windowlink           73
#define Haslisp              74
#define Hasup                75
#define Hasreturn            76
#define Hasreturnto          77
#define Lastwindow           78
#define Endtitems            79
#define Lispwindowlink       80
#define Beginpile            81
#define Endpile              82
#define Nextline             83
#define Pastebutton          84
#define Color                85
#define Helppage             86
#define Patch                87
#define Radiobox             88
#define ifrecond             89
#define Math                 90
#define Mitem                91
#define Pagename             92
#define Examplenumber        93
#define Replacepage          94
#define Inputimage           95
#define Spadgraph            96
#define Indentrel            97
#define Controlbitmap        98

#define NumberUserTokens     98


extern char *token_table[];

#ifdef PARSER
char *token_table[] = {
  "",           /* Dummy token name */
  "word",
  "page",
  "lispcommandquit",
  "bf",
  "link",
  "downlink",
  "beginscroll",
  "spadcommand",
  "nolines",
  "env",
  "par",
  "centerline",
  "begin",
  "beginitems",
  "item",
  "table",
  "fbox",
  "tab",
  "space",
  "indent",
  "horizontalline",
  "newline",
  "enditems",
  "returnbutton",
  "memolink",
  "upbutton",
  "endscroll",
  "thispage",
  "returnto",
  "free",
  "bound",
  "lisplink",
  "unixlink",
  "mbox",
  "inputstring",
  "stringvalue",
  "spadlink",
  "inputbitmap",
  "inputpixmap",
  "unixcommand",
  "em",
  "lispcommand",
  "lispmemolink",
  "lispdownlink",
  "spadcall",
  "spadcallquit",
  "spaddownlink",
  "spadmemolink",
  "qspadcall",
  "qspadcallquit",
  "inputbox",
  "radioboxes",
  "boxvalue",
  "vspace",
  "hspace",
  "newcommand",
  "windowid",
  "beep",
  "quitbutton",
  "begintitems",
  "titem",
  "end",
  "it",
  "sl",
  "tt",
  "rm",
  "ifcond",
  "else",
  "fi",
  "newcond",
  "setcond" ,
  "button",
  "windowlink",
  "haslisp",
  "hasup",
  "hasreturn",
  "hasreturnto",
  "lastwindow",
  "endtitems",
  "lispwindowlink",
  "beginpile",
  "endpile",
  "nextline",
  "pastebutton",
  "color",
  "helppage",
  "patch",
  "radiobox",
  "ifrecond",
  "math",
  "mitem",
  "pagename",
  "examplenumber",
  "replacepage",
  "inputimage",
  "spadgraph",
  "indentrel",
  "controlbitmap"
  };
#endif


/* places from which input may be read */
#define FromFile        1
#define FromString      2
#define FromSpadSocket  3
#define FromUnixFD      4

extern FILE *unixfd;

/*
 * Here are the system tokens. These are used internally to help
 * with parsing and displaying of text
 */

#define SystemTokens   1001
#define Lbrace         1001
#define Rbrace         1002
#define Macro          1003
#define Group          1004
#define Scrollbar      1005
#define Pound          1006
#define Lsquarebrace   1007
#define Rsquarebrace   1008
#define Punctuation    1009
#define Dash           1010
#define Tableitem      1011
#define Scrollingnode  1012
#define Headernode     1013
#define Footernode     1014
#define Verbatim       1015
#define Scroll         1016
#define Dollar         1017
#define Percent        1018
#define Carrot         1019
#define Underscore     1020
#define Tilde          1021
#define Cond           1022
#define Noop           1023
#define Description    1024
#define Icorrection    1025
#define Boxcond        1026
#define Unkeyword      1027
#define Titlenode      1028
#define Paste          1029
#define Spadsrc        1030
#define Helpbutton     1031
#define Spadsrctxt     1032


/*
 * Here are the tokens used to mark the end to some sort of group of
 * tokens. ie, the tokens found in a centerline command
 */

#define Endtokens      2000
#define End1           2001
#define End2           2002
#define Endbutton      2003
#define Endlink        2004
#define Endheader      2005
#define Endfooter      2006
#define Endscrolling   2007
#define Endgroup       2008
#define Endarg         2009
#define Endbox         2010
#define Endmbox        2011
#define Endspadcommand 2012
#define Endpix         2013
#define Endmacro       2014
#define Endparameter   2015
#define Endtable       2016
#define Endtableitem   2017
#define End3           2018
#define Endif          2019
#define Enddescription 2020
#define Endinputbox    2021
#define Endtitle       2022
#define Endpastebutton 2023

#define Endtypes       3000
#define Endpage        3002
#define EndScroll      3007        /* had to use a S because Endscroll is
                                       already a keyword      */

#define Endcenter      3012
#define EndItems       3014        /* Same thing here as EndScroll except
                                          with the i          */
#define EndTitems      3060        /* Ibid for the T          */
#define Endpatch       3087
#define Endverbatim    4015
#define Endmath        4016
#define Endpaste       4029
#define Endspadsrc     4030

#endif
