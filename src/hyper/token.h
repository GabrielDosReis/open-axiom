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

#include "openaxiom-c-macros.h"
#include <stdio.h>

/*
 Here are a couple of flags added for whitespace stuff. They tell
      punctuation if there was space in front of it or not
*/

#define FRONTSPACE 0001
#define BACKSPACE  0002

/* HyperDoc parser tokens */

struct Token {
   int type;              /* token type.  One of those listed below */
   const char *id;                  /* string value if type == Identifier */
};

/*
    User tokens. ie, these can be found on a page
*/
enum openaxiom_token_kind {
  openaxiom_Word_token = 1,
  openaxiom_Page_token = 2,
  openaxiom_Lispcommandquit_token = 3,
  openaxiom_BoldFace_token = 4,
  openaxiom_Link_token = 5,
  openaxiom_Downlink_token = 6,
  openaxiom_Beginscroll_token = 7,
  openaxiom_Spadcommand_token = 8,
  openaxiom_NoLines_token = 9,
  openaxiom_Env_token = 10,
  openaxiom_Par_token = 11,
  openaxiom_Center_token = 12,
  openaxiom_Begin_token = 13,
  openaxiom_Beginitems_token = 14,
  openaxiom_Item_token = 15,
  openaxiom_Table_token = 16,
  openaxiom_Box_token = 17,
  openaxiom_Tab_token = 18,
  openaxiom_Space_token = 19,
  openaxiom_Indent_token = 20,
  openaxiom_Horizontalline_token = 21,
  openaxiom_Newline_token = 22,
  openaxiom_Enditems_token = 23,
  openaxiom_Returnbutton_token = 24,
  openaxiom_Memolink_token = 25,
  openaxiom_Upbutton_token = 26,
  openaxiom_Endscroll_token = 27,
  openaxiom_Thispage_token = 28,
  openaxiom_Returnto_token = 29,
  openaxiom_Free_token = 30,
  openaxiom_Bound_token = 31,
  openaxiom_Lisplink_token = 32,
  openaxiom_Unixlink_token = 33,
  openaxiom_Mbox_token = 34,
  openaxiom_Inputstring_token = 35,
  openaxiom_StringValue_token = 36,
  openaxiom_Spadlink_token = 37,
  openaxiom_Inputbitmap_token = 38,
  openaxiom_Inputpixmap_token = 39,
  openaxiom_Unixcommand_token = 40,
  openaxiom_Emphasize_token = 41,
  openaxiom_Lispcommand_token = 42,
  openaxiom_LispMemoLink_token = 43,
  openaxiom_LispDownLink_token = 44,
  openaxiom_Spadcall_token = 45,
  openaxiom_Spadcallquit_token = 46,
  openaxiom_Spaddownlink_token = 47,
  openaxiom_Spadmemolink_token = 48,
  openaxiom_Qspadcall_token = 49,
  openaxiom_Qspadcallquit_token = 50,
  openaxiom_SimpleBox_token = 51,
  openaxiom_Radioboxes_token = 52,
  openaxiom_BoxValue_token = 53,
  openaxiom_VSpace_token = 54,
  openaxiom_HSpace_token = 55,
  openaxiom_NewCommand_token = 56,
  openaxiom_WindowId_token = 57,
  openaxiom_Beep_token = 58,
  openaxiom_Quitbutton_token = 59,
  openaxiom_Begintitems_token = 60,
  openaxiom_Titem_token = 61,
  openaxiom_End_token = 62,
  openaxiom_It_token = 63,
  openaxiom_Sl_token = 64,
  openaxiom_Tt_token = 65,
  openaxiom_Rm_token = 66,
  openaxiom_Ifcond_token = 67,
  openaxiom_Else_token = 68,
  openaxiom_Fi_token = 69,
  openaxiom_Newcond_token = 70,
  openaxiom_Setcond_token = 71,
  openaxiom_Button_token = 72,
  openaxiom_Windowlink_token = 73,
  openaxiom_Haslisp_token = 74,
  openaxiom_Hasup_token = 75,
  openaxiom_Hasreturn_token = 76,
  openaxiom_Hasreturnto_token = 77,
  openaxiom_Lastwindow_token = 78,
  openaxiom_Endtitems_token = 79,
  openaxiom_Lispwindowlink_token = 80,
  openaxiom_Beginpile_token = 81,
  openaxiom_Endpile_token = 82,
  openaxiom_Nextline_token = 83,
  openaxiom_Pastebutton_token = 84,
  openaxiom_Color_token = 85,
  openaxiom_Helppage_token = 86,
  openaxiom_Patch_token = 87,
  openaxiom_Radiobox_token = 88,
  openaxiom_ifrecond_token = 89,
  openaxiom_Math_token = 90,
  openaxiom_Mitem_token = 91,
  openaxiom_Pagename_token = 92,
  openaxiom_Examplenumber_token = 93,
  openaxiom_Replacepage_token = 94,
  openaxiom_Inputimage_token = 95,
  openaxiom_Spadgraph_token = 96,
  openaxiom_Indentrel_token = 97,
  openaxiom_Controlbitmap_token = 98,
  openaxiom_NumberUserTokens_token = 98,

/*
 * Here are the system tokens. These are used internally to help
 * with parsing and displaying of text
 */
  openaxiom_SystemTokens_token = 1001,
  openaxiom_Lbrace_token = 1001,
  openaxiom_Rbrace_token = 1002,
  openaxiom_Macro_token = 1003,
  openaxiom_Group_token = 1004,
  openaxiom_Scrollbar_token = 1005,
  openaxiom_Pound_token = 1006,
  openaxiom_Lsquarebrace_token = 1007,
  openaxiom_Rsquarebrace_token = 1008,
  openaxiom_Punctuation_token = 1009,
  openaxiom_Dash_token = 1010,
  openaxiom_Tableitem_token = 1011,
  openaxiom_Scrollingnode_token = 1012,
  openaxiom_Headernode_token = 1013,
  openaxiom_Footernode_token = 1014,
  openaxiom_Verbatim_token  = 1015,
  openaxiom_Scroll_token = 1016,
  openaxiom_Dollar_token = 1017,
  openaxiom_Percent_token = 1018,
  openaxiom_Carrot_token = 1019,
  openaxiom_Underscore_token = 1020,
  openaxiom_Tilde_token = 1021,
  openaxiom_Cond_token =  1022,
  openaxiom_Noop_token = 1023,
  openaxiom_Description_token = 1024,
  openaxiom_Icorrection_token = 1025,
  openaxiom_Boxcond_token = 1026,
  openaxiom_Unkeyword_token = 1027,
  openaxiom_Titlenode_token = 1028,
  openaxiom_Paste_token = 1029,
  openaxiom_Spadsrc_token = 1030,
  openaxiom_Helpbutton_token = 1031,
  openaxiom_Spadsrctxt_token = 1032,
/*
 * Here are the tokens used to mark the end to some sort of group of
 * tokens. ie, the tokens found in a centerline command
 */
  openaxiom_Endtokens_token = 2000,
  openaxiom_End1_token = 2001,
  openaxiom_End2_token = 2002,
  openaxiom_Endbutton_token = 2003,
  openaxiom_Endlink_token = 2004,
  openaxiom_Endheader_token = 2005,
  openaxiom_Endfooter_token = 2006,
  openaxiom_Endscrolling_token = 2007,
  openaxiom_Endgroup_token = 2008,
  openaxiom_Endarg_token = 2009,
  openaxiom_Endbox_token = 2010,
  openaxiom_Endmbox_token = 2011,
  openaxiom_Endspadcommand_token = 2012,
  openaxiom_Endpix_token = 2013,
  openaxiom_Endmacro_token = 2014,
  openaxiom_Endparameter_token = 2015,
  openaxiom_Endtable_token = 2016,
  openaxiom_Endtableitem_token = 2017,
  openaxiom_End3_token = 2018,
  openaxiom_Endif_token = 2019,
  openaxiom_Enddescription_token = 2020,
  openaxiom_Endinputbox_token = 2021,
  openaxiom_Endtitle_token = 2022,
  openaxiom_Endpastebutton_token = 2023,
  openaxiom_Endtypes_token = 3000,
  openaxiom_Endpage_token = 3002,
  openaxiom_EndScroll_token =  3007, /* had to use a S because Endscroll is
                                        already a keyword      */
  openaxiom_Endcenter_token = 3012,
  openaxiom_EndItems_token = 3014, /* Same thing here as EndScroll except
                                      with the i          */
  openaxiom_EndTitems_token = 3060, /* Ibid for the T          */
  openaxiom_Endpatch_token = 3087,
  openaxiom_Endverbatim_token = 4015,
  openaxiom_Endmath_token = 4016,
  openaxiom_Endpaste_token = 4029,
  openaxiom_Endspadsrc_token = 4030
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
