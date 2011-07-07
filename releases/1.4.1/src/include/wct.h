/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2011, Gabriel Dos Reis.
  All rights reserved.

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

#ifndef OPENAXIOM_wct_INCLUDED
#define OPENAXIOM_wct_INCLUDED

extern time_t ftime(char * );
extern void fatal(const char *  , const char * );
extern off_t fsize(char * );
extern Wix * scanWct(Wct *  , char * );
extern void reintern1Wct(Wct * );
extern Wix * rescanWct(void);
extern void skimWct(Wct * );
extern void skim1Wct(Wct * );
extern void printTime(long * );
extern int skimString(char *  , int  , int  , int );
extern int prChar(int );
extern Wct * reread1Wct(Wct * );
extern void sfatal(const char * );
extern Wct * read1Wct(char * );
extern Wct * nconcWct(Wct *  , Wct * );
extern void sortWct(Wct * );
extern void sort1Wct(Wct * );
extern int mystrcmp(const void  *  , const void * );
extern void burstWct(Wct * );
extern void burst1Wct(Wct * );
extern Wct * intern1Wct(char * );
extern void load_wct_file(char * );
extern void  skim_wct(void);
extern void  rescan_wct(void);
extern void  find_wct(void);

#endif  /* OPENAXIOM_wct_INCLUDED */
