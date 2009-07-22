/* 
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis.
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

#include "openaxiom-c-macros.h"

#include <stdlib.h>
#include <stdio.h>
#include <cfuns.h>
#include <string.h>

char* htpagedir;
char* hthitscmd;
char* htdbfile;

int 
pages_cmp(const void *str1, const void *str2)
{
    return strcmp(*((char**)str1), *((char**)str2));
}

void 
sort_pages(char** list, int size)
{
    qsort(list, size, sizeof(char*), pages_cmp);
}

void 
presea(char** a, int n, int cases, char* expr)
{
    int m = 0, i = 0, j = 0;
    char** b = NULL;

    for (i = 0; i < n; i++) {

        b = oa_split(a[i],"{",&j);
        if (j >= 2)
            m = m + atol(oa_substr(b[1],0,strlen(b[1])-2));
    }

    if (cases==1)
        printf("\\begin{page}{staticsearchpage}{No matches found}\n");
    else if ( n==0 || m==0 )
        printf("\\begin{page}{staticsearchpage}{No matches found for {\\em %s}}\n",expr);
    else
        printf("\\begin{page}{staticsearchpage}{%d matches found in %d pages for {\\em %s}}\n",m,n,expr);
    printf("Matches\\tab{8}in Page\n");
    printf("\\beginscroll\n");
    printf("\\beginmenu\n");
    for(i = n-1; i >= 0; i--) printf ("%s\n",a[i]);
    printf("\\endmenu\n");
    printf("\\endscroll\n");
    printf("\\end{page}\n");
}

void 
htsearch(char* pattern)
{

   FILE* hits;
   char buf[1024];
   char** sorted_hits;
   char* matches = "";
   int size = 0;

   if (strcmp(pattern,"") == 0)
      presea(NULL,size,1,pattern);
   else {


      if (oa_chdir(htpagedir) == -1)
        exit(-1);

      hthitscmd = oa_strcat(hthitscmd, " ");
      hthitscmd = oa_strcat(hthitscmd, pattern);
      hthitscmd = oa_strcat(hthitscmd, " ");
      hthitscmd = oa_strcat(hthitscmd, htdbfile);

      if ((hits = popen(hthitscmd, "r")) != NULL) {
          while (fgets(buf, 1024, hits) != NULL)
             matches = oa_strcat(matches,buf);
          pclose(hits);
      }
      else {
          printf("Could not execute %s", hthitscmd);
          exit(-1);
      }
 
      sorted_hits = oa_split(matches,"\n",&size);
      sort_pages(sorted_hits, size);
      presea(sorted_hits,size,0,pattern);
   }
}

static void
usage(char* progname)
{
   fprintf(stderr, "Usage: %s pattern \n", progname);
   exit(1);
}

static void
cmdline(int argc, char** argv)
{
   char* progname = argv[0];

   if (argc == 1)
      htsearch("");
   else if (argc == 2) {

      if (!strcmp(argv[1],"--help"))
         usage(progname);
      else
         htsearch(argv[1]);
   }
   else
      usage(progname);
}

int 
main(int argc, char** argv)
{
   const char* oavariable = oa_getenv("AXIOM");

   if (oavariable == NULL) {
      printf("%s\n", "OpenAxiom variable is not set.");
      return(-1);
   }

   htpagedir = oa_strcat(oavariable,"/share/hypertex/pages/");
   hthitscmd = oa_strcat(oavariable,"/lib/hthits");
   htdbfile = oa_strcat(htpagedir,"ht.db");
   cmdline(argc, argv);
   return(0);
}
