/* 
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis.
  All rights reserved.
  Copyright (C) 2009, Alfredo Portes.
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

/* 
 * htsearch: is used by Hyperdoc to search for keywords in 
 * Hyperdoc pages and create a HyperDoc page of the search 
 * result.
 */

#include "openaxiom-c-macros.h"

#include <stdlib.h>
#include <stdio.h>
#include <cfuns.h>
#include <string.h>

const char* htpagedir; // Directory containing the hyperdoc pages.
const char* hthitscmd; // Location of the hthits program.
const char* htdbfile;  // Database file of the hyperdoc pages.

/*
 * pages_cmp compares two strings.
 *
 * @param str1 string to be compared.
 * @param str2 string to be compared.
 *
 * @returns 0 when the strings are equal, a negative integer 
 * when str1 is less than str2, or a positive integer if str1 is 
 * greater than str2, according to the lexicographical order.
 */
int 
pages_cmp(const char **str1, const char **str2)
{
    return strcmp(*str1,*str2);
}

/*
 * sort_pages sorts a list of HyperDoc pages links.
 *
 * @param links is an array containing the HyperDoc 
 * links to be sorted.
 *
 * @size of the number of elements in the links array.
 */
void 
sort_pages(char** links, int size)
{
    qsort(links, size, sizeof(char*), pages_cmp);
}

/*
 * presea function constructs a HyperDoc
 * page with the links to HyperDoc pages 
 * that contain the given pattern.
 *
 * @param links array with the links to the matched
 *        HyperDoc pages.
 * @param n number of elements in the links array.
 * @param cases flag indicating if there was any matches.
 * @param pattern searched in the HyperDoc pages.
 */
void 
presea(char** links, int n, int cases, char* pattern)
{
    int m = 0, i = 0, j = 0;
    char** tokens = NULL;
    const char** delimiter = "{";

    for (i = 0; i < n; i++) {

        tokens = oa_split(links[i],delimiter,&j);
        if (j >= 2)
            m = m + atol(oa_substr(tokens[1],0,strlen(tokens[1])-2));
    }

    if (cases==1)
        printf("\\begin{page}{staticsearchpage}{No matches found}\n");
    else if ( n==0 || m==0 )
        printf("\\begin{page}{staticsearchpage}{No matches found for {\\em %s}}\n",pattern);
    else
        printf("\\begin{page}{staticsearchpage}{%d matches found in %d pages for {\\em %s}}\n",m,n,pattern);
    printf("Matches\\tab{8}in Page\n");
    printf("\\beginscroll\n");
    printf("\\beginmenu\n");
    for(i = n-1; i >= 0; i--) printf ("%s\n",links[i]);
    printf("\\endmenu\n");
    printf("\\endscroll\n");
    printf("\\end{page}\n");
}

/*
 * Set global variables.
 */
void
setvariables(void) {

    const char* oavariable = oa_getenv("AXIOM");

    if (oavariable == NULL) {
        printf("%s\n", "OpenAxiom variable is not set.");
        exit(-1);
    }

    htpagedir = oa_strcat(oavariable,"/share/hypertex/pages/");
    hthitscmd = oa_strcat(oavariable,"/lib/hthits");
    htdbfile = oa_strcat(htpagedir,"ht.db");
}

/* 
 * htsearch invokes hthits to search for pattern
 * in the HyperDoc pages.
 *
 * @param pattern string to be searched in the
 * HyperDoc pages.
 */
void 
htsearch(char* pattern)
{
    FILE* hits;
    char buf[1024];
    char** sorted_hits;
    char* matches = "";
    int size = 0;
    const char** delimiter = "\n";

    setvariables();

    if (strcmp(pattern,"") == 0)
        presea(NULL,size,1,pattern);
    else {
        // hthits requires to change directory
        // to where the HyperDoc pages reside.
        if (oa_chdir(htpagedir) == -1)
            exit(-1);

        // Call hthits with: hthits pattern ht.db
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

        printf("matches: %s\n", matches);
 
        sorted_hits = oa_split(matches,delimiter,&size);
        sort_pages(sorted_hits, size);
        presea(sorted_hits,size,0,pattern);
    }
}

/*
 * Display how to use the htsearch program.  
 */
void
usage(void)
{
    fprintf(stderr, "Usage: htsearch pattern \n");
    exit(1);
}

/*
 * cmdline processes the command line arguments 
 * passed to htsearch.
 *
 * @param argc number of arguments.
 * @param argv array of command line arguments.
 */
void
cmdline(int argc, char** argv)
{
    if (argc == 1)
        htsearch("");
    else if (argc == 2) {

        if (!strcmp(argv[1],"--help"))
            usage();
        else
            htsearch(argv[1]);
    }
    else
        usage();
}

/* Main routine */
int 
main(int argc, char** argv)
{
    cmdline(argc, argv);
    return(0);
}
