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

#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

#include "debug.h"
#include "cfuns.h"
#include "halloc.h"
#include "sockio.h"
#include "addfile.h"

/* FIXME: Remove this kludge */
using namespace OpenAxiom;


static int build_ht_filename(char*, char*, const char*);
static int pathname(const char*);

/* Return non-zero if the string T is a postfix of S.  */
static int
strpostfix(const char *s, const char *t)
{
    const int slen = strlen(s);
    const int tlen = strlen(t);
    return tlen <= slen && strcmp(s + slen - tlen, t) == 0;
}

/* extend_ht : just checks the name and adds a .ht if needed */

void
extend_ht(char *name)
{

    if (!strpostfix(name, ".ht") && !strpostfix(name, ".pht"))
        strcat(name, ".ht");
    return;
}

#define cwd(n) ((n[0] == '.' && n[1] == '/')?(1):(0))

/*
 * This procedure is sent a filename, and from it tries to build the full
 * filename, this it returns in the fullname variable. If the file is not
 * found, then it returns a -1. The fname is the fullpath name for the file,
 * including the .ht extension. The aname is the filename minus the added .ht
 * extension, and the pathname.
 */

static int
build_ht_filename(char *fname, char *aname, const char* name)
{
    char *c_dir;
    char *HTPATH;
    const char *trace;
    char *trace2;
    int ht_file;

    if (cwd(name)) {
        /* user wants to use the current working directory */
        c_dir = oa_getcwd();
        strcpy(fname, c_dir);
        free(c_dir);

        /* Now add the rest of the filename */
        strcat(fname, "/");
        strcat(fname, &name[2]);

        /** now copy the actual file name to addname **/
        for (trace = &name[strlen(name)]; trace != name &&
             (*trace != '/'); trace--);
        if (trace == name) {
            fprintf(stderr, "ht_open_file: Didn't expect a filename like %s\n",
                    name);
            exit(-1);
        }
        trace++;
        strcpy(aname, trace);

        /** add  the .ht extension if needed **/
        extend_ht(aname);
        extend_ht(fname);

        /* Now just try to access the file */
        return oa_access_file_for_read(fname);
    }
    else if (pathname(name)) {
        /* filename already has the path specified */
        strcpy(fname, name);

        /** now copy the actual file name to addname **/
        for (trace = &name[strlen(name)]; trace != name &&
             (*trace != '/'); trace--);
        if (trace == name) {
            fprintf(stderr, "ht_open_file: Didn't expect a filename like %s\n",
                    name);
            exit(-1);
        }
        trace++;
        strcpy(aname, trace);

        /** add  the .ht extension if needed **/
        extend_ht(aname);
        extend_ht(fname);

        /* Now just try to access the file */
        return oa_access_file_for_read(fname);
    }
    else {/** If not I am going to have to append path names to it **/
        HTPATH = oa_getenv("HTPATH");
        if (HTPATH == NULL) {
        /** The user does not have a HTPATH, so I will use the the directory
        $AXIOM/share/hypertex/pages/ as the default path ***/
          char *spad = oa_getenv("AXIOM");
          if (spad == NULL) {
            fprintf(stderr,
            "ht_file_open:Cannot find ht data base: setenv HTPATH or AXIOM\n");
             exit(-1);
          }
          HTPATH = (char *) halloc(1024 * sizeof(char), "HTPATH");
          strcpy(HTPATH, spad);
          strcat(HTPATH, "/share/hypertex/pages");
        }

        /** Now that I have filled HTPATH, I should try to open a file by the
          given name **/
        strcpy(aname, name);
        extend_ht(aname);
        for (ht_file = -1, trace2 = HTPATH;
             ht_file == -1 && *trace2 != '\0';) {
            char* ptr = fname;
            while (*trace2 != '\0' && (*trace2 != ':'))
                *ptr++ = *trace2++;
            *ptr++ = '/';
            *ptr = 0;
            if (!strcmp(fname, "./")) {
                /** The person wishes me to check the current directory too **/
               c_dir = oa_getcwd();
               strcpy(fname,c_dir);
               free(c_dir);
               strcat(fname, "/");
            }
            if (*trace2)
                trace2++;
            strcat(fname, aname);
            ht_file = oa_access_file_for_read(fname);
        }
        return (ht_file);
    }
}

static int pathname(const char* name)
{
    while (*name)
        if (*name++ == '/')
            return 1;

    return 0;
}

/** This procedure opens the proper HT file **/

FILE *
ht_file_open(char *fname, char *aname, const char *name)
{
    FILE *ht_fp;
    int ret_value;

    ret_value = build_ht_filename(fname, aname, name);
    if (ret_value == -1) {
        fprintf(stderr, "ht_file_open: Unknown file %s\n", fname);
        exit(-1);
    }

    ht_fp = fopen(fname, "r");
    if (ht_fp == NULL) {
        perror("ht_file_open");
        exit(-1);
    }
    return (ht_fp);
}

FILE *
temp_file_open(char *temp_db_file)
{
    FILE *temp_db_fp;

    /** Just make the name and open it **/

    strcpy(temp_db_file, oa_get_tmpdir());
    strcat(temp_db_file, "/ht2.db" /* db_file_name */ );
    temp_db_fp = fopen(temp_db_file, "w");

    if (temp_db_fp == NULL) {
        perror("temp_file_open");
        exit(-1);
    }
    return temp_db_fp;
}
