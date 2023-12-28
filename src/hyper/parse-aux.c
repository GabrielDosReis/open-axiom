/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2016, Gabriel Dos Reis.
  All rights reverved.

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

#include <vector>
#include <string>
#include <unordered_set>
#include <string_view>

#include "debug.h"
#include "halloc.h"
#include "sockio.h"
#include "parse.h"
#include "addfile.h"
#include "lex.h"
#include "hyper.h"
#include "cfuns.h"

using namespace OpenAxiom;

namespace {
    struct HTEnvironment {
        HashTable* pages { };
        HashTable* macros { };
        HashTable* patches { };
        std::unordered_set<std::string_view> ht_files { };
    };
}

static void read_ht_file(HTEnvironment& env, FILE * db_fp , const std::string& db_file);
static HyperDocPage * make_special_page(int type , const char * name);

extern int make_input_file;
extern int gverify_dates;

InputBox *rb_list;
InputBox *end_rb_list;

#define htfhSize 100

/* Hash functions for active link windows */

int
window_equal(Window *w1, Window *w2)
{
    return *w1 == *w2;
}

/* hash code for a window */

int
window_code(Window *w, int size)
{
    return (*w) % size;
}

char *
window_id(Window w)
{
    char *ret;
    char buff[32];
    int length;

    sprintf(buff, "%ld", w);
    length = strlen(buff);
    ret = (char *) halloc(length * sizeof(char) + 1, "windowid");
    strcpy(ret, buff);
    return (ret);
}

// Sequence of directory pathnames.
using HTDirectories = std::vector<std::string>;

// Return a sequuence of pathnames for directories potentially
// containing `ht.db` files, as indicated by the environment variables
// `HTPATH` or `AXIOM`. The value of `HTPATH` maybe a sequence of directories
// separated by the host's filesystem path separator.
static HTDirectories get_ht_db_directories()
{
    // FIXME: Handle Windows filesystem path peculiarities.
    constexpr auto strip_slash = [](auto cur) {
        return *cur == '/' ? --cur : cur;
    };
    HTDirectories dirs {};
    if (auto cur = oa_getenv("HTPATH")) {
        auto start = cur;
        while (*cur != '\0') {
            if (*cur == OPENAXIOM_INTERNAL_PATH_SEPARATOR[0]) {
                dirs.emplace_back(start, strip_slash(cur));
                start = cur + 1;
            }
            ++cur;
        }
        if (start < cur)
            dirs.emplace_back(start, strip_slash(cur));
    }
    else if (auto root = oa_getenv("AXIOM")) {
        dirs.push_back(std::string{root} + "/share/hypertex/pages");
    }

    return dirs;
}

/*
 * This procedure reads the ht database. 
 */
void
read_ht_db(HashTable *page_hash, HashTable *macro_hash, HashTable *patch_hash)
{
    hash_init(
              page_hash, 
              PageHashSize, 
              (EqualFunction) string_equal, 
              (HashcodeFunction) string_hash);
    hash_init(
              macro_hash, 
              MacroHashSize, 
              (EqualFunction) string_equal, 
              (HashcodeFunction) string_hash);
    hash_init(
              patch_hash, 
              PatchHashSize, 
              (EqualFunction) string_equal, 
              (HashcodeFunction) string_hash);
    HTEnvironment env { page_hash, macro_hash, patch_hash };

    int i = 0;
    for (auto& dir : get_ht_db_directories()) {
        auto path = dir + "/ht.db";
        auto db_fp = fopen(path.c_str(), "r");
        if (db_fp == nullptr)
            continue;
        ++i;
        read_ht_file(env, db_fp, path);
        fclose(db_fp);
    }

    if (i == 0) {
        fprintf(stderr, 
          "(HyperDoc) read_ht_db: No %s file found\n", db_file_name);
        exit(-1);
    }
}

// If `file` is not absolute, build a pathname for it under directory `dir`.
// Note: The argument for the `file` parameter is assumed to have been
//       obtained via `alloc_string`, so that the return value of this
//       function always lies in the same lifetime.
static const char* ht_filepath_if_not_absolute(const char* file, const std::string& dir)
{
    // FIXME: Handle Windows filesystem path peculiarities.
    if (file[0] == '/')
        return file;
    auto path = dir + '/' + file;
    return alloc_string(path.c_str());
}

/*
 * This procedure reads a single HyperDoc database file. It is passed an already
 * initilaized file pointer. It reads the whole file, updating the
 * page hash, or the macro hash only when a previous entry with the same name
 * is not found
 */

static void
read_ht_file(HTEnvironment& env, FILE *db_fp, const std::string& db_file)
{
    UnloadedPage *page;
    MacroStore *macro;
    PatchStore *patch;
    int pages = 0, c, mtime, ret_val;
    struct stat fstats;
    auto page_hash = env.pages;
    auto macro_hash = env.macros;
    auto patch_hash = env.patches;

    cfile = db_fp;
    init_scanner();
    c = getc(db_fp);
    do {
        if (c == '\t') {
            get_filename();
            auto fullname = ht_filepath_if_not_absolute(token.id, db_file);

            /*
             * Until I get a filename that I have not seen before, just keep
             * reading
             */
            while (env.ht_files.contains(fullname)) {
                do {
                    c = getc(db_fp);
                } while ((c != EOF) && (c != '\t'));
                if (c == EOF)
                    return;
                get_filename();
                fullname = ht_filepath_if_not_absolute(token.id, db_file);
            }
/*          fprintf(stderr,"parse_aux:read_ht_file: fullname=%s\n",fullname);*/
            /* If I got here, then I must have a good filename  */
            env.ht_files.insert(fullname);

            ret_val = stat(fullname, &fstats);
            if (ret_val == -1) {
                char buffer[300];

                sprintf(buffer, "(HyperDoc) read_ht_db: Unable To Open %s :", fullname);
                perror(buffer);
                exit(-1);
            }
            get_token();
            mtime = atoi(token.id);
            if (gverify_dates & (fstats.st_mtime > mtime)) {
                fprintf(stderr, "(HyperDoc) read_ht_file: HyperDoc file %s has been updated\n",

                        fullname);
                fprintf(stderr, "(HyperDoc) Issue htadd %s to update database\n", fullname);
                exit(-1);
            }
            while ((c = getc(db_fp)) != EOF) {
                if (c == '\t')
                    break;
                ungetc(c, db_fp);
                get_token();
                switch (token.type) {
                  case openaxiom_Page_token:
                    get_token();

                    /*
                     * now check to see if the page has already been
                     * loaded
                     */
                    page = (UnloadedPage *) halloc(sizeof(UnloadedPage),
                                                   "UnloadedPage");
                    page->fpos.name = alloc_string(fullname);
                    page->name = alloc_string(token.id);
                    get_token();
                    if (hash_find(page_hash, page->name) != NULL) {
                        fprintf(stderr, "(HyperDoc) Page name %s  occurred twice\n", page->name);
                        fprintf(stderr, "(HyperDoc) The Version in %s is being ignored \n",
                                page->fpos.name);
                        free(page);
                        get_token();
                        break;
                    }
                    page->fpos.pos = atoi(token.id);
                    get_token();
                    page->fpos.line_number = atoi(token.id);
                    page->type = UnloadedPageType;
                    hash_insert(page_hash, (char *)page, page->name);
                    pages++;
                    break;
                  case openaxiom_NewCommand_token:
                    get_token();
                    macro = (MacroStore *) halloc(sizeof(MacroStore), "MacroStore");
                    macro->fpos.name = alloc_string(fullname);
                    macro->name = alloc_string(token.id);
                    macro->macro_string = NULL;
                    get_token();
                    if (hash_find(macro_hash, macro->name) != NULL) {
                        if (strcmp(macro->name, "localinfo") != 0) {
                            fprintf(stderr, "(HyperDoc) Macro name %s  occurred twice\n",
                                    macro->name);
                            fprintf(stderr, "(HyperDoc) The Version in %s is being ignored \n",
                                    macro->fpos.name);
                        }
                        get_token();
                        free(macro);
                        break;
                    }
                    macro->fpos.pos = atoi(token.id);
                    get_token();
                    macro->fpos.line_number = atoi(token.id);
                    macro->loaded = 0;
                    hash_insert(macro_hash, (char *)macro, macro->name);
                    break;
                  case openaxiom_Patch_token:
                    get_token();
                    patch = (PatchStore *) alloc_patchstore();
                    patch->fpos.name = alloc_string(fullname);
                    patch->name = alloc_string(token.id);
                    get_token();
                    patch->fpos.pos = atoi(token.id);
                    get_token();
                    patch->fpos.line_number = atoi(token.id);
                    if (hash_find(patch_hash, patch->name) != NULL) {
                        fprintf(stderr, "(HyperDoc) Patch name %s  occurred twice\n", patch->name);
                        fprintf(stderr, "(HyperDoc) The version in %s is being ignored \n",
                                patch->fpos.name);
                        free_patch(patch);
                        break;
                    }
                    hash_insert(patch_hash, (char *)patch, patch->name);
                    break;
                  default:
                    fprintf(stderr, "(HyperDoc) read_ht_db: Unknown type %s in ht.db\n", token.id);
                    exit(-1);
                    break;
                }
            }
        }
        else
            c = getc(db_fp);
    } while (c != EOF);
/*    fprintf(stderr,
     "parse_aux:read_ht_file:read %d pages from database\n", pages); */
}


/* create an unmapped input-only window for an active screen area */

HyperLink *
make_link_window(TextNode *link_node, int type, int isSubWin)
{
    HyperLink *link;
    XSetWindowAttributes at;

    if (make_input_file)
        switch (type) {
          case openaxiom_Downlink_token:
          case openaxiom_Memolink_token:
          case openaxiom_Windowlink_token: {
                char *name;
                HyperDocPage *p;

                name = print_to_string(link_node);
                p = (HyperDocPage *) hash_find(gWindow->fPageHashTable, name);
                if (!p)
                    printf("undefined link to %s\n", name);
                break;
            }
        }
    else {
        link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");
        if (link == NULL) {
            fprintf(stderr, "(HyperDoc) Ran out of memory allocating a hypertext link!\n");
            exit(-1);
        }
        at.cursor = gActiveCursor;
        at.event_mask = ButtonPress;
        if (isSubWin)
            link->win = XCreateWindow(gXDisplay, gWindow->fDisplayedWindow, 0, 0, 100, 100, 0,
                                      0, InputOnly, CopyFromParent,
                                      CWEventMask | CWCursor, &at);
        else
            link->win = 0;
        link->type = type;
        link->x = link->y = 0;
        link->reference.node = link_node;
        hash_insert(gLinkHashTable, (char *)link,(char *)&link->win);
        return link;
    }
    return 0;
}

HyperLink *
make_paste_window(PasteNode *paste)
{
    HyperLink *link;
    XSetWindowAttributes at;

    if (!make_input_file) {
        link = (HyperLink *) halloc(sizeof(HyperLink), "HyperLink");
        if (link == NULL) {
            fprintf(stderr, "(HyperDoc) Ran out of memory allocating a hypertext link!\n");
            exit(-1);
        }
        at.cursor = gActiveCursor;
        at.event_mask = ButtonPress;
        link->win = XCreateWindow(gXDisplay, gWindow->fDisplayedWindow,
                                  0, 0, 100, 100, 0,
                                  0, InputOnly, CopyFromParent,
                                  CWEventMask | CWCursor, &at);
        link->type = openaxiom_Pastebutton_token;
        link->x = link->y = 0;
        link->reference.paste = paste;
        hash_insert(gLinkHashTable, (char *)link,(char *) &link->win);
        return link;
    }
    return 0;
}



/* create a HyperDoc page structure with the given type and name */

static HyperDocPage *
make_special_page(int type, const char *name)
{
    HyperDocPage *page = alloc_page(name);

    if (page == NULL) {
        fprintf(stderr, "(HyperDoc) Ran out of memory allocating page.\n");
        exit(-1);
    }
    page->type = type;
    free(page->fLinkHashTable);
    page->fLinkHashTable = NULL;
    return page;
}


/* insert the special button page types into the page hash table */

void
make_special_pages(HashTable *pageHashTable)
{
    hash_insert(pageHashTable,
                (char *)make_special_page(openaxiom_Quitbutton_token,
                                          "QuitPage"),
                "QuitPage");
    hash_insert(pageHashTable,
                (char *)make_special_page(openaxiom_Returnbutton_token,
                                          "ReturnPage"),
                "ReturnPage");
    hash_insert(pageHashTable,
                (char *)make_special_page(openaxiom_Upbutton_token, "UpPage"),
                "UpPage");
}


/* Here is where I put the item into the pages linked list */

/* Parse the \bound{varlist} command, and add vars to dependency table */

void
add_dependencies()
{
    TextNode *bound_node = curr_node;
    TextNode *node;
    SpadcomDepend *depend;

    if (cur_spadcom == NULL) {
        fprintf(stderr, "(HyperDoc) \\bound occuring outside a \\spadcom\n");
        print_page_and_filename();
        exit(-1);
    }
    curr_node->type = openaxiom_Bound_token;
    curr_node->data.node = alloc_node();
    curr_node = curr_node->data.node;
    get_expected_token(openaxiom_Lbrace_token);
    parse_HyperDoc();
    curr_node->type = openaxiom_Endarg_token;
    curr_node = bound_node;

    if (gPageBeingParsed->depend_hash == NULL) {
        gPageBeingParsed->depend_hash =
            (HashTable *) halloc(sizeof(HashTable), "Hash Table");
        hash_init(
                  gPageBeingParsed->depend_hash, 
                  DependHashSize,
                  (EqualFunction) string_equal, 
                  (HashcodeFunction) string_hash);
    }
    for (node = bound_node->data.node;
         node->type != openaxiom_Endarg_token;
         node = node->next) {
        if (node->type == openaxiom_Word_token) {
            depend = (SpadcomDepend *) halloc(sizeof(SpadcomDepend), "SpadcomDepend");
            depend->label = alloc_string(node->data.text);
            depend->spadcom = cur_spadcom;
            depend->executed = 0;
            hash_insert(gPageBeingParsed->depend_hash, (char *)depend, depend->label);
        }
    }
}

/* Returns true iff the TextNode contains a single integer */

int
is_number(const char * str)
{
    const char *s;

    for (s = str; *s != '\0'; s++) {
        if (!(isdigit(*s) || *s == '-'))
            return 0;
    }
    return 1;
}
void
parser_error(char *str)
{
    /** this procedure is called by the parser when an error occurs. It prints
      the error message, followed by the next 10 tokens to ease finding the
      error for the user.                                               *****/

    int i, v;

    fprintf(stderr, " %s\n", str);
    fprintf(stderr, "Here are the next 10 tokens:\n");
    for (i = 0; i < 10; i++) {
        v = get_token();
        if (v == EOF)
            break;
        print_token();
    }
    fprintf(stderr, "\n");
    exit(-1);
}


#define whitespace(c) ((c) == ' ' || (c) == '\t' || (c) == '\n')
#define delim(c) \
  (whitespace(c))


/* advance token to the next token in the input stream.  */
int
get_filename()
{
    int c, ws;
    static int seen_white = 0; /*UNUSED */
    static char buffer[256];
    char *buf = buffer;

    if (last_token) {
        last_token = 0;
        return 0;
    }
    do {
        keyword_fpos = fpos;
        c = get_char();
        ws = whitespace(c);
        if (ws)
            seen_white = 1;
    } while (ws);
    switch (c) {
      case EOF:
        fprintf(stderr, "(HyperDoc) Error trying to read %s, unexpected end-of-file.\n",db_file_name);
        exit(-1);
      case '%':
      case '\\':
      case '{':
      case '}':
        fprintf(stderr, "(HyperDoc) Error unexpected character %c.\n",c);
        exit(-1);
      default:
        do {
            *buf++ = c;
        } while ((c = get_char()) != EOF && !delim(c));
        unget_char(c);
        *buf = '\0';
        token.type = openaxiom_Word_token;
        token.id = buffer;
        seen_white = 0;
        break;
    }
    return 1;
}

char *
get_input_string()
{
    char *string;
    TextNode *string_node,*save_node;

    save_node = curr_node;
    /* Get the nodes that make up the string */
    string_node = alloc_node();
    curr_node = string_node;
    parse_HyperDoc();
    curr_node->type = openaxiom_Endarg_token;

    /* Once here we print to string to get the actual name */
    string = print_to_string(string_node);
    free_node(string_node, 0);
    curr_node=save_node;
    return string;
}

/*
 * tries to determine if there is an optional argument for where I should be
 * parsing from. If so it then tries to determine which
 */
SourceInputKind
get_where()
{
    SourceInputKind tw;

    get_token();
    if (token.type != openaxiom_Word_token)
       return SourceInputKind::Error;

    /* Now try to determine if it is a good type */
    if (!strcmp(token.id, "lisp")) {
       tw = SourceInputKind::SpadSocket;
    }
    else if (!strcmp(token.id, "unix")) {
       tw = SourceInputKind::UnixFD;
    }
    else if (!strcmp(token.id, "ht")) {
       tw = SourceInputKind::File;
    }
    else {
       return SourceInputKind::Error;
    }

    /* now check to see if I got a closing square brace */
    get_token();
    if (token.type != openaxiom_Rsquarebrace_token)
       return SourceInputKind::Error;

    return tw;
}


FILE *
find_fp(FilePosition fp)
{
    FILE *lfile;
    char fullname[256], addname[256];
    int ret_val;

    /* find the source file in the file hash table, if not there, open it */
    lfile = (FILE *) hash_find(&gFileHashTable, fp.name);
    if (lfile == NULL) {
        lfile = ht_file_open(fullname, addname, fp.name);
        hash_insert(&gFileHashTable, (char *)lfile, fp.name);
    }

    /* seek to beginning fp.pos */
    ret_val = fseek(lfile, fp.pos, 0);
    if (ret_val == -1) {
        perror("fseeking to a page");
        throw HyperError{};
    }

    /* now set some global values */
    page_start_fpos = fp.pos;
    line_number = fp.line_number;
    return lfile;
}
