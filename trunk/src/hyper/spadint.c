/*
   Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
   All rights reserved.
   Copyright (C) 2007-2010, Gabriel Dos Reis.
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

/* Still a problem with close_client */

/* Communication interface for external OpenAxiom buffers */

#include "open-axiom.h"
#include "debug.h"
#include <signal.h>
#include "halloc.h"
#include "sockio.h"
#include "hyper.h"
#include "parse.h"
#include "bsdsignal.h"
#include "sockio.h"
#include "cfuns.h"

using namespace OpenAxiom;

static void start_user_buffer(HyperDocPage * page);
static void clear_execution_marks(HashTable * depend_hash);
static void issue_dependent_commands(HyperDocPage * page , TextNode * command , int type);
static void send_pile(openaxiom_sio * sock , char * str);
static void mark_as_executed(HyperDocPage * page , TextNode * command , int type);
static void accept_menu_server_connection(HyperDocPage * page);
static void switch_frames(void );
static void close_client(int pid);

typedef struct sock_list {      /* linked list of openaxiom_sio */
    openaxiom_sio Socket;
    struct sock_list *next;
}   Sock_List;

Sock_List *plSock = (Sock_List *) 0;

/* connect to OpenAxiom , return 0 if succesful, 1 if not */
int
connect_spad(void)
{
    if (!MenuServerOpened) {
        fprintf(stderr, "(HyperDoc) Warning: Not connected to OpenAxiom Server!\n");
        LoudBeepAtTheUser();
        return NotConnected;
    }
    if (spad_socket == NULL) {
        spad_socket = connect_to_local_server(SpadServer, MenuServer, Forever);
        if (spad_socket == NULL) {
            fprintf(stderr, "(HyperDoc) Warning: Could not connect to OpenAxiom Server!\n");
            LoudBeepAtTheUser();
            return NotConnected;
        }
    }
    /* if (spad_busy()) return SpadBusy; */
    return Connected;
}

/* returns true if spad is currently computing */
int
spad_busy(void)
{
    if (session_server == NULL)
        return 1;
    send_int(session_server, QuerySpad);
    return get_int(session_server);
}


/* issue a OpenAxiom command to the buffer associated with a page */
void
issue_spadcommand(HyperDocPage *page, TextNode *command, int immediate,
                  int type)
{
    char *buf;
    int ret_val;

    ret_val = connect_spad();
    if (ret_val == NotConnected || ret_val == SpadBusy)
        return;

    if (page->sock == NULL)
        start_user_buffer(page);
    ret_val = send_int(page->sock, TestLine);
    if (ret_val == -1) {
        page->sock = NULL;
        clear_execution_marks(page->depend_hash);
        issue_spadcommand(page, command, immediate, type);
        return;
    }
    issue_dependent_commands(page, command, type);
    ret_val = send_int(page->sock, ReceiveInputLine);
    buf = print_to_string(command);
    if (immediate) {
        buf[strlen(buf) + 1] = '\0';
        buf[strlen(buf)] = '\n';
    }
    if (type == openaxiom_Spadsrc_token)
        send_pile(page->sock, buf);
    else
        send_string(page->sock, buf);
    mark_as_executed(page, command, type);
    gIsEndOfOutput = 0;
}
static void
send_pile(openaxiom_sio *sock,char * str)
{
    FILE *f;
    char name[512], command[512];

    sprintf(name, "/tmp/hyper%s.input", oa_getenv("SPADNUM"));
    f = fopen(name, "w");
    if (f == NULL) {
        fprintf(stderr, "Can't open temporary input file %s\n", name);
        return;
    }
    fprintf(f, "%s", str);
    fclose(f);
    sprintf(command, ")read %s\n", name);
    send_string(sock, command);
}
static void
issue_dependent_commands(HyperDocPage *page, TextNode *command,int type)
{
    TextNode *node, *depend_label;
    SpadcomDepend *depend;
    int end_type = (type == openaxiom_Spadcommand_token
                    || type == openaxiom_Spadgraph_token) ?
    (openaxiom_Endspadcommand_token) : (openaxiom_Endspadsrc_token);

    for (node = command->next; node->type != end_type;
         node = node->next)
        if (node->type == openaxiom_Free_token)
            for (depend_label = node->data.node; depend_label != NULL;
                 depend_label = depend_label->next)
                if (depend_label->type == openaxiom_Word_token) {
                    depend = (SpadcomDepend *)
                        hash_find(page->depend_hash, depend_label->data.text);
                    if (depend == NULL) {
                        fprintf(stderr, "Error: dependency on undefined label: %s\n",
                                depend_label->data.text);
                        continue;
                    }
                    if (!depend->executed) {
                        issue_spadcommand(page, depend->spadcom->next, 1,
                                          depend->spadcom->type);
                        while (!gIsEndOfOutput)
                            pause();
                        openaxiom_sleep(1);
                    }
                }
}
static void
mark_as_executed(HyperDocPage *page, TextNode *command,int type)
{
    TextNode *node, *depend_label;
    SpadcomDepend *depend;
    int end_type = (type == openaxiom_Spadcommand_token
                    || type == openaxiom_Spadgraph_token)
    ? (openaxiom_Endspadcommand_token) : (openaxiom_Endspadsrc_token);

    for (node = command; node->type != end_type; node = node->next)
        if (node->type == openaxiom_Bound_token)
            for (depend_label = node->data.node; depend_label != NULL;
                 depend_label = depend_label->next)
                if (depend_label->type == openaxiom_Word_token) {
                    depend = (SpadcomDepend *)
                        hash_find(page->depend_hash, depend_label->data.text);
                    if (depend == NULL) {
                        fprintf(stderr, "No dependency entry for label: %s\n",
                                depend_label->data.text);
                        continue;
                    }
                    depend->executed = 1;
                }
}

/* start a spad buffer for the page associated with the give */
static void
start_user_buffer(HyperDocPage *page)
{
    char buf[1024], *title;
    char *SPAD;
    char spadbuf[250];
    char complfile[250];
    int ret_val;

    SPAD = (char *) oa_getenv("AXIOM");
    if (SPAD == NULL) {
        sprintf(SPAD, "/spad/mnt/rios");
    }
    sprintf(spadbuf, "%s/lib/spadbuf", SPAD);
    sprintf(complfile, "%s/lib/command.list", SPAD);
    title = print_to_string(page->title);
    if (access(complfile, R_OK) == 0)

        /*
         * TTT says : why not invoke with "-name axiomclient" and set any
         * defaults in the usual way
         */
#ifdef RIOSplatform
        sprintf(buf,
                "aixterm -sb -sl 500 -name axiomclient -n '%s' -T '%s'  -e  %s %s %s&",
                title, title, spadbuf, page->name, complfile);
    else
        sprintf(buf,
         "aixterm -sb -sl 500 -name axiomclient -n '%s' -T '%s' -e  %s %s&",
                title, title, spadbuf, page->name);
#else
#ifdef SUNplatform
        sprintf(buf,
        "xterm -sb -sl 500 -name axiomclient -n '%s' -T '%s' -e  %s %s %s&",
                title, title, spadbuf, page->name, complfile);
    else
        sprintf(buf,
           "xterm -sb -sl 500 -name axiomclient -n '%s' -T '%s' -e  %s %s&",
                title, title, spadbuf, page->name);
#else
        sprintf(buf,
        "xterm -sb -sl 500 -name axiomclient -n '%s' -T '%s' -e  %s %s %s&",
                title, title, spadbuf, page->name, complfile);
    else
        sprintf(buf,
         "xterm -sb -sl 500 -name axiomclient -n '%s' -T '%s' -e  %s '%s'&",
                title, title, spadbuf, page->name);
#endif
#endif
    ret_val = system(buf);
    if (ret_val == -1 || ret_val == 127) {

        /*
         * perror("running the xterm spadbuf program"); exit(-1);
         */
    }
    accept_menu_server_connection(page);
    openaxiom_sleep(2);
}

/* Clears the execution marks in a hash table when a buffer has been killed */
static void
clear_execution_marks(HashTable *depend_hash)
{
    int i;
    HashEntry *h;
    SpadcomDepend *depend;

    if (depend_hash == 0)
        return;
    for (i = 0; i < depend_hash->size; i++)
        for (h = depend_hash->table[i]; h != NULL; h = h->next) {
            depend = (SpadcomDepend *) h->data;
            depend->executed = 0;
        }
}

openaxiom_sio *
accept_menu_connection(openaxiom_sio *server_sock)
{
    int sock_fd /*, session, ret_code*/;
    Sock_List *pls;
    /*openaxiom_sio local_sock;*/

    /* Could only be InterpWindow */

    pls = (Sock_List *) halloc(sizeof(Sock_List),"SockList");
    sock_fd = accept(server_sock->socket, 0, 0);
    if (sock_fd == -1) {
        perror("session : accepting connection");
        return 0;
    }
    (pls->Socket).socket = sock_fd;
    get_socket_type((openaxiom_sio *) pls);

#ifdef DEBUG
    fprintf(stderr,
            "session: accepted InterpWindow , fd = %d\n", sock_fd);
#endif


    /* put new item at head of list */

    if (plSock == (Sock_List *) 0) {
        plSock = pls;
        plSock->next = (Sock_List *) 0;
    }
    else {
        pls->next = plSock;
        plSock = pls;
    }

    /* need to maintain socket_mask since we roll our own accept */

    FD_SET(plSock->Socket.socket, &socket_mask);
    return (openaxiom_sio *) plSock;
}

static void
accept_menu_server_connection(HyperDocPage *page)
{

    /*
     * TTT thinks this code should just provide a openaxiom_sio to the page.
     * The only client assumed is a spadbuf. Since spadbuf was invoked with
     * the page name, it just passes it back here as a check (get_string line).
     */
    int ret_code/*, i*/;
    fd_set rd;
    openaxiom_sio *sock;
    char *buf_name;
    HyperDocPage *npage;

    if (page->sock != NULL)
        return;
    while (1) {
        rd = server_mask;
        ret_code = sselect(FD_SETSIZE, &rd, 0, 0, NULL);
        if (ret_code == -1) {
            perror("Session manager select");
            continue;
        }

        if (server[1].socket > 0 && FD_ISSET(server[1].socket, &rd)) {
            sock = accept_menu_connection(server + 1);
            if (sock == 0)
                return;
            if (sock->purpose == InterpWindow) {
                buf_name = get_string(sock);
                npage = (HyperDocPage *)
                    hash_find(gWindow->fPageHashTable, buf_name);
                if (npage == NULL) {

                    /*
                     * Lets just try using the current page TTT doesn't know
                     * why this could be detrimental
                     *
                     * fprintf(stderr, "connecting spadbuf to unknown page:
                     * %s\n", buf_name);
                     */
                    page->sock = sock;
                    return;
                }
                else {

                    /*
                     * For some reason npage and page may be different TTT
                     * thinks this happens when a dynamic page has the same
                     * name as an existing static page.
                     */
                    npage->sock = sock;
                    page->sock = sock;
                }
                if (!strcmp(buf_name, page->name)) {
                    return;
                }
            }
        }
    }
}


/*
 * This procedure takes a HyperDoc node, and expands it into string
 */

char *p2sBuf = NULL;
int p2sBufSize = 0;

/*
 * This routine takes a text node and creates a string out of it. This is for
 * use with things such as spad commands. There are  a very limited set of
 * node types it can handle, so be careful
 */

char *
print_to_string(TextNode *command)
{
    int len = 0;

    print_to_string1(command, &len);
    p2sBuf = resizeBuffer(len, p2sBuf, &p2sBufSize);
    return print_to_string1(command, NULL);
}

/* 
see the code in ht-util.boot
        $funnyQuote := char 127
        $funnyBacks := char 128
*/
#define funnyEscape(c)  ((c) == '"' ? '\177' : ((c) == '\\' ? '\200' : c))
#define funnyUnescape(c) ((c) == '\177' ? '"' : ((c) == '\200' ? '\\' : c))
#define storeChar(ch) if (sizeBuf) (*sizeBuf)++; else  *c++ = (ch)
#define storeString(str) for (s=str;*s;s++) {storeChar(*s);}

extern int include_bf;

char *
print_to_string1(TextNode *command,int * sizeBuf)
{
    char *c = p2sBuf;
    char *s;
    InputItem *item;
    LineStruct *curr_line;
    int lcount;
    InputBox *box;
    int num_spaces;
    int count;
    TextNode *node;

    /*
     * Init the stack of text nodes, things are pushed on here when I trace
     * through a nodes data.node. This way I always no where my next is.
     */

    for (node = command; node != NULL;) {
        switch (node->type) {
          case openaxiom_Newline_token:
            storeChar('\n');
            node = node->next;
            break;
          case openaxiom_Ifcond_token:
            if (check_condition(node->data.ifnode->cond))
                node = node->data.ifnode->thennode;
            else
                node = node->data.ifnode->elsenode;
            break;
          case openaxiom_Endarg_token:
          case openaxiom_Endspadcommand_token:
          case openaxiom_Endspadsrc_token:
          case openaxiom_Endpix_token:
            storeChar('\0');
            return p2sBuf;
          case openaxiom_Endverbatim_token:
          case openaxiom_Endif_token:
          case openaxiom_Fi_token:
          case openaxiom_Endmacro_token:
          case openaxiom_Endparameter_token:
          case openaxiom_Rbrace_token:
          case openaxiom_Endgroup_token:
            node = node->next;
            break;
          case openaxiom_Punctuation_token:

            /*
             * Simply copy the piece of text
             */
            if (node->space & FRONTSPACE) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case openaxiom_WindowId_token:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            storeChar(' ');
            node = node->next;
            break;
          case openaxiom_Verbatim_token:
          case openaxiom_Spadsrctxt_token:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }

            /*
             * now add the eol
             */

            /*
             * if(node->next && node->next->type != openaxiom_Endspadsrc_token)
             * storeChar('\n');
             */
            node = node->next;
            break;
          case openaxiom_Dash_token:
          case openaxiom_Rsquarebrace_token:
          case openaxiom_Lsquarebrace_token:
          case openaxiom_Word_token:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case openaxiom_BoxValue_token:
            box = 
             (InputBox *) hash_find(gWindow->page->box_hash, node->data.text);
            if (box == NULL) {
                fprintf(stderr, 
                        "Print_to_string:Box %s Has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            storeChar(' ');
            if (box->picked) {
                storeChar('t');
            }
            else {
                storeChar('n');
                storeChar('i');
                storeChar('l');
            }
            node = node->next;
            break;
          case openaxiom_StringValue_token:
            item = return_item(node->data.text);
            if (item != NULL) {
                if (node->space)
                    storeChar(' ');
                curr_line = item->lines;
                while (curr_line != NULL) {
                    for (lcount = 0, s = curr_line->buffer; *s && lcount < item->size;
                         s++, lcount++) {
                        storeChar(funnyUnescape(*s));
                    }
                    if (curr_line->len <= item->size && curr_line->next)
                        storeChar('\n');
                    curr_line = curr_line->next;
                }
            }
            else if ((box = (InputBox *) hash_find(gWindow->page->box_hash,
                                                node->data.text)) != NULL) {
                if (node->space) { storeChar(' '); }
                if (box->picked) {
                    storeChar('t');
                }
                else {
                    storeChar('n');
                    storeChar('i');
                    storeChar('l');
                }
            }
            else {
                fprintf(stderr, "Error, Symbol %s has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            node = node->next;
            break;
          case openaxiom_Space_token:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case openaxiom_Titlenode_token:
          case openaxiom_Endtitle_token:
          case openaxiom_Center_token:
          case openaxiom_Endcenter_token:
          case openaxiom_BoldFace_token:
          case openaxiom_Emphasize_token:
          case openaxiom_Indentrel_token:
            node = node->next;
            break;
          case openaxiom_Bound_token:
            if (include_bf) {
                int len, i;
                TextNode *n2 = node->data.node;

                storeChar('\\');
                storeChar('b');
                storeChar('o');
                storeChar('u');
                storeChar('n');
                storeChar('d');
                storeChar('{');
                for (; n2->type != openaxiom_Endarg_token; n2 = n2->next) {
                    if (n2->type == openaxiom_Word_token) {
                        len = strlen(n2->data.text);
                        for (i = 0; i < len; i++)
                            storeChar(n2->data.text[i]);
                        storeChar(' ');
                    }
                }
                storeChar('}');
            }
            node = node->next;
            break;
          case openaxiom_Free_token:
            if (include_bf) {
                int len, i;
                TextNode *n2 = node->data.node;

                storeChar('\\');
                storeChar('f');
                storeChar('r');
                storeChar('e');
                storeChar('e');
                storeChar('{');
                for (; n2->type != openaxiom_Endarg_token; n2 = n2->next) {
                    if (n2->type == openaxiom_Word_token) {
                        len = strlen(n2->data.text);
                        for (i = 0; i < len; i++)
                            storeChar(n2->data.text[i]);
                        storeChar(' ');
                    }
                }
                storeChar('}');
            }
            node = node->next;
            break;
          case openaxiom_Macro_token:
            node = node->next;
            break;
          case openaxiom_Pound_token:
            if (node->space) { storeChar(' '); }
            node = node->next;
            break;
          case openaxiom_Group_token:
            node = node->next;
            break;
          case openaxiom_Indent_token:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          default:
            fprintf(stderr,
                    "Print_to_string: Unrecognized Keyword Type %d\n",
                    node->type);
            node=node->next;
            break;
        }
    }
    storeChar('\0');
    return p2sBuf;
}

/*
 * Send a lisp or spad command to the OpenAxiom server for execution , if
 * type is link, then we wait for a HyperDoc card to be returned
 */

HyperDocPage *
issue_server_command(HyperLink *link)
{
    TextNode *command = (TextNode *) link->reference.node;
    int ret_val;
    char *buf;
    HyperDocPage *page;

    ret_val = connect_spad();
    if (ret_val == NotConnected) {
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "SpadNotConnectedPage");
        if (page == NULL)
            fprintf(stderr, "No SpadNotConnectedPage found\n");
        return page;
    }
    if (ret_val == SpadBusy) {
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "SpadBusyPage");
        if (page == NULL)
            fprintf(stderr, "No SpadBusyPage found\n");
        return page;
    }
    switch_frames();
    switch (link->type) {
      case openaxiom_Qspadcall_token:
      case openaxiom_Qspadcallquit_token:
      case openaxiom_Spadlink_token:
      case openaxiom_Spaddownlink_token:
      case openaxiom_Spadmemolink_token:
        send_int(spad_socket, QuietSpadCommand);
        break;
      case openaxiom_Spadcall_token:
      case openaxiom_Spadcallquit_token:
        send_int(spad_socket, SpadCommand);
        break;
      default:
        send_int(spad_socket, LispCommand);
        break;
    }
    buf = print_to_string(command);
    send_string(spad_socket, buf);
    if (link->type == openaxiom_Lispcommand_token
        || link->type == openaxiom_Spadcall_token
        || link->type == openaxiom_Spadcallquit_token
        || link->type == openaxiom_Qspadcallquit_token
        || link->type == openaxiom_Qspadcall_token
        || link->type == openaxiom_Lispcommandquit_token)
        return NULL;
    page = parse_page_from_socket();
    return page;
}

int
issue_serverpaste(TextNode *command)
{
    char *buf;
    int ret_val;

    ret_val = connect_spad();
    if (ret_val == NotConnected || ret_val == SpadBusy)
        return 1;
    switch_frames();
    send_int(spad_socket, LispCommand);
    buf = print_to_string(command);
    send_string(spad_socket, buf);
    return 1;
}

/*
 * issue a unix command
 */
void
issue_unixcommand(TextNode *node)
{
    char *buf;
    char *copy;


    buf = print_to_string(node);
    copy =(char *) halloc((strlen(buf)+2)*sizeof(char),"Unixcommand");
    strcpy(copy,buf);
    copy[strlen(buf) + 1] = '\0';
    copy[strlen(buf)] = '&';
    system(copy);
    free(copy);
    return;
}

HyperDocPage *
issue_unixlink(TextNode *node)
{
    HyperDocPage *page;
    char *buf;

    buf = print_to_string(node);
    if ((unixfd = popen(buf, "r")) == NULL) {
        fprintf(stderr, "Error popening %s\n", buf);
        exit(-1);
    }
    bsdSignal(SIGUSR2,SIG_IGN,0);
    page = parse_page_from_unixfd();
    bsdSignal(SIGUSR2,sigusr2_handler,0);
    return page;
}

int
issue_unixpaste(TextNode *node)
{
    char *buf;

    buf = print_to_string(node);
    if ((unixfd = popen(buf, "r")) == NULL) {
        fprintf(stderr, "Error popening %s\n", buf);
        exit(-1);
    }
    return 1;
}


/*
 * called when session_server selects
 */
void
service_session_socket(void)
{
    int cmd, pid;

    cmd = get_int(session_server);
    switch (cmd) {
      case CloseClient:
        pid = get_int(session_server);
        if (pid != -1)
            close_client(pid);
        break;
      default:
        fprintf(stderr,
                "(HyperDoc) Unknown command from SessionServer %d\n", cmd);
        break;
    }
}


/*
 * let spad know which frame to issue command via
 */
static void
switch_frames(void)
{
    if (session_server == NULL) {
        fprintf(stderr, "(HyperDoc) No session manager connected!\n");
        return;
    }
    if (gWindow->fAxiomFrame == -1) {
        fprintf(stderr, "(HyperDoc) No OpenAxiom frame associated with top level window!\n");
        return;
    }
    send_int(session_server, SwitchFrames);
    send_int(session_server, gWindow->fAxiomFrame);
}
void
send_lisp_command(char *command)
{
    int ret_val;

    ret_val = connect_spad();
    if (ret_val == NotConnected || ret_val == SpadBusy) {
        return;
    }
    send_int(spad_socket, LispCommand);
    send_string(spad_socket, command);
}
void
escape_string(char *s)
{
    char *st;

    for (st = s; *st; st++)
        *st = funnyEscape(*st);
}
void
unescape_string(char *s)
{
    char *st;

    for (st = s; *st; st++)
        *st = funnyUnescape(*st);
}
static void
close_client(int pid)
{
    /*int i;*/
    Sock_List *pSock, *locSock;

    /*
     * just need to drop the list item
     */

    if (plSock == (Sock_List *) 0)
        return;

    /*
     * first check head
     */

    if ((plSock->Socket.pid == pid)) {
        locSock = plSock;
        if ((*plSock).next == (Sock_List *) 0) {
            plSock = (Sock_List *) 0;
        }
        else {
            plSock = plSock->next;
        }
        free(locSock);
    }

    /*
     * now check the rest
     */

    else {
        for (pSock = plSock; pSock->next != (Sock_List *) 0; pSock = pSock->next)
            if (pSock->next->Socket.pid == pid) {
                locSock = pSock->next;
                if (pSock->next->next == (Sock_List *) 0) {
                    pSock->next = (Sock_List *) 0;
                }
                else {
                    pSock->next = pSock->next->next;
                }
                free(locSock);
                break;
            }
    }
}



char *
print_source_to_string(TextNode *command)
{
    int len = 0;

    print_source_to_string1(command, &len);
    p2sBuf = resizeBuffer(len, p2sBuf, &p2sBufSize);
    return print_source_to_string1(command, NULL);
}
char *
print_source_to_string1(TextNode *command,int * sizeBuf)
{
    char *c = p2sBuf;
    const char *s;
    InputItem *item;
    LineStruct *curr_line;
    int lcount;
    InputBox *box;
    int num_spaces;
    int count;
    TextNode *node;

    /*
        print out HyperDoc source for what you see
     */

    for (node = command; node != NULL;) {
        switch (node->type) {
          case openaxiom_Newline_token:
            storeString("\\newline\n");
            node = node->next;
            break;
          case openaxiom_Par_token:
            storeString("\n\n");
            node = node->next;
            break;
          case openaxiom_Indentrel_token:
            storeString("\\indentrel{");
            storeString(node->data.node->data.text);
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Tab_token:
            storeString("\\tab{");
            storeString(node->data.node->data.text);
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Ifcond_token:
            if (check_condition(node->data.ifnode->cond))
                node = node->data.ifnode->thennode;
            else
                node = node->data.ifnode->elsenode;
            break;
          case openaxiom_Endarg_token:
          case openaxiom_Endspadsrc_token:
          case openaxiom_Endpix_token:
          case openaxiom_Endbutton_token:
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Endverbatim_token:
          case openaxiom_Endif_token:
          case openaxiom_Fi_token:
          case openaxiom_Endmacro_token:
          case openaxiom_Endparameter_token:
          case openaxiom_Rbrace_token:
            node = node->next;
            break;
          case openaxiom_Punctuation_token:

            /*
             * Simply copy the piece of text
             */
            if (node->space & FRONTSPACE) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case openaxiom_WindowId_token:
            storeString("\\windowid ");
            node = node->next;
            break;
          case openaxiom_Verbatim_token:
          case openaxiom_Spadsrctxt_token:
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case openaxiom_Dash_token:
          case openaxiom_Rsquarebrace_token:
          case openaxiom_Lsquarebrace_token:
          case openaxiom_Word_token:
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case openaxiom_BoxValue_token:
            box = (InputBox *) hash_find(gWindow->page->box_hash, node->data.text);
            if (box == NULL) {
                fprintf(stderr, "Print_to_string:Box %s Has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            storeChar(' ');
            if (box->picked) {
                storeChar('t');
            }
            else {
                storeChar('n');
                storeChar('i');
                storeChar('l');
            }
            node = node->next;
            break;
          case openaxiom_StringValue_token:
            item = return_item(node->data.text);
            if (item != NULL) {
                if (node->space) {  storeChar(' '); }
                curr_line = item->lines;
                while (curr_line != NULL) {
                    for (lcount = 0, s = curr_line->buffer; 
                         *s && lcount < item->size;
                         s++, lcount++) {
                        storeChar(funnyUnescape(*s));
                    }
                    if (curr_line->len <= item->size && curr_line->next)
                        storeChar('\n');
                    curr_line = curr_line->next;
                }
            }
            else if ((box = (InputBox *) hash_find(gWindow->page->box_hash,
                                                node->data.text)) != NULL) {
                if (node->space) { storeChar(' '); }
                if (box->picked) {
                    storeChar('t');
                }
                else {
                    storeChar('n');
                    storeChar('i');
                    storeChar('l');
                }
            }
            else {
                fprintf(stderr, "Error, Symbol %s has no symbol table entry\n",
                        node->data.text);
                exit(-1);
            }
            node = node->next;
            break;
          case openaxiom_Space_token:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case openaxiom_Emphasize_token:
            storeString("\\em ");
            node = node->next;
            break;
          case openaxiom_BoldFace_token:
            storeString("\\bf ");
            node = node->next;
            break;
          case openaxiom_Sl_token:
            storeString("\\it ");
            node = node->next;
            break;
          case openaxiom_Rm_token:
            storeString("\\rm ");
            node = node->next;
            break;
          case openaxiom_It_token:
            storeString("\\it ");
            node = node->next;
            break;
          case openaxiom_Tt_token:
            storeString("\\tt ");
            node = node->next;
            break;
          case openaxiom_Group_token:
/* skip {} */
            if (node->next->type==openaxiom_Endgroup_token){
               node=node->next->next;
               break;
                }
            storeChar('{');
            node = node->next;
            break;
          case openaxiom_Endgroup_token:
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Box_token:
            storeString("\\box{");
            node = node->next;
            break;
          case openaxiom_Endbox_token:
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Center_token:
            storeString("\\center{");
            node = node->next;
            break;
          case openaxiom_Endcenter_token:
            storeString("}");
            storeChar('\n');
            node = node->next;
            break;
          case openaxiom_Titlenode_token:
          case openaxiom_Endtitle_token:
            node = node->next;
            break;
          case openaxiom_Bound_token:
            {
                TextNode *n2 = node->data.node;

                storeString("\\bound{");
                for (; n2->type != openaxiom_Endarg_token; n2 = n2->next) {
                    if (n2->type == openaxiom_Word_token) {
                        storeString(n2->data.text);
                        storeChar(' ');
                        }
                    }
                storeChar('}');
            }
            node = node->next;
            break;
          case openaxiom_Free_token:
            {
                TextNode *n2 = node->data.node;

                storeString("\\free{");
                for (; n2->type != openaxiom_Endarg_token; n2 = n2->next) {
                    if (n2->type == openaxiom_Word_token) {
                        storeString(n2->data.text);
                        storeChar(' ');
                        }
                    }
                storeChar('}');
                }
            node = node->next;
            break;
          case openaxiom_Macro_token:
            storeChar(' ');
            node = node->next;
            break;
          case openaxiom_Pound_token:
            if (node->space) { storeChar(' '); }
            node = node->next;
            break;
          case openaxiom_Indent_token:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case openaxiom_Inputbitmap_token:
            storeString("\\inputbitmap{");
            storeString(node->data.text); 
            storeString("}\n");
            node = node->next;
            break;
          case openaxiom_Endscrolling_token:
            storeString("\\end{scroll}\n");
            node = node->next;
            break;
          case openaxiom_Scrollingnode_token:
            storeString("\\begin{scroll}\n");
            storeString("% This is the scrolling area\n");
            node = node->next;
            break;
          case openaxiom_Horizontalline_token:
            storeString("\\horizontalline\n");
            node = node->next;
            break;
          case openaxiom_Endtable_token:
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Table_token:
            storeString("\\table{");
            node = node->next;
            break;
          case openaxiom_Tableitem_token:
            storeChar('{');
            node = node->next;
            break;
          case openaxiom_Endtableitem_token:
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Beginitems_token:
            storeString("\\begin{items}");
            node = node->next;
            break;
          case openaxiom_Item_token:
            storeString("\n\\item");
            node = node->next;
            break;
          case openaxiom_Enditems_token:
            storeString("\n\\end{items}");
            node = node->next;
            break;
/*** LINKS ***/
/* all these guys are ended by Endbutton 
we close the brace then */
          case openaxiom_Spadlink_token:
            storeString("\\fauxspadlink{");
            node = node->next;
            break;
          case openaxiom_Unixlink_token:
            storeString("\\fauxunixlink{");
            node = node->next;
            break;
          case openaxiom_Lisplink_token:
            storeString("\\fauxlisplink{");
            node = node->next;
            break;
          case openaxiom_Link_token:
            storeString("\\fauxlink{");
            node = node->next;
            break;
          case openaxiom_LispDownLink_token:
            storeString("\\fauxlispdownlink{");
            node = node->next;
            break;
          case openaxiom_LispMemoLink_token:
            storeString("\\fauxlispmemolink{");
            node = node->next;
            break;
          case openaxiom_Memolink_token:
            storeString("\\fauxmemolink{");
            node = node->next;
            break;
          case openaxiom_Windowlink_token:
            storeString("\\fauxwindowlink{");
            node = node->next;
            break;
          case openaxiom_Downlink_token:
            storeString("\\fauxdownlink{");
            node = node->next;
            break;
/** END OF LINKS **/
          case openaxiom_Unixcommand_token:
            storeString("\\unixcommand{");
            node = node->next;
            break;
          case openaxiom_Lispcommand_token:
            storeString("\\lispcommand{");
            node = node->next;
            break;
          case openaxiom_Spadgraph_token:
            storeString("\\spadgraph{");
            node = node->next;
            break;
          case openaxiom_Spadcommand_token:
            storeString("\\spadcommand{");
            node = node->next;
            break;
          case openaxiom_Endspadcommand_token:
            storeChar('}');
            node = node->next;
            break;
          case openaxiom_Footernode_token:
            storeString("% This is the footer\n");
            node = node->next;
            break;
          case openaxiom_Endfooter_token:
            storeString("% This is the end of the footer\n");
            node = node->next;
            break;
          case openaxiom_Endheader_token:
            storeString("% This is the end of the header\n");
            node = node->next;
            break;
          case openaxiom_Headernode_token:
            storeString("% This is the header\n");
            node = node->next;
            break;
          default:
            fprintf(stderr,
                    "Print_to_string: Unrecognized Keyword Type %d\n",
                    node->type);
            node=node->next;
            break;
        }
    }
    storeChar('\0');
    return p2sBuf;
}
