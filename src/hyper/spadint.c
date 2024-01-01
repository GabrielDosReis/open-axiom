/*
   Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
   All rights reserved.
   Copyright (C) 2007-2023, Gabriel Dos Reis.
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
static void issue_dependent_commands(HyperDocPage * page , TextNode * command , TokenType type);
static void send_pile(openaxiom_sio * sock , char * str);
static void mark_as_executed(HyperDocPage * page , TextNode * command , TokenType type);
static void accept_menu_server_connection(HyperDocPage * page);
static void switch_frames(void );
static void close_client(int pid);

typedef struct sock_list {      /* linked list of openaxiom_sio */
    openaxiom_sio Socket;
    struct sock_list *next;
}   Sock_List;

Sock_List *plSock = (Sock_List *) 0;

/* Attempt to connect to OpenAxiom , return connection status. */
SpadStatus connect_spad()
{
    if (!MenuServerOpened) {
        fprintf(stderr, "(HyperDoc) Warning: Not connected to OpenAxiom Server!\n");
        LoudBeepAtTheUser();
        return SpadStatus::NotConnected;
    }
    if (spad_socket == NULL) {
        spad_socket = connect_to_local_server(SpadServer, MenuServer, Forever);
        if (spad_socket == NULL) {
            fprintf(stderr, "(HyperDoc) Warning: Could not connect to OpenAxiom Server!\n");
            LoudBeepAtTheUser();
            return SpadStatus::NotConnected;
        }
    }
    /* if (spad_busy()) return SpadBusy; */
    return SpadStatus::Connected;
}

/* returns true if spad is currently computing */
int
spad_busy()
{
    if (session_server == NULL)
        return 1;
    send_int(session_server, QuerySpad);
    return get_int(session_server);
}


/* issue a OpenAxiom command to the buffer associated with a page */
void
issue_spadcommand(HyperDocPage *page, TextNode *command, int immediate,
                  TokenType type)
{
    char *buf;

    auto status = connect_spad();
    if (status == SpadStatus::NotConnected || status == SpadStatus::SpadBusy)
        return;

    if (page->sock == NULL)
        start_user_buffer(page);
    auto ret_val = send_int(page->sock, TestLine);
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
    if (type == TokenType::Spadsrc)
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
issue_dependent_commands(HyperDocPage *page, TextNode *command, TokenType type)
{
    TextNode *node, *depend_label;
    SpadcomDepend *depend;
    TokenType end_type = (type == TokenType::Spadcommand
                    || type == TokenType::Spadgraph) ?
    (TokenType::Endspadcommand) : (TokenType::Endspadsrc);

    for (node = command->next; node->type != end_type;
         node = node->next)
        if (node->type == TokenType::Free)
            for (depend_label = node->data.node; depend_label != NULL;
                 depend_label = depend_label->next)
                if (depend_label->type == TokenType::Word) {
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
mark_as_executed(HyperDocPage *page, TextNode *command, TokenType type)
{
    TextNode *node, *depend_label;
    SpadcomDepend *depend;
    TokenType end_type = (type == TokenType::Spadcommand
                    || type == TokenType::Spadgraph)
    ? (TokenType::Endspadcommand) : (TokenType::Endspadsrc);

    for (node = command; node->type != end_type; node = node->next)
        if (node->type == TokenType::Bound)
            for (depend_label = node->data.node; depend_label != NULL;
                 depend_label = depend_label->next)
                if (depend_label->type == TokenType::Word) {
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

        if (server.socket > 0 && FD_ISSET(server.socket, &rd)) {
            sock = accept_menu_connection(&server);
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
          case TokenType::Newline:
            storeChar('\n');
            node = node->next;
            break;
          case TokenType::Ifcond:
            if (check_condition(node->data.ifnode->cond))
                node = node->data.ifnode->thennode;
            else
                node = node->data.ifnode->elsenode;
            break;
          case TokenType::Endarg:
          case TokenType::Endspadcommand:
          case TokenType::Endspadsrc:
          case TokenType::Endpix:
            storeChar('\0');
            return p2sBuf;
          case TokenType::Endverbatim:
          case TokenType::Endif:
          case TokenType::Fi:
          case TokenType::Endmacro:
          case TokenType::Endparameter:
          case TokenType::Rbrace:
          case TokenType::Endgroup:
            node = node->next;
            break;
          case TokenType::Punctuation:

            /*
             * Simply copy the piece of text
             */
            if (node->space & FRONTSPACE) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case TokenType::WindowId:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            storeChar(' ');
            node = node->next;
            break;
          case TokenType::Verbatim:
          case TokenType::Spadsrctxt:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }

            /*
             * now add the eol
             */

            /*
             * if(node->next && node->next->type != TokenType::Endspadsrc)
             * storeChar('\n');
             */
            node = node->next;
            break;
          case TokenType::Dash:
          case TokenType::Rsquarebrace:
          case TokenType::Lsquarebrace:
          case TokenType::Word:

            /*
             * Simply copy the piece of text
             */
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case TokenType::BoxValue:
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
          case TokenType::StringValue:
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
          case TokenType::Space:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case TokenType::Titlenode:
          case TokenType::Endtitle:
          case TokenType::Center:
          case TokenType::Endcenter:
          case TokenType::BoldFace:
          case TokenType::Emphasize:
          case TokenType::Indentrel:
            node = node->next;
            break;
          case TokenType::Bound:
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
                for (; n2->type != TokenType::Endarg; n2 = n2->next) {
                    if (n2->type == TokenType::Word) {
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
          case TokenType::Free:
            if (include_bf) {
                int len, i;
                TextNode *n2 = node->data.node;

                storeChar('\\');
                storeChar('f');
                storeChar('r');
                storeChar('e');
                storeChar('e');
                storeChar('{');
                for (; n2->type != TokenType::Endarg; n2 = n2->next) {
                    if (n2->type == TokenType::Word) {
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
          case TokenType::Macro:
            node = node->next;
            break;
          case TokenType::Pound:
            if (node->space) { storeChar(' '); }
            node = node->next;
            break;
          case TokenType::Group:
            node = node->next;
            break;
          case TokenType::Indent:
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
    char *buf;
    HyperDocPage *page;

    auto status = connect_spad();
    if (status == SpadStatus::NotConnected) {
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "SpadNotConnectedPage");
        if (page == NULL)
            fprintf(stderr, "No SpadNotConnectedPage found\n");
        return page;
    }
    if (status == SpadStatus::SpadBusy) {
        page = (HyperDocPage *) hash_find(gWindow->fPageHashTable, "SpadBusyPage");
        if (page == NULL)
            fprintf(stderr, "No SpadBusyPage found\n");
        return page;
    }
    switch_frames();
    switch (link->type) {
      case TokenType::Qspadcall:
      case TokenType::Qspadcallquit:
      case TokenType::Spadlink:
      case TokenType::Spaddownlink:
      case TokenType::Spadmemolink:
        send_int(spad_socket, QuietSpadCommand);
        break;
      case TokenType::Spadcall:
      case TokenType::Spadcallquit:
        send_int(spad_socket, SpadCommand);
        break;
      default:
        send_int(spad_socket, LispCommand);
        break;
    }
    buf = print_to_string(command);
    send_string(spad_socket, buf);
    if (link->type == TokenType::Lispcommand
        || link->type == TokenType::Spadcall
        || link->type == TokenType::Spadcallquit
        || link->type == TokenType::Qspadcallquit
        || link->type == TokenType::Qspadcall
        || link->type == TokenType::Lispcommandquit)
        return NULL;
    page = parse_page_from_socket();
    return page;
}

int
issue_serverpaste(TextNode *command)
{
    char *buf;

    auto status = connect_spad();
    if (status == SpadStatus::NotConnected || status == SpadStatus::SpadBusy)
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
service_session_socket()
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
switch_frames()
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

void send_lisp_command(const char* command)
{
    auto status = connect_spad();
    if (status == SpadStatus::NotConnected || status == SpadStatus::SpadBusy) {
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
          case TokenType::Newline:
            storeString("\\newline\n");
            node = node->next;
            break;
          case TokenType::Par:
            storeString("\n\n");
            node = node->next;
            break;
          case TokenType::Indentrel:
            storeString("\\indentrel{");
            storeString(node->data.node->data.text);
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Tab:
            storeString("\\tab{");
            storeString(node->data.node->data.text);
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Ifcond:
            if (check_condition(node->data.ifnode->cond))
                node = node->data.ifnode->thennode;
            else
                node = node->data.ifnode->elsenode;
            break;
          case TokenType::Endarg:
          case TokenType::Endspadsrc:
          case TokenType::Endpix:
          case TokenType::Endbutton:
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Endverbatim:
          case TokenType::Endif:
          case TokenType::Fi:
          case TokenType::Endmacro:
          case TokenType::Endparameter:
          case TokenType::Rbrace:
            node = node->next;
            break;
          case TokenType::Punctuation:

            /*
             * Simply copy the piece of text
             */
            if (node->space & FRONTSPACE) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case TokenType::WindowId:
            storeString("\\windowid ");
            node = node->next;
            break;
          case TokenType::Verbatim:
          case TokenType::Spadsrctxt:
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case TokenType::Dash:
          case TokenType::Rsquarebrace:
          case TokenType::Lsquarebrace:
          case TokenType::Word:
            if (node->space) { storeChar(' '); }
            for (s = node->data.text; *s; s++) { storeChar(*s); }
            node = node->next;
            break;
          case TokenType::BoxValue:
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
          case TokenType::StringValue:
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
          case TokenType::Space:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case TokenType::Emphasize:
            storeString("\\em ");
            node = node->next;
            break;
          case TokenType::BoldFace:
            storeString("\\bf ");
            node = node->next;
            break;
          case TokenType::Sl:
            storeString("\\it ");
            node = node->next;
            break;
          case TokenType::Rm:
            storeString("\\rm ");
            node = node->next;
            break;
          case TokenType::It:
            storeString("\\it ");
            node = node->next;
            break;
          case TokenType::Tt:
            storeString("\\tt ");
            node = node->next;
            break;
          case TokenType::Group:
/* skip {} */
            if (node->next->type==TokenType::Endgroup){
               node=node->next->next;
               break;
                }
            storeChar('{');
            node = node->next;
            break;
          case TokenType::Endgroup:
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Box:
            storeString("\\box{");
            node = node->next;
            break;
          case TokenType::Endbox:
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Center:
            storeString("\\center{");
            node = node->next;
            break;
          case TokenType::Endcenter:
            storeString("}");
            storeChar('\n');
            node = node->next;
            break;
          case TokenType::Titlenode:
          case TokenType::Endtitle:
            node = node->next;
            break;
          case TokenType::Bound:
            {
                TextNode *n2 = node->data.node;

                storeString("\\bound{");
                for (; n2->type != TokenType::Endarg; n2 = n2->next) {
                    if (n2->type == TokenType::Word) {
                        storeString(n2->data.text);
                        storeChar(' ');
                        }
                    }
                storeChar('}');
            }
            node = node->next;
            break;
          case TokenType::Free:
            {
                TextNode *n2 = node->data.node;

                storeString("\\free{");
                for (; n2->type != TokenType::Endarg; n2 = n2->next) {
                    if (n2->type == TokenType::Word) {
                        storeString(n2->data.text);
                        storeChar(' ');
                        }
                    }
                storeChar('}');
                }
            node = node->next;
            break;
          case TokenType::Macro:
            storeChar(' ');
            node = node->next;
            break;
          case TokenType::Pound:
            if (node->space) { storeChar(' '); }
            node = node->next;
            break;
          case TokenType::Indent:
            num_spaces = (node->data.node != NULL ?
                          atoi(node->data.node->data.text) : 1);
            for (count = 0; count < num_spaces; count++)
                storeChar(' ');
            node = node->next;
            break;
          case TokenType::Inputbitmap:
            storeString("\\inputbitmap{");
            storeString(node->data.text); 
            storeString("}\n");
            node = node->next;
            break;
          case TokenType::Endscrolling:
            storeString("\\end{scroll}\n");
            node = node->next;
            break;
          case TokenType::Scrollingnode:
            storeString("\\begin{scroll}\n");
            storeString("% This is the scrolling area\n");
            node = node->next;
            break;
          case TokenType::Horizontalline:
            storeString("\\horizontalline\n");
            node = node->next;
            break;
          case TokenType::Endtable:
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Table:
            storeString("\\table{");
            node = node->next;
            break;
          case TokenType::Tableitem:
            storeChar('{');
            node = node->next;
            break;
          case TokenType::Endtableitem:
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Beginitems:
            storeString("\\begin{items}");
            node = node->next;
            break;
          case TokenType::Item:
            storeString("\n\\item");
            node = node->next;
            break;
          case TokenType::Enditems:
            storeString("\n\\end{items}");
            node = node->next;
            break;
/*** LINKS ***/
/* all these guys are ended by Endbutton 
we close the brace then */
          case TokenType::Spadlink:
            storeString("\\fauxspadlink{");
            node = node->next;
            break;
          case TokenType::Unixlink:
            storeString("\\fauxunixlink{");
            node = node->next;
            break;
          case TokenType::Lisplink:
            storeString("\\fauxlisplink{");
            node = node->next;
            break;
          case TokenType::Link:
            storeString("\\fauxlink{");
            node = node->next;
            break;
          case TokenType::LispDownLink:
            storeString("\\fauxlispdownlink{");
            node = node->next;
            break;
          case TokenType::LispMemoLink:
            storeString("\\fauxlispmemolink{");
            node = node->next;
            break;
          case TokenType::Memolink:
            storeString("\\fauxmemolink{");
            node = node->next;
            break;
          case TokenType::Windowlink:
            storeString("\\fauxwindowlink{");
            node = node->next;
            break;
          case TokenType::Downlink:
            storeString("\\fauxdownlink{");
            node = node->next;
            break;
/** END OF LINKS **/
          case TokenType::Unixcommand:
            storeString("\\unixcommand{");
            node = node->next;
            break;
          case TokenType::Lispcommand:
            storeString("\\lispcommand{");
            node = node->next;
            break;
          case TokenType::Spadgraph:
            storeString("\\spadgraph{");
            node = node->next;
            break;
          case TokenType::Spadcommand:
            storeString("\\spadcommand{");
            node = node->next;
            break;
          case TokenType::Endspadcommand:
            storeChar('}');
            node = node->next;
            break;
          case TokenType::Footernode:
            storeString("% This is the footer\n");
            node = node->next;
            break;
          case TokenType::Endfooter:
            storeString("% This is the end of the footer\n");
            node = node->next;
            break;
          case TokenType::Endheader:
            storeString("% This is the end of the header\n");
            node = node->next;
            break;
          case TokenType::Headernode:
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
