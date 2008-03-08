/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis
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

      - Neither the name of The Numerical ALgorithms Group Ltd. nor the
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

#ifndef OPENAXIOM_SOCKIO_included
#define OPENAXIOM_SOCKIO_included


#ifdef __MINGW32__
#  include <winsock2.h>
#else
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#endif

#include "axiom-c-macros.h"

/* On Windows, a socket identifier is not a file descriptor.  It is
   represented by an integer type, but that integer type is not just
   plain int as in the Unix world.  It is an unsigned integer.
   Consequently, we abstract over that variation, using the typedef
   axiom_socket.  */

#ifdef __MINGW32__
typedef SOCKET axiom_socket;
#else
typedef int axiom_socket;
#endif


/* Close a socket communication endpoint.  */
extern void axiom_close_socket(axiom_socket);

typedef struct {
  axiom_socket socket;  /* socket number returned by "socket" call */
  int type;             /* socket type (AF_UNIX or AF_INET) */
  int purpose;          /* can be SessionManager, GraphicsServer, etc. */
  int pid;              /* process ID of connected socket */
  int frame;            /* spad interpreter frame (for interpreter windows) */
  axiom_socket remote;  /* file descriptor of remote socket */
  union {
    struct sockaddr u_addr;
    struct sockaddr_in i_addr;
  } addr;
  char *host_name;      /* name of foreign host if type == AF_INET */
} Sock;


extern int get_int(Sock*);
extern char* get_string(Sock*);
extern double get_float(Sock*);
extern Sock* connect_to_local_server(char*, int, int);
extern int sread(Sock*, char*, int, char*);
extern double plus_infinity(void);
extern double minus_infinity(void);
extern double NANQ(void);
extern void sigpipe_handler(int);
extern int wait_for_client_read(Sock*, char*, int, char*);
extern int wait_for_client_write(Sock*, char*, int, char*);
extern int swrite(Sock*, char*, int, char*);
extern int sselect(int, fd_set*, fd_set*, fd_set*, void*);
extern int fill_buf(Sock*, char*, int, char*);
extern int sock_get_int(int);
extern int get_ints(Sock*, int*, int);
extern int sock_get_ints(int, int*, int);
extern int send_int(Sock*, int);
extern int sock_send_int(int, int);
extern int send_ints(Sock*, int*, int);
extern int sock_send_ints(int, int*, int);
extern int send_string(Sock*, char*);
extern int send_string_len(Sock*, char*, int);
extern int sock_send_string(int, char*);
extern int sock_send_string_len(int, char*, int);
extern int send_strings(Sock*, char**, int);
extern int sock_send_strings(int, char**, int);
extern char* sock_get_string(int);
extern char* get_string_buf(Sock*, char*, int);
extern char* sock_get_string_buf(int, char*, int);
extern int get_strings(Sock*, char**, int);
extern int sock_get_strings(int, char**, int);
extern int send_float(Sock*, double);
extern int sock_send_float(int, double);
extern int send_sfloats(Sock*, float*, int);
extern int sock_send_sfloats(int, float*, int);
extern int send_floats(Sock*, double*, int);
extern int sock_send_floats(int, double*, int);
extern double sock_get_float(int);
extern int get_sfloats(Sock*, float*, int);
extern int sock_get_sfloats(int, float*, int);
extern int get_floats(Sock*, double*, int);
extern int sock_get_floats(int, double*, int);
extern int wait_for_client_kill(Sock*, int);
extern int sock_get_remote_fd(int);
extern int send_signal(Sock*, int);
extern int sock_send_signal(int, int);
extern int send_wakeup(Sock*);
extern int sock_send_wakeup(int);
extern Sock* connect_to_local_server_new(char*, int, int);
extern void remote_stdio(Sock*);
extern void init_purpose_table(void);
extern int make_server_number(void);
extern void close_socket(axiom_socket, char*);
extern int make_server_name(char*, char*);
extern int open_server(char*);
extern int accept_connection(Sock*);
extern void get_socket_type(Sock*);
extern int sock_accept_connection(int);
extern void redirect_stdio(Sock*);
extern void init_socks(void);
extern int server_switch(void);
extern void flush_stdout(void);
extern void print_line(char*);


#define MaxClients      150

/* possible socket types (purpose) */

#define SessionManager  1
#define ViewportServer  2
#define MenuServer      3
#define SessionIO       4
#define BaloonServer    5
#define InterpWindow    6
#define KillSpad        7
#define DebugWindow     8  /* used for nagman */
#define Forker          9
#define AV              10 /*Simon's algebraic viewer */

#define Acknowledge     255

/* Timeout value for connection to remote socket */

#define Forever 0

/* Socket name for local AXIOM server and session manager */

#define SpadServer              "/tmp/.d"
#define SessionServer           "/tmp/.s"
#define SessionIOName           "/tmp/.i"
#define MenuServerName          "/tmp/.h"
#define ForkServerName          "/tmp/.f"


#define MASK_SIZE       (NBBY*sizeof(fd_set))


/* table of dedicated socket types */

extern Sock *purpose_table[];
extern Sock server[];
extern Sock clients[];
extern fd_set socket_mask;
extern fd_set server_mask;


#endif /* OPENAXIOM_SOCKIO_included */
