/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2013, Gabriel Dos Reis
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

#include <stddef.h>

#ifdef __WIN32__
#  include <winsock2.h>
#  define OPENAXIOM_INVALID_SOCKET INVALID_SOCKET
#else
#  include <sys/types.h>
#  include <sys/socket.h>
#  include <netinet/in.h>
#  include <arpa/inet.h>
#  include <sys/un.h>
#  define OPENAXIOM_INVALID_SOCKET (-1)
#endif

#include "openaxiom-c-macros.h"
#include "open-axiom.h"

namespace OpenAxiom {

/* On Windows, a socket identifier is not a file descriptor.  It is
   represented by an integer type, but that integer type is not just
   plain int as in the Unix world.  It is an unsigned integer.
   Consequently, we abstract over that variation, using the typedef
   axiom_socket.  */

#ifdef __WIN32__
typedef SOCKET openaxiom_socket;
typedef HANDLE openaxiom_filedesc;
#else
typedef int openaxiom_socket;
typedef int openaxiom_filedesc;
#endif
typedef int openaxiom_port;

struct openaxiom_sio {
  openaxiom_socket socket; /* descriptor of this socket I/O endpoint.  */
  int type;             /* socket type (AF_UNIX or AF_INET) */
  int purpose;          /* can be SessionManager, GraphicsServer, etc. */
  int pid;              /* process ID of connected socket */
  int frame;            /* spad interpreter frame. */
  openaxiom_socket remote;  /* descriptor of socket at the other endpoint. */
  union {
    struct sockaddr u_addr;
    struct sockaddr_in i_addr;
  } addr;
  char *host_name;      /* name of foreign host if type == AF_INET */
  int nbytes_pending;       /* pending bytes for read.  */
};



OPENAXIOM_C_EXPORT openaxiom_filedesc
   oa_open_local_client_stream_socket(const char*);
OPENAXIOM_C_EXPORT int oa_inet_pton(const char*, int, Byte*);
OPENAXIOM_C_EXPORT int oa_get_host_address(const char*, int, Byte*);
OPENAXIOM_C_EXPORT int oa_open_local_server_stream_socket(const char*);
OPENAXIOM_C_EXPORT openaxiom_socket
   oa_connect_ip_port_stream(const Byte*, int, openaxiom_port);

OPENAXIOM_C_EXPORT int oa_socket_write(openaxiom_socket, const Byte*, int);
OPENAXIOM_C_EXPORT int oa_socket_write_byte(openaxiom_socket, Byte);

OPENAXIOM_C_EXPORT int oa_socket_read(openaxiom_socket, Byte*, int);
OPENAXIOM_C_EXPORT int oa_socket_read_byte(openaxiom_socket);

OPENAXIOM_C_EXPORT void oa_close_socket(openaxiom_socket);

OPENAXIOM_C_EXPORT int 
oa_filedesc_write(openaxiom_filedesc, const Byte*, int);
OPENAXIOM_C_EXPORT int 
oa_filedesc_read(openaxiom_filedesc, Byte*, int);
OPENAXIOM_C_EXPORT int oa_filedesc_close(openaxiom_filedesc);

OPENAXIOM_C_EXPORT int sread(openaxiom_sio*, Byte*, int, const char*);
OPENAXIOM_C_EXPORT int swrite(openaxiom_sio*, const Byte*, int,
                            const char*);

OPENAXIOM_C_EXPORT int wait_for_client_read(openaxiom_sio*, Byte*,
                                          int, const char*);
OPENAXIOM_C_EXPORT int wait_for_client_write(openaxiom_sio*,
                                           const Byte*, int,
                                           const char*);

OPENAXIOM_C_EXPORT int make_server_name(char*, const char*);
OPENAXIOM_C_EXPORT int make_server_number();
OPENAXIOM_C_EXPORT openaxiom_sio* connect_to_local_server(const char*, int, int);
OPENAXIOM_C_EXPORT int open_server(const char*);
OPENAXIOM_C_EXPORT int accept_connection();
OPENAXIOM_C_EXPORT int sselect(int, fd_set*, fd_set*, fd_set*, void*);
OPENAXIOM_C_EXPORT void close_socket(openaxiom_socket, const char*);

OPENAXIOM_C_EXPORT int get_int(openaxiom_sio*);
OPENAXIOM_C_EXPORT double get_float(openaxiom_sio*);
OPENAXIOM_C_EXPORT double sock_get_float(int);
OPENAXIOM_C_EXPORT int get_sfloats(openaxiom_sio*, float*, int);
OPENAXIOM_C_EXPORT char* get_string(openaxiom_sio*);
OPENAXIOM_C_EXPORT void sigpipe_handler(int);
OPENAXIOM_C_EXPORT int fill_buf(openaxiom_sio*, Byte*, int,
                              const char*);
OPENAXIOM_C_EXPORT int sock_get_int(int);
OPENAXIOM_C_EXPORT int get_ints(openaxiom_sio*, int*, int);
OPENAXIOM_C_EXPORT int sock_get_ints(int, int*, int);
OPENAXIOM_C_EXPORT int send_int(openaxiom_sio*, int);
OPENAXIOM_C_EXPORT int sock_send_int(int, int);
OPENAXIOM_C_EXPORT int send_ints(openaxiom_sio*, const int*, int);
OPENAXIOM_C_EXPORT int sock_send_ints(int, const int*, int);
OPENAXIOM_C_EXPORT int send_string(openaxiom_sio*, const char*);
OPENAXIOM_C_EXPORT int send_string_len(openaxiom_sio*, const char*, int);
OPENAXIOM_C_EXPORT int sock_send_string(int, const char*);
OPENAXIOM_C_EXPORT int sock_send_string_len(int, const char*, int);
OPENAXIOM_C_EXPORT int send_strings(openaxiom_sio*, const char**, int);
OPENAXIOM_C_EXPORT int sock_send_strings(int, const char**, int);
OPENAXIOM_C_EXPORT char* sock_get_string(int);
OPENAXIOM_C_EXPORT char* get_string_buf(openaxiom_sio*, char*, int);
OPENAXIOM_C_EXPORT char* sock_get_string_buf(int, char*, int);
OPENAXIOM_C_EXPORT int get_strings(openaxiom_sio*, char**, int);
OPENAXIOM_C_EXPORT int sock_get_strings(int, char**, int);
OPENAXIOM_C_EXPORT int send_float(openaxiom_sio*, double);
OPENAXIOM_C_EXPORT int sock_send_float(int, double);
OPENAXIOM_C_EXPORT int send_sfloats(openaxiom_sio*, const float*, int);
OPENAXIOM_C_EXPORT int sock_send_sfloats(int, const float*, int);
OPENAXIOM_C_EXPORT int send_floats(openaxiom_sio*, const double*, int);
OPENAXIOM_C_EXPORT int sock_send_floats(int, const double*, int);
OPENAXIOM_C_EXPORT int sock_get_sfloats(int, float*, int);
OPENAXIOM_C_EXPORT int get_floats(openaxiom_sio*, double*, int);
OPENAXIOM_C_EXPORT int sock_get_floats(int, double*, int);
OPENAXIOM_C_EXPORT int wait_for_client_kill(openaxiom_sio*, int);
OPENAXIOM_C_EXPORT int sock_get_remote_fd(int);
OPENAXIOM_C_EXPORT int send_signal(openaxiom_sio*, int);
OPENAXIOM_C_EXPORT int sock_send_signal(int, int);
OPENAXIOM_C_EXPORT int send_wakeup(openaxiom_sio*);
OPENAXIOM_C_EXPORT int sock_send_wakeup(int);
OPENAXIOM_C_EXPORT void remote_stdio(openaxiom_sio*);
OPENAXIOM_C_EXPORT void get_socket_type(openaxiom_sio*);
OPENAXIOM_C_EXPORT int sock_accept_connection(int);
OPENAXIOM_C_EXPORT int server_switch();

#define MaxClients      150

/* possible socket types (purpose) */

#define SessionManager  1
#define ViewportServer  2
#define MenuServer      3
#define SessionIO       4
#define InterpWindow    5
#define KillSpad        6

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

OPENAXIOM_C_EXPORT openaxiom_sio *purpose_table[];
OPENAXIOM_C_EXPORT openaxiom_sio server;
OPENAXIOM_C_EXPORT openaxiom_sio clients[];
OPENAXIOM_C_EXPORT fd_set socket_mask;
OPENAXIOM_C_EXPORT fd_set server_mask;

}

#endif /* OPENAXIOM_SOCKIO_included */
