/*
    Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
    All rights reserved.
    Copyright (C) 2007-2013, Gabriel Dos Reis.
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

/* socket i/o primitives */

#include <stdio.h>
#include <stdlib.h>

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <sys/time.h>
#include <string.h>
#include <signal.h>
#ifdef __WIN32__
#  include <winsock2.h>
#  include <ws2tcpip.h>
#else
#  include <arpa/inet.h>
#  include <netdb.h>
#endif

#include "cfuns.h"
#include "sockio.h"
#include "com.h"
#include "bsdsignal.h"
#include "sockio.h"

namespace OpenAxiom {

#define TotalMaxPurposes 50
#define MaxServerNumbers 100
#define accept_if_needed(purpose) \
  ( purpose_table[purpose] == NULL ? sock_accept_connection(purpose) : 1 )

/* Note that the name AF_LOCAL is more portable than AF_UNIX, but MingW
   implementation and Windows documentation don't always agree.  */

#if HAVE_AF_LOCAL
#  define OPENAXIOM_AF_LOCAL AF_LOCAL
#elif HAVE_AF_UNIX
#  define OPENAXIOM_AF_LOCAL AF_UNIX
#else
#  error "needs one of AF_LOCAL or AF_UNIX"
#endif


/* socket description of spad clients */
openaxiom_sio clients[MaxClients];       

/* Local socket for server */
openaxiom_sio server;                 

/* table of dedicated socket types */
openaxiom_sio *purpose_table[TotalMaxPurposes]; 

/* bit mask of active sockets */
fd_set socket_mask;             

/* bit mask of server sockets */
fd_set server_mask;             

/* used to identify closed socket on SIGPIPE */
int socket_closed;              

/* spad server number used in sman */
int spad_server_number = -1;    


/* Non zero if the host system module support for socket is activated.
   This is needed only for MS platforms.  */
static int openaxiom_socket_module_loaded = 0;

#ifdef __WIN32__
/* Windows require some handshaking with the WinSock DLL before
   we can even think about talking about sockets. */

static void
openaxiom_unload_socket_module()
{
   WSACleanup();
   openaxiom_socket_module_loaded = 0;
}
#endif


static void
openaxiom_load_socket_module()
{
  if (!openaxiom_socket_module_loaded) {
#ifdef __WIN32__
   WSADATA wsaData;

   /* Request version 2.0 of WinSock DLL. */
   if (WSAStartup(MAKEWORD(2, 0), &wsaData) != 0) {
      perror("could not find suitable WinSock DLL.");
      exit(WSAGetLastError());
   }

   atexit(&openaxiom_unload_socket_module);
   
   if (LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 0) {
      perror("could not find suitable WinSock DLL.");
      exit(WSAGetLastError());
   }
#endif   
  }
   openaxiom_socket_module_loaded = 1;
}


/* Convert an IP address from presentation (string or ascii form)
   to numeric form.  The result is stored in the last argument.
   Success is indicated by a return value 0; failure is -1.  */
OPENAXIOM_C_EXPORT int
oa_inet_pton(const char* addr, int prot, Byte* bytes)
{
   openaxiom_load_socket_module();
   switch (prot) {
   case 4: {
#ifdef __WIN32__
      unsigned long inet_val = inet_addr(addr);
      if (inet_val == INADDR_NONE || inet_val == INADDR_ANY)
         return -1;
      memcpy(bytes, &inet_val, 4);
      return 0;
#else
      struct in_addr inet_val;
      if (inet_aton(addr, &inet_val) != 0) {
         memcpy(bytes, &inet_val, 4);
         return 0;
      }
      return -1;
#endif
   }
   default:
      return -1;
   }
}

/* Resolve a hostname to its IP address.  On success return 0,
   otherwise -1.  */
OPENAXIOM_C_EXPORT int
oa_get_host_address(const char* n, int prot, Byte* bytes)
{
   struct hostent* h;
   openaxiom_load_socket_module();
   h = gethostbyname(n);
   if (h == 0)
      return -1;

   if (h->h_length != prot)
      /* Protocol mismatch.  */
      return -1;
   memcpy(bytes, h->h_addr_list[0], prot);
   return 0;
}


/* Get a socket identifier to a local server.  We take whatever protocol
   is the default for the address family in the SOCK_STREAM type.  */
static inline openaxiom_socket
openaxiom_socket_stream_link(int family)
{
   openaxiom_load_socket_module();
   return socket(family, SOCK_STREAM, 0);
}


/* Returns 1 if SOCKET is an invalid socket.  Otherwise return 0.  */

static inline int
openaxiom_socket_is_invalid(openaxiom_socket sock)
{
#ifdef __WIN32__
   return sock == INVALID_SOCKET;
#else
   return sock < 0;
#endif   
}

static inline int
is_invalid_socket(const openaxiom_sio* s)
{
   return openaxiom_socket_is_invalid(s->socket);
}

/* Returns 1 if SOCKET is a valid socket.  Otherwise return 0.  */

static inline int
is_valid_socket(const openaxiom_sio* s)
{
#ifdef __WIN32__
   return s->socket != INVALID_SOCKET;
#else
   return s->socket > 0;
#endif   
}


/* Because a socket on Windows platform is a not just a simple file
   descriptor as it is in the Unix world, it is invalid to use
   a socket identifier as argument for read(), or close, or
   any other file descriptor function.  Furthermore, Windows
   requires cleanup.  */

OPENAXIOM_C_EXPORT void
oa_close_socket(openaxiom_socket s)
{
#ifdef __WIN32__
   shutdown(s, SD_BOTH);
   closesocket(s);
#else
   close(s);
#endif
}  

/* Local IPC Socket:
      On POSIX systems, this is just the Local IPC Socket.
      On Windows, we Windows Named Pipes.

      Please, remember that Windows Named Pipes are closer to
      Unix Domain Sockets than they are to Unix Named Pipes.  They
      ae full duplex communication links, supporting regular
      file I/O operations.  */

OPENAXIOM_C_EXPORT openaxiom_filedesc
oa_open_local_client_stream_socket(const char* path)
{
#ifdef __WIN32__
#  define NAMED_PIPE_PREFIX "\\\\.\\pipe"
   char* pipename;
   HANDLE handle;

   pipename = (char *) malloc(sizeof NAMED_PIPE_PREFIX + strlen(path));
   strcpy(pipename, NAMED_PIPE_PREFIX);
   strcat(pipename, path);

   /* Try to open only an existing named pipe.  */
   while (1) {
      handle = CreateFile(/* lpFileName */ pipename,
                          /* dwDesiredAccess */ GENERIC_READ | GENERIC_WRITE,
                          /* dwSharedMode */ 0,
                          /* lpSecurityAttributes */ NULL,
                          /* dwCreationDisposition */ OPEN_EXISTING,
                          /* dwFlagsAttributes */ 0,
                          /* hTemplateFile */ NULL);

      if (handle != INVALID_HANDLE_VALUE)
         return handle;

      if (GetLastError() != ERROR_PIPE_BUSY
          || !WaitNamedPipe(pipename, NMPWAIT_WAIT_FOREVER))
         return INVALID_HANDLE_VALUE;
   }
#  undef NAMED_PIPE_PREFIX   
#else
   int sock = socket(AF_UNIX, SOCK_STREAM, 0);
   struct sockaddr_un addr;
   if (sock < 0)
      return -1;

   memset(&addr, 0, sizeof addr);
   addr.sun_family = OPENAXIOM_AF_LOCAL;
   strncpy(addr.sun_path, path, sizeof(addr.sun_path) - 1);
   if (connect(sock, (struct sockaddr*)&addr, sizeof addr) < 0) {
      close(sock);
      return -1;
   }
   return sock;
#endif   
}

OPENAXIOM_C_EXPORT int
oa_filedesc_read(openaxiom_filedesc desc, Byte* buf, int size)
{
#ifdef __WIN32__
   DWORD count = -1;
   if (ReadFile(/* hFile */ desc,
                /* lpBuffer */ buf,
                /* nNumberOfBytesToRead */ size,
                /* lpNumberOfBytesRead */ &count,
                /* lpOverlapped */ NULL))
      return count;
   return -1;
#else
   return read(desc, buf, size);
#endif   
}

OPENAXIOM_C_EXPORT int
oa_filedesc_write(openaxiom_filedesc desc, const Byte* buf, int size)
{
#ifdef __WIN32__
   DWORD count = -1;
   if (WriteFile(/* hFile */ desc,
                 /* lpBuffer */ buf,
                 /* nNumberOfBytesToWrite */ size,
                 /* lpNumberOfBytesWritten */ &count,
                 /* lpOverlapped */ NULL))
      return count;
   return -1;
#else
   return write(desc, buf, size);
#endif   
}

OPENAXIOM_C_EXPORT int
oa_filedesc_close(openaxiom_filedesc desc)
{
#ifdef __WIN32__
   return CloseHandle(desc) ? 0 : -1;
#else
   return close(desc);
#endif   
}

/* IP sockets.
*/

OPENAXIOM_C_EXPORT openaxiom_socket
oa_connect_ip_port_stream(const Byte* addr, int prot, openaxiom_port port)
{
   struct sockaddr_in server;
   openaxiom_socket sock;
   /* IP6 is not yet supported.  */
   if (prot != 4)
      return OPENAXIOM_INVALID_SOCKET;

   sock = openaxiom_socket_stream_link(AF_INET);
   if (openaxiom_socket_is_invalid(sock))
      return OPENAXIOM_INVALID_SOCKET;

   memset(&server, 0, sizeof server);
   server.sin_family = AF_INET;
   memcpy(&server.sin_addr, addr, prot);
   server.sin_port = htons(port);
   if (connect(sock, (struct sockaddr*)&server, sizeof server) < 0) {
      oa_close_socket(sock);
      return OPENAXIOM_INVALID_SOCKET;
   }
   return sock;
}

/* It is idiomatic in the Unix/POSIX world to use the standard
   read() and write() functions on sockets.  However, in the Windows
   world, that is invalid.  Consequently, portability suggests that
   we restrict ourselves to the POSIX standard functions recv() and
   send().  */


/* Read some bytes of data into buffer `buf' with capacity `size'.
   On failure, return -1; otherwise return the number of bytes
   actually read.  */

OPENAXIOM_C_EXPORT int
oa_socket_read(openaxiom_socket sock, Byte* buf, int size)
{
   return recv(sock, (char*) buf, size, 0);
}

/* Attempt to read a byte from scoket `sock'.
   On failure, return -1; otherwise the actual byte read.  */

OPENAXIOM_C_EXPORT int
oa_socket_read_byte(openaxiom_socket sock)
{
   Byte byte;
   if(oa_socket_read(sock, &byte, 1) < 1)
      return -1;
   return byte;
}
   

/* Send `size' bytes of data contained in `buf' to the socket `sock'.
   Return -1 on failured; the number of actualy write bytes on success.  */

OPENAXIOM_C_EXPORT int
oa_socket_write(openaxiom_socket sock, const Byte* buf, int size)
{
   return send(sock, (const char*) buf, size, 0);
}

/* Send one byte to socket `sock'.  */
OPENAXIOM_C_EXPORT int
oa_socket_write_byte(openaxiom_socket sock, Byte byte)
{
   return oa_socket_write(sock, &byte, 1) < 1 ? -1 : byte;
}


/* Return 1 is the last call was cancelled. */

static inline int
openaxiom_syscall_was_cancelled()
{
#ifdef __WIN32__
   return WSAGetLastError() == WSAEINTR;
#else
   return errno == EINTR;
#endif
}

/* Return 1 is last connect() was refused.  */

static inline int
openaxiom_connection_refused()
{
#ifdef __WIN32__
   return WSAGetLastError() == WSAECONNREFUSED;
#else
   return errno == ECONNREFUSED;
#endif   
}


OPENAXIOM_C_EXPORT void  
sigpipe_handler(int sig)
{
  socket_closed = 1;
}

OPENAXIOM_C_EXPORT int 
wait_for_client_read(openaxiom_sio *sock, Byte* buf, int buf_size,
                     const char* msg)
{
  int ret_val;
  switch(sock->purpose) {
  case SessionManager:
  case ViewportServer:
    sock_accept_connection(sock->purpose);
    ret_val = sread(purpose_table[sock->purpose], buf, buf_size, msg);
    sock->socket = 0;
    return ret_val;
  default:
    sock->socket = 0;
    return -1;
  }
}

OPENAXIOM_C_EXPORT int 
wait_for_client_write(openaxiom_sio* sock, const Byte* buf,
                      int buf_size, const char* msg)
{
  int ret_val;
  switch(sock->purpose) {
  case SessionManager:
  case ViewportServer:
    sock_accept_connection(sock->purpose);
    ret_val = swrite(purpose_table[sock->purpose], buf, buf_size, msg);
    sock->socket = 0;
    return ret_val;
  default:
    sock->socket = 0;
    return -1;
  }
}

// Prefix a message with a given string for diagnostic purposes.
static std::string diagnostic(const char* prefix, const char* msg)
{
  std::string s = prefix;
  s += msg;
  return s;
}

OPENAXIOM_C_EXPORT int 
sread(openaxiom_sio* sock, Byte* buf, int buf_size, const char *msg)
{
  int ret_val;
  errno = 0;
  do {
    ret_val = oa_socket_read(sock->socket, buf, buf_size);
  } while (ret_val == -1 && openaxiom_syscall_was_cancelled());
  if (ret_val == 0) {
    FD_CLR(sock->socket, &socket_mask);
    purpose_table[sock->purpose] = NULL;
    oa_close_socket(sock->socket);
    return wait_for_client_read(sock, buf, buf_size, msg);
  }
  if (ret_val == -1) {
    if (msg) {
      perror(diagnostic("reading: ", msg).c_str());
    }
    return -1;
  }
  return ret_val;
}

OPENAXIOM_C_EXPORT int 
swrite(openaxiom_sio* sock, const Byte* buf, int buf_size,
       const char* msg)
{
  int ret_val;
  errno = 0;
  socket_closed = 0;
  ret_val = oa_socket_write(sock->socket, buf, buf_size);
  if (ret_val == -1) {
    if (socket_closed) {
      FD_CLR(sock->socket, &socket_mask);
      purpose_table[sock->purpose] = NULL;
      /*      printf("   closing socket %d\n", sock->socket); */
      oa_close_socket(sock->socket);
      return wait_for_client_write(sock, buf, buf_size, msg);
    } else {
      if (msg) {
        perror(diagnostic("writing: ", msg).c_str());
      }
      return -1;
    }
  }
  return ret_val;
}

OPENAXIOM_C_EXPORT int 
sselect(int n,fd_set  *rd, fd_set  *wr, fd_set *ex, void *timeout)
{
  int ret_val;
  do {
    ret_val = select(n, rd, wr, ex, (struct timeval *) timeout);
  } while (ret_val == -1 && openaxiom_syscall_was_cancelled());
  return ret_val;
}

OPENAXIOM_C_EXPORT int 
fill_buf(openaxiom_sio *sock, Byte* buf, int len, const char* msg)
{
  int bytes =  0, ret_val;
  while(bytes < len) {
    ret_val = sread(sock, buf + bytes, len - bytes, msg);
    if (ret_val == -1) return -1;
    bytes += ret_val;
  }
  return bytes;
}

OPENAXIOM_C_EXPORT int 
get_int(openaxiom_sio *sock)
{
  int val = -1, len;
  len = fill_buf(sock, byte_address(val), sizeof(int), "get_int");
  if (len != sizeof(int)) {
#ifdef DEBUG
  fprintf(stderr,"get_int: caught error\n",val);
#endif
    return -1;
  }
#ifdef DEBUG
  fprintf(stderr,"get_int: received %d\n",val);
#endif
  return val;
}

OPENAXIOM_C_EXPORT int 
sock_get_int(int purpose)
{
  if (accept_if_needed(purpose) != -1)
    return get_int(purpose_table[purpose]);
  else return -1;
}

OPENAXIOM_C_EXPORT int 
get_ints(openaxiom_sio *sock, int *vals, int num)
{
  int i;
  for(i=0; i<num; i++)
    *vals++ = get_int(sock);
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_get_ints(int purpose, int *vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return get_ints(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_int(openaxiom_sio *sock,int val)
{
  int ret_val;
  ret_val = swrite(sock, byte_address(val), sizeof(int), "send_int");
  if (ret_val == -1) {
    return -1;
  }
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_send_int(int purpose,int  val)
{
  if (accept_if_needed(purpose) != -1)
    return send_int(purpose_table[purpose], val);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_ints(openaxiom_sio *sock, const int *vals, int num)
{
  int i;
  for(i=0; i<num; i++)
    if (send_int(sock, *vals++) == -1)
      return -1;
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_send_ints(int purpose, const int *vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return send_ints(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_string_len(openaxiom_sio *sock, const char *str,int len)
{
  int val;
  if (len > 1023) {
    char *buf;
    buf = (char*) malloc(len+1);
    strncpy(buf,str,len);
    buf[len]='\0';
    send_int(sock,len+1);
    val = swrite(sock, (const Byte*) buf, len+1, "send_string_len");
    free(buf);
  } else {
    static char buf[1024];
    strncpy(buf, str, len);
    buf[len] = '\0';
    send_int(sock, len+1);
    val = swrite(sock, (const Byte*) buf, len+1, "send_string_len");
  }
  if (val == -1) {
    return val;
  }
  return 0;
}

OPENAXIOM_C_EXPORT int 
send_string(openaxiom_sio* sock, const char* str)
{
  int val, len = strlen(str);
  send_int(sock, len+1);
  val = swrite(sock, (const Byte*) str, len+1, "send_string");
  if (val == -1) {
    return -1;
  }
  return 0;
}


OPENAXIOM_C_EXPORT int 
sock_send_string(int purpose, const char *str)
{
  if (accept_if_needed(purpose) != -1)
    return send_string(purpose_table[purpose], str);
  return -1;
}

OPENAXIOM_C_EXPORT int 
sock_send_string_len(int purpose, const char* str, int len)
{
  if (accept_if_needed(purpose) != -1)
    return send_string_len(purpose_table[purpose], str, len);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_strings(openaxiom_sio *sock, const char** vals, int num)
{
  int i;
  for(i=0; i<num; i++)
    if (send_string(sock, *vals++) == -1)
      return -1;
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_send_strings(int purpose, const char**vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return send_strings(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT char *
get_string(openaxiom_sio *sock)
{
  int val, len;
  char *buf;
  len = get_int(sock);
  if (len <0) return NULL;
  buf = (char*) malloc(len*sizeof(char));
  val = fill_buf(sock, (Byte*) buf, len, "get_string");
  if (val == -1){
        free(buf);
        return NULL;
        }
#ifdef DEBUG
  fprintf(stderr,"get_string: received \"%s\" \n",buf);
#endif
  return buf;
}

OPENAXIOM_C_EXPORT char *
sock_get_string(int purpose)
{
  if (accept_if_needed(purpose) != -1)
    return get_string(purpose_table[purpose]);
  else return NULL;
}


OPENAXIOM_C_EXPORT char *
get_string_buf(openaxiom_sio *sock, char *buf, int buf_len)
{
   int nbytes_read;
   int nbytes_to_read;
   if(sock->nbytes_pending == 0)
      sock->nbytes_pending = get_int(sock);
   nbytes_to_read = sock->nbytes_pending > buf_len
      ? buf_len
      : sock->nbytes_pending;
   nbytes_read = fill_buf(sock, (Byte*)buf, nbytes_to_read,
                          "get_string_buf");
   if (nbytes_read == -1) {
      sock->nbytes_pending = 0;
      return NULL;
   }
   sock->nbytes_pending -= nbytes_read;
   return sock->nbytes_pending == 0 ? NULL : buf;
}

OPENAXIOM_C_EXPORT char *
sock_get_string_buf(int purpose, char* buf, int buf_len)
{
  if (accept_if_needed(purpose) != -1)
    return get_string_buf(purpose_table[purpose], buf, buf_len);
  return NULL;
}

OPENAXIOM_C_EXPORT int 
get_strings(openaxiom_sio *sock, char** vals,int num)
{
  int i;
  for(i=0; i<num; i++)
    *vals++ = get_string(sock);
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_get_strings(int purpose, char** vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return get_strings(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_float(openaxiom_sio *sock, double num)
{
  int val;
  val = swrite(sock, byte_address(num), sizeof(double),"send_float");
  if (val == -1) {
    return -1;
  }
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_send_float(int purpose, double num)
{
  if (accept_if_needed(purpose) != -1)
    return send_float(purpose_table[purpose], num);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_sfloats(openaxiom_sio *sock, const float *vals,int  num)
{
  int i;
  for(i=0; i<num; i++)
    if (send_float(sock, (double) *vals++) == -1)
      return -1;
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_send_sfloats(int purpose, const float* vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return send_sfloats(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_floats(openaxiom_sio *sock, const double *vals, int num)
{
  int i;
  for(i=0; i<num; i++)
    if (send_float(sock, *vals++) == -1)
      return -1;
  return 0;
}

OPENAXIOM_C_EXPORT int 
sock_send_floats(int purpose, const double  *vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return send_floats(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT double 
get_float(openaxiom_sio *sock)
{
  double num = -1.0;
  fill_buf(sock, byte_address(num), sizeof(double), "get_float");
#ifdef DEBUG
  fprintf(stderr,"get_float: received %f\n",num);
#endif
  return num;
}

OPENAXIOM_C_EXPORT double 
sock_get_float(int purpose)
{
  if (accept_if_needed(purpose) != -1)
    return get_float(purpose_table[purpose]);
  else return 0.0;
}

OPENAXIOM_C_EXPORT int 
get_sfloats(openaxiom_sio *sock, float *vals, int num)
{
  int i;
  for(i=0; i<num; i++)
    *vals++ = (float) get_float(sock);
  return 0;
}


OPENAXIOM_C_EXPORT int 
sock_get_sfloats(int purpose,float * vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return get_sfloats(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT int 
get_floats(openaxiom_sio *sock,double *vals,int num)
{
  int i;
  for(i=0; i<num; i++)
    *vals++ = get_float(sock);
  return 0;
}


OPENAXIOM_C_EXPORT int 
sock_get_floats(int purpose, double *vals, int num)
{
  if (accept_if_needed(purpose) != -1)
    return get_floats(purpose_table[purpose], vals, num);
  return -1;
}

OPENAXIOM_C_EXPORT int 
wait_for_client_kill(openaxiom_sio *sock, int sig)
{
  int ret_val;
  switch(sock->purpose) {
  case SessionManager:
  case ViewportServer:
    sock_accept_connection(sock->purpose);
    ret_val = send_signal(purpose_table[sock->purpose], sig);
    sock->socket = 0;
    return ret_val;
  default:
    sock->socket = 0;
    return -1;
  }
}


OPENAXIOM_C_EXPORT int 
sock_get_remote_fd(int purpose)
{
  if (accept_if_needed(purpose) != -1)
    return purpose_table[purpose]->remote;
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_signal(openaxiom_sio *sock, int sig)
{
  int ret_val;
#if HAVE_DECL_KILL
  ret_val = kill(sock->pid, sig);
#else
  ret_val = raise(sig);
#endif  
  if (ret_val == -1 && errno == ESRCH) {
    FD_CLR(sock->socket, &socket_mask);
    purpose_table[sock->purpose] = NULL;
/*    printf("   closing socket %d\n", sock->socket); */
    oa_close_socket(sock->socket);
    return wait_for_client_kill(sock, sig);
  }
  return ret_val;
}

OPENAXIOM_C_EXPORT int 
sock_send_signal(int purpose,int  sig)
{
  if (accept_if_needed(purpose) != -1)
    return send_signal(purpose_table[purpose], sig);
  return -1;
}

OPENAXIOM_C_EXPORT int 
send_wakeup(openaxiom_sio *sock)
{
#ifdef SIGUSR1   
  return send_signal(sock, SIGUSR1);
#else
  return -1;
#endif  
}

OPENAXIOM_C_EXPORT int 
sock_send_wakeup(int purpose)
{
  if (accept_if_needed(purpose) != -1)
    return send_wakeup(purpose_table[purpose]);
  return -1;
}

OPENAXIOM_C_EXPORT openaxiom_sio *
connect_to_local_server_new(const char *server_name, int purpose, int time_out)
{
  int max_con=(time_out == 0 ? 1000000 : time_out), i, code=-1;
  openaxiom_sio *sock;
  char name[256];

  make_server_name(name, server_name);
  sock = (openaxiom_sio *) calloc(sizeof(openaxiom_sio), 1);
  if (sock == NULL) {
    perror("allocating socket space");
    return NULL;
  }

  sock->socket = openaxiom_socket_stream_link(OPENAXIOM_AF_LOCAL);
  if (is_invalid_socket(sock)) {
    perror("opening client socket");
    free(sock);
    return NULL;
  }

  memset(server.addr.u_addr.sa_data, 0,
         sizeof(server.addr.u_addr.sa_data));
  sock->addr.u_addr.sa_family = OPENAXIOM_AF_LOCAL;
  strcpy(sock->addr.u_addr.sa_data, name);
  for(i=0; i<max_con; i++) {
    code = connect(sock->socket, &sock->addr.u_addr,
                   sizeof(sock->addr.u_addr));
    if (code == -1) {
      if (errno != ENOENT && !openaxiom_connection_refused()) {
        perror("connecting server stream socket");
        return NULL;
      } else {
        if (i != max_con - 1)
           openaxiom_sleep(1);
        continue;
      }
    } else break;
  }

  if (code == -1) {
    return NULL;
  }

  send_int(sock, oa_getpid());
  send_int(sock, purpose);
  send_int(sock, sock->socket);
  sock->pid = get_int(sock);
  sock->remote = get_int(sock);
  return sock;
}

OPENAXIOM_C_EXPORT openaxiom_sio *
connect_to_local_server(const char *server_name, int purpose, int time_out)
{
  int max_con=(time_out == 0 ? 1000000 : time_out), i, code=-1;
  openaxiom_sio *sock;
  char name[256];

  make_server_name(name, server_name);
  sock = (openaxiom_sio *) calloc(sizeof(openaxiom_sio), 1);
  if (sock == NULL) {
    perror("allocating socket space");
    return NULL;
  }

  sock->purpose = purpose;
  /* create the socket */
  sock->socket = openaxiom_socket_stream_link(OPENAXIOM_AF_LOCAL);
  if (is_invalid_socket(sock)) {
    perror("opening client socket");
    free(sock);
    return NULL;
  }
  /* connect socket using name specified in command line */
  memset(server.addr.u_addr.sa_data, 0,
         sizeof(server.addr.u_addr.sa_data));
  sock->addr.u_addr.sa_family = OPENAXIOM_AF_LOCAL;
  strcpy(sock->addr.u_addr.sa_data, name);
  for(i=0; i<max_con; i++) {
    code = connect(sock->socket, &sock->addr.u_addr,
                   sizeof(sock->addr.u_addr));
    if (code == -1) {
      if (errno != ENOENT && !openaxiom_connection_refused()) {
        perror("connecting server stream socket");
        return NULL;
      } else {
        if (i != max_con - 1)
           openaxiom_sleep(1);
        continue;
      }
    } else break;
  }
  if (code == -1) {
    return NULL;
  }
  send_int(sock, oa_getpid());
  send_int(sock, sock->purpose);
  send_int(sock, sock->socket);
  sock->pid = get_int(sock);
/*  fprintf(stderr, "Got int form socket\n"); */
  sock->remote = get_int(sock);
  return sock;
}

/* act as terminal session for sock connected to stdin and stdout of another
   process */
OPENAXIOM_C_EXPORT void 
remote_stdio(openaxiom_sio *sock)
{
  char buf[1024];
  fd_set rd;
  int len;
  while (1) {
    FD_ZERO(&rd);
    FD_SET(sock->socket,&rd);
    FD_SET(0, &rd);
    len = sselect(FD_SETSIZE, &rd, nullptr, nullptr, NULL);
    if (len == -1) {
      perror("stdio select");
      return;
    }
    if (FD_ISSET(0, &rd)) {
      fgets(buf,1024,stdin);
      len = strlen(buf);
      swrite(sock, byte_address(buf), len, "remote_stdio::write");
    }
    if (FD_ISSET(sock->socket, &rd)) {
       len = sread(sock, byte_address(buf), 1024, "remote_stdio::read");
      if (len == -1)
        return;
      else {
        *(buf + len) = '\0';
        fputs(buf, stdout);
        fflush(stdout);
      }
    }
  }
}

/* initialize the table of dedicated sockets */
static void 
init_purpose_table()
{
  int i;
  for(i=0; i<TotalMaxPurposes; i++) {
    purpose_table[i] = NULL;
  }
}


OPENAXIOM_C_EXPORT int 
make_server_number()
{
  spad_server_number = oa_getpid();
  return spad_server_number;
}

OPENAXIOM_C_EXPORT void 
close_socket(openaxiom_socket socket_num, const char *name)
{
  oa_close_socket(socket_num);
  oa_unlink(name);
}

OPENAXIOM_C_EXPORT int 
make_server_name(char *name, const char* base)
{
  char *num;
  if (spad_server_number != -1) {
    sprintf(name, "%s%d", base, spad_server_number);
    return 0;
  }
  num = oa_getenv("SPADNUM");
  if (num == NULL) {
/*    fprintf(stderr,
      "\n(AXIOM Sockets) The AXIOM server number is undefined.\n");
*/
    return -1;
  }
  sprintf(name, "%s%s", base, num);
  return 0;
}

static void
init_socks()
{
  FD_ZERO(&socket_mask);
  FD_ZERO(&server_mask);
  init_purpose_table();
  server.socket = 0;
  for (int i=0; i<MaxClients; i++)
     clients[i].socket = 0;
}

/* client Spad server sockets: server is a local domain socket. */
OPENAXIOM_C_EXPORT int 
open_server(const char* server_name)
{
  char *s, name[256];

  init_socks();
#ifdef SIGPIPE
  bsdSignal(SIGPIPE, sigpipe_handler,RestartSystemCalls);
#endif  
  if (make_server_name(name, server_name) == -1)
    return -2;
  /* Next create the local domain socket */
  server.socket = openaxiom_socket_stream_link(OPENAXIOM_AF_LOCAL);
  if (is_invalid_socket(&server)) {
    perror("opening local server socket");
    server.socket = 0;
    return -2;
  } else {
    server.addr.u_addr.sa_family = OPENAXIOM_AF_LOCAL;
    memset(server.addr.u_addr.sa_data, 0,
           sizeof(server.addr.u_addr.sa_data));
    strcpy(server.addr.u_addr.sa_data, name);
    if (bind(server.socket, &server.addr.u_addr,
             sizeof(server.addr.u_addr))) {
      perror("binding local server socket");
      server.socket = 0;
      return -2;
    }
    FD_SET(server.socket, &socket_mask);
    FD_SET(server.socket, &server_mask);
    listen(server.socket, 5);
  }
  s = oa_getenv("SPADSERVER");
  if (s == NULL) {
/*    fprintf(stderr, "Not a spad server system\n"); */
    return -1;
  }
  return 0;
}

/* reads a the socket purpose declaration for classification */
OPENAXIOM_C_EXPORT void 
get_socket_type(openaxiom_sio *sock)
{
  sock->pid = get_int(sock);
  sock->purpose = get_int(sock);
  sock->remote = get_int(sock);
  send_int(sock, oa_getpid());
  send_int(sock, sock->socket);
  purpose_table[sock->purpose] = sock;
}

OPENAXIOM_C_EXPORT int 
accept_connection()
{
  int client = 0;
  while (client < MaxClients && clients[client].socket != 0)
     ++client;
  if (client == MaxClients) {
    printf("Ran out of client openaxiom_sio structures\n");
    return -1;
  }
  clients[client].socket = accept(server.socket, 0, 0);
  if (is_invalid_socket(&clients[client])) {
    perror("accept_connection");
    clients[client].socket = 0;
    return -1;
  }
  FD_SET(clients[client].socket, &socket_mask);
  get_socket_type(&clients[client]);
  return clients[client].purpose;
}

OPENAXIOM_C_EXPORT int 
sock_accept_connection(int purpose)
{
  fd_set rd;
  int ret_val, p;
  if (oa_getenv("SPADNUM") == NULL) return -1;
  while (1) {
    rd = server_mask;
    ret_val = sselect(FD_SETSIZE, &rd, nullptr, nullptr, NULL);
    if (ret_val == -1) {
      perror ("Select");
      return -1;
    }
    if (is_valid_socket(&server) && FD_ISSET(server.socket, &rd)) {
       p = accept_connection();
       if (p == purpose) return 1;
    }
  }
}

/* Socket I/O selection called from the BOOT serverLoop function */

OPENAXIOM_C_EXPORT int 
server_switch()
{
  int ret_val, cmd = 0;
  fd_set rd, wr, ex, fds_mask;
  FD_ZERO(&rd);
  FD_ZERO(&wr);
  FD_ZERO(&ex);
  fds_mask = server_mask;
  cmd = 0;
  if (purpose_table[SessionManager] != NULL) {
    FD_SET(0, &fds_mask);
    FD_SET(purpose_table[SessionManager]->socket, &fds_mask);
  }
  while (1) {
    do {
      if (purpose_table[MenuServer] != NULL) {
        FD_SET(purpose_table[MenuServer]->socket, &fds_mask);
      }
      rd = fds_mask;
      ret_val = select(FD_SETSIZE, &rd, 0, 0, 0);
      if (ret_val == -1) {
        /* perror ("Select in switch"); */
        return -1;
      }
      if (is_valid_socket(&server) && FD_ISSET(server.socket, &rd))
         accept_connection();
    } while (purpose_table[SessionManager] == NULL);
    FD_SET(purpose_table[SessionManager]->socket, &fds_mask);
    if (FD_ISSET(purpose_table[SessionManager]->socket, &rd)) {
      cmd = get_int(purpose_table[SessionManager]);
      return cmd;
    }
    if (FD_ISSET(0, &rd)) {
      return CallInterp;
    }
    if (purpose_table[MenuServer] != NULL &&
        (FD_ISSET(purpose_table[MenuServer]->socket, &rd))) {
      cmd = get_int(purpose_table[MenuServer]);
      return cmd;
    }
  }
}

}
