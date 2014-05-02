/*
    Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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

#include "openaxiom-c-macros.h"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>

#ifdef HAVE_SYS_IOCTL_H
#  include <sys/ioctl.h>
#endif
#ifdef HAVE_PTY_H
#  include <pty.h>
#endif
#ifdef HAVE_UTIL_H
#  include <util.h>
#endif
#ifdef HAVE_TERMIOS_H
#  include <termios.h>
#endif
#ifdef HAVE_LIBUTIL_H
#  include <sys/types.h>
#  include <libutil.h>
#endif


#include "openpty.h"

#if defined(SUNplatform)
static void 
makeNextPtyNames(char *cont,char * serv)
{
        static int channelNo = 0;
        static char group[] = "pqrstuvwxyzPQRST";
        static int groupNo = 0;

        sprintf(cont, "/dev/pty%c%x", group[groupNo], channelNo);
        sprintf(serv, "/dev/tty%c%x", group[groupNo], channelNo);
        channelNo++;                /* try next */
        if (channelNo == 16) {      /* move to new group */
                channelNo = 0;
                groupNo++;
                if (groupNo == 16) groupNo = 0;        /* recycle */
                }
}
#endif


/*
 * The main function is ptyopen. It simply opens up both sides of a
 * pseudo-terminal. It uses and saves the pathnames for
 * the devices which were actually opened.
 *
 * If it fails it simply exits the program.
 *
 *
 * ptyopen(controller, server, controllerPath, serverPath) 
 * int *controller;     The file descriptor for controller side of the pty 
 * int *server;         The file descriptor for the server side 
 * char *serverPath;
 *
 * The path name  vars should be declared of size 11 or more
 */


int  
ptyopen(int *controller,int * server,char * serverPath)
{
#if HAVE_DECL_OPENPTY
   return openpty(controller,server, serverPath, NULL, NULL);
#elif defined(SUNplatform)
  int looking = 1, i;
  int oflag = O_RDWR;                  /* flag for opening the pty */
  char controllerPath[128];
  
  for (i = 0; looking && i < 1000; i++) {
    makeNextPtyNames(controllerPath, serverPath);
    if (access(controllerPath, 6) != 0) continue;
    *controller = open(controllerPath, oflag, 0);
    if (*controller >= 0) {
      *server = open(serverPath, oflag, 0);
      if (*server > 0)
        looking = 0;
      else
        close(*controller);
    }
  }
  if (looking) {
    fprintf(stderr, "Couldn't find a free pty.\n");
    exit(-1);
  }
  return (*controller);
#elif defined(SUN4OS5platform)
extern int grantpt(int);
extern int unlockpt(int);
extern char* ptsname(int);
  int fdm,fds;
  char *slavename;

  /* open master */
  if ((fdm = open("/dev/ptmx", O_RDWR)) < 0 )
    perror("ptyopen: Failed to open /dev/ptmx");
  else {
    /* change permission ofslave */
    if (grantpt(fdm) < 0)
      perror("ptyopen: Failed to grant access to slave device");
    /* unlock slave */
    if (unlockpt(fdm) < 0)
      perror("ptyopen: Failed to unlock master/slave pair");
    /* get name of slave */
    if ((slavename = ptsname(fdm)) == NULL)
      perror("ptyopen: Failed to get name of slave device");
    /* open slave */
    if ((fds = open(slavename, O_RDWR)) < 0 )
      perror("ptyopen: Failed to open slave");
    else {
      /* push ptem */
      if (ioctl(fds, I_PUSH, "ptem") < 0)
        perror("ptyopen: Failed to push ptem");
      /* push ldterm */
      if (ioctl(fds, I_PUSH, "ldterm") < 0)
        perror("ptyopen: Failed to push idterm");
      strcpy(serverPath,slavename);
      *controller=fdm;
      *server=fds;
    }
  }
  return(fdm);
#else
#  error "don't know how to open a pty"
#endif
}


