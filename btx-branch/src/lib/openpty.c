/*
    Copyright (c) 1991-2002, The Numerical ALgorithms Group Ltd.
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

#include "openpty.H1"


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
 * char *controllerPath;  actually , this is not used anywhere on return
                          and can be taken out of the call sequence
 * char *serverPath;
 *
 * The path name  vars should be declared of size 11 or more
 */


int  
ptyopen(int *controller,int * server, char *controllerPath,char * serverPath)
{
#if defined(SUNplatform) || defined (HP9platform) || defined(RTplatform) ||defined(AIX370platform) || defined(BSDplatform)
  int looking = 1, i;
  int oflag = O_RDWR;                  /* flag for opening the pty */
  
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
#endif
#if defined RIOSplatform
  int fdm,fds;
  char *slavename;

  /* open master */
  if ((fdm=open("/dev/ptc",O_RDWR))<0)
    perror("ptyopen failed to open /dev/ptc");
  else {
    /* get slave name */
    if((slavename = ttyname(fdm))==0)
      perror("ptyopen failed to get the slave device name");
    /* open slave */
    if ((fds = open(slavename, O_RDWR)) < 0 )
      perror("ptyopen: Failed to open slave");
    strcpy(serverPath,slavename);
    *controller=fdm;
    *server=fds;
  }
  return(fdm);
#endif

#if defined(SUN4OS5platform) ||defined(ALPHAplatform) || defined(HP10platform) || defined(LINUXplatform) || defined(MACOSXplatform) || defined(BSDplatform)
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
#if defined(SUN4OS5platform) || defined(HP10platform)
      /* push ptem */
      if (ioctl(fds, I_PUSH, "ptem") < 0)
        perror("ptyopen: Failed to push ptem");
      /* push ldterm */
      if (ioctl(fds, I_PUSH, "ldterm") < 0)
        perror("ptyopen: Failed to push idterm");
#endif
      strcpy(serverPath,slavename);
      *controller=fdm;
      *server=fds;
    }
  }
  return(fdm);
#endif
#if defined SGIplatform
  char *fds;
  fds = _getpty(controller, O_RDWR|O_NDELAY, 0600, 0);
  strcpy(serverPath,fds);
  if (0 == serverPath)
    return(-1);
  if (0 > (*server = open(serverPath,O_RDWR))) {
    (void) close(*controller);
    return(-1);
  }
  return (*controller);

#endif
}


void 
makeNextPtyNames(char *cont,char * serv)
{
#ifdef AIX370platform
        static int channelNo = 0;
        sprintf(cont, "/dev/ptyp%02x", channelNo);
        sprintf(serv, "/dev/ttyp%02x", channelNo);
        channelNo++;
#endif
#if defined(SUNplatform) || defined(HP9platform) || defined(LINUXplatform) || defined(MACOSXplatform) || defined(BSDplatform)
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
#endif
}
