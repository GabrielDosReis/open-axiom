/* tm_openaxiom.c
 *   COPYRIGHT : (C) 2004  Bill Page <bill.page1@sympatico.ca>
 *   (Portions) COPYRIGHT  : (C) 1999  Andrey Grozin
 ***********************************************************************
 * This software falls under the GNU general public license and comes
 * WITHOUT ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE
 * for more details. If you don't have this file, write to the Free
 * Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
 * MA 02111-1307, USA.
 ***********************************************************************
 *
 * The program runs OPENAXIOMsys as a separate process under Windows
 * using CreateProcess() and asynchronous reader threads. It provides
 * an interface between OpenAxiom and TeXmacs for windows.
 *
 * Before launching, stdin/out/err are each redirected into pipes
 * so that OPENAXIOM can be fed commands and its output read from
 * those pipes. A separate thread is created to manage reading the
 * output of OpenAxiom and sending  it (appropriatedly modified) to
 * TeXmacs. The main thread reads from TeXmacs and sends it's
 * output to OpenAxiom. In this way the input and output is completely
 * asynchronous. There may be some advantage to the use of such
 * "light weight" threads in Windows.
 *
 * Build with the command:
 *    gcc tm_openaxiom.c texbreaker.c -o tm_openaxiom.exe
 * It is known to compile with gcc version 3.4.2 (mingw-special)
 * under MinGW/MSYS. Other compilers may also work.
 *
 * Install the tm_openaxiom.exe file in the directory
 *    C:\Program Files\WinTeXmacs\TeXmacs\bin
 *
 * Written by Bill Page 20041203 
 * - based on Maxima test version by Mike Thomas 20030624
 *   (Thankyou Mike!)
 *   and on the TeXmacs tm_openaxiom.c program (linux)
 * Modified by Bill Page 20041215
 * - add call to Robert Sutor line-break routine
 * Modified by Bill Page 20041217
 * - initialize OPENAXIOM_exe from OPENAXIOM environment variable
 * Modified by Bill Page 20041220
 * - use args, TM_OPENAXIOM environment variable and/or
 *     )set output texmacs option,option, ...
 *   to specify options
 *     break no     -- disables line-break algorithm
 *     over no      -- converts 2-d \over to 1-d /
 *     cdot no      -- converts \cdot to \ (space)
 *     space no     -- convert \ (space) to \,
 *     big( no      -- convert \left( \right to ( )
 *     width 4.5    --  4.5  inches * 1000
 * - process OpenAxiom output with no LaTeX content, e.g.
 *     )set output tex off
 *     )set output algebra on
 *                                                                */

#include <windows.h>
#include <process.h>
#include <memory.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <io.h>

/* Allow some debugging output from the IO threads */
/*#define DEBUG_OUT*/

int option_texbreak = 1,        /* default options */
    option_use_over = 1,
    option_use_cdot = 1,
    option_big_space = 0,
    option_big_paren = 1;

#define   INP_BUFF_SIZE 3072
#define   OUT_BUFF_SIZE 8192
char szBuffer[OUT_BUFF_SIZE];
#define MATHBUFLEN     8*8192
extern int maxLineWidth;
extern char bufout[2*MATHBUFLEN];

int nBuffer=0;
int nRead;
int IgnorePrompts=0;

#define   READ_HANDLE 0
#define   WRITE_HANDLE 1

char OPENAXIOM_cmd[256];
char ENV_OPENAXIOM[256];

int fdStdOutPipe[2], fdStdInPipe[2], fdStdErrPipe[2];

HANDLE hProcess;
STARTUPINFO si;                                /* Only need to set si.cb */
PROCESS_INFORMATION pi;        /* Post launch child process information. */

/* consult the environment */

void parse_options(int nargs, char *args[]) {
  char *pOPENAXIOM, *pTM_OPENAXIOM, *pOption;
  int i;
  float w;

  if (pOPENAXIOM=getenv("OPENAXIOM")) {
    strcpy(OPENAXIOM_cmd,pOPENAXIOM); strcat(OPENAXIOM_cmd, "/../../../../bin/open-axiom.exe");
    strcat(OPENAXIOM_cmd," --system=\""); strcat(OPENAXIOM_cmd,pOPENAXIOM);
    strcat(OPENAXIOM_cmd,"\"");
    strcpy(ENV_OPENAXIOM,"OPENAXIOM="); strcat(ENV_OPENAXIOM, pOPENAXIOM);
  } 
  else {                               
    printf("You must set the OPENAXIOM environment variable, e.g.\n");
    exit(1);
  }

/* e.g. TM_OPENAXIOM='break off, over yes, ...
 * if not found or different options, ignore silently                   */
  for (i=0;i<nargs;i++) {
    pTM_OPENAXIOM=args[i];                     /* args override environment */
    if (i>0 || (pTM_OPENAXIOM=getenv("TM_OPENAXIOM"))) {
      if (strstr(pTM_OPENAXIOM,"break off")
      || strstr(pTM_OPENAXIOM,"break n")
      || strstr(pTM_OPENAXIOM,"break 0"))
        option_texbreak=0;
      if (strstr(pTM_OPENAXIOM,"break on")
      || strstr(pTM_OPENAXIOM,"break y")
      || strstr(pTM_OPENAXIOM,"break 1"))
        option_texbreak=1;
      if (strstr(pTM_OPENAXIOM,"over off")
      || strstr(pTM_OPENAXIOM,"over n")
      || strstr(pTM_OPENAXIOM,"over 0"))
        option_use_over=0;
      if (strstr(pTM_OPENAXIOM,"over on")
      || strstr(pTM_OPENAXIOM,"over y")
      || strstr(pTM_OPENAXIOM,"over 1"))
        option_use_over=1;
      if (strstr(pTM_OPENAXIOM,"cdot off")
      || strstr(pTM_OPENAXIOM,"cdot n")
      || strstr(pTM_OPENAXIOM,"cdot 0"))
        option_use_cdot=0;
      if (strstr(pTM_OPENAXIOM,"cdot on")
      || strstr(pTM_OPENAXIOM,"cdot y")
      || strstr(pTM_OPENAXIOM,"cdot 1"))
        option_use_cdot=1;
      if (strstr(pTM_OPENAXIOM,"space off")
      || strstr(pTM_OPENAXIOM,"space n")
      || strstr(pTM_OPENAXIOM,"space 0"))
        option_big_space=0;
      if (strstr(pTM_OPENAXIOM,"space on")
      || strstr(pTM_OPENAXIOM,"space y")
      || strstr(pTM_OPENAXIOM,"space 1"))
        option_big_space=1;
      if (strstr(pTM_OPENAXIOM,"big( off")
      || strstr(pTM_OPENAXIOM,"big( n")
      || strstr(pTM_OPENAXIOM,"big( 0"))
        option_big_paren=0;
      if (strstr(pTM_OPENAXIOM,"big( on")
      || strstr(pTM_OPENAXIOM,"big( y")
      || strstr(pTM_OPENAXIOM,"big( 1"))
        option_big_paren=1;
      if (pOption=strstr(pTM_OPENAXIOM,"width ")) {
        sscanf(pOption+strlen("width "),"%f",&w);
        maxLineWidth=trunc(1000.0*w);
      };
    };
  };
}

/* This is the OpenAxiom Reader thread. It runs asynchronously and
 * in parallel with the main thread. It reads output from OpenAxiom
 * and after some selection and conversion, it is sent on to
 * TeXmacs. */

unsigned __stdcall StdOutReadThread ( void* pArguments )
{
  int nExitCode = STILL_ACTIVE;

  fputs("\2verbatim:",stdout);                        /* ---> to TeXmacs */

  GetExitCodeProcess ( hProcess, (unsigned long*) &nExitCode );

  while ( nExitCode == STILL_ACTIVE ) {
    #ifdef DEBUG_OUT
      fprintf ( stderr, " - Stdout read loop\n" );
      fflush ( stderr );
    #endif /* DEBUG_OUT */
    nRead = _read ( fdStdOutPipe[READ_HANDLE], &szBuffer[nBuffer],
                    OUT_BUFF_SIZE-nBuffer );      /* Read <--- from OpenAxiom */

    szBuffer[nBuffer+nRead]='\0';
    #ifdef DEBUG_OUT
      fprintf ( stderr, "After _read on stdoutpipe (%d bytes read)\n%s\n",
      nRead, &szBuffer[nBuffer] ); fflush ( stderr );
    #endif /* DEBUG_OUT */
    while (nRead>0) {            /* fill buffer until we see OpenAxiom prompt */
      if ( strncmp(&szBuffer[nBuffer],"-> ",3)==0 ) {             /* done */
        HandleOutput(szBuffer, nBuffer);   /* send output ---> to TeXmacs */
        nRead = nRead-3;                           /* keep the left overs */
        if (nRead>0) strncpy(szBuffer,&szBuffer[nBuffer+3],nRead);
        nBuffer = 0; szBuffer[nRead]='\0';
        #ifdef DEBUG_OUT
          fprintf(stderr, "Leftovers\n%s\n",&szBuffer);
          fflush(stderr);
        #endif /* DEBUG_OUT */
      } else {
        nBuffer++;
        nRead--;
      };
    };
    GetExitCodeProcess ( hProcess, (unsigned long*) &nExitCode );
  };

  fputs("\2latex:\\red The end\\black\5\5",stdout); /* The ends --> TeXmacs */
  fflush(stdout);
  _endthreadex(0);
}

/* This routine parses all OpenAxiom output between -> prompts */

HandleOutput(char Buffer[], int n)         /* sends output ---> to TeXmacs */
{
  int mmode, i, j;

  #ifdef DEBUG_OUT
   fprintf(stderr, "HandleOutput IgnorePrompts:%d\n%s\n",IgnorePrompts,
           Buffer);
   fflush(stderr);
  #endif /* DEBUG_OUT */
  if (IgnorePrompts) --IgnorePrompts;                     /* counting down */
  else {                                              /* its a good packet */
    while (n>0 && Buffer[n-1]=='\n') n--;
    Buffer[n]='\0';                                   /* mark the real end */
    mmode = 0; j=0;
    while (Buffer[j]=='\n') j++;                    /* find the real start */
    for (i=j;i<n;i++) {
      if (strncmp(&Buffer[i],"$$\n",3)==0) {       /* Found a LaTeX marker */
        #ifdef DEBUG_OUT
          fprintf(stderr, "LaTeX marker at: %d, mode: %d\n",i,mmode);
          fflush(stderr);
        #endif /* DEBUG_OUT */
        Buffer[i]='\0';                             /* something ends here */
        if (mmode<=0) {                         /* we were not in mathmode */
          fputs(&Buffer[j],stdout); /* send the plain text ---> to TeXmacs */
          mmode = i+3;                                   /* start of LaTeX */
          fputs("\2latex:$$",stdout);/* tell ---> TeXmacs */
        } else {                                    /* we were in mathmode */
          HandleLatex(&Buffer[mmode]);            /* make some adjustments */
          fputs("$$\5",stdout); /* Say "that's all" ---> to TeXmacs */
          mmode = -1; j=i+3;                                  /* aftermath */
        }
      } else {                                     /* this is not a marker */
        if (mmode<=0) {                              /* if not in mathmode */
          if (strncmp(&Buffer[i],"Type:",5)==0) {   /* look for OpenAxiom Type */
            int k;
            k=i-1; while (Buffer[k]==' ') k--;
            Buffer[k]='\0';                        /* mark end of previous */
            fputs(&Buffer[j],stdout); /* send the plain text -> to TeXmacs */
            fprintf(stdout,"\2latex:\\openaxiomtype{%s}\5",
              &Buffer[i+6]);
            i=n; j=n;                                   /* and we are done */
          }
        }
      }
    };
    if (j<n) {
      fputs(&Buffer[j],stdout);    /* send the plain text ---> to TeXmacs */
    };
                              /* ask for some more work from ---> TeXmacs */
    fputs("\2channel:prompt\5\2latex:\\red$\\rightarrow$\\ \5\5",stdout);
    fflush(stdout);              /* Say "give me a prompt" ---> to TeXmacs */
    fputs("\2verbatim:",stdout);  /* maybe plain text next ---> to TeXmacs */
  }
}

HandleLatex(char buf[]) {                           /* fixup Latex coding */

  char *ptr1, *ptr2, *ptr3;
  char label[64];

/*   /n -> blank */
  while (ptr1=strchr(buf,'\n')) { *ptr1=' '; };

if (option_texbreak) { /* break long TeX lines */

/* prepare OpenAxiom output in buf for call to texbreak */

/*   remove the label and save it for later */
  label[0]='\0';
  while (ptr1=strstr(buf,"\\leqno(")) {
    if ((ptr2=strchr(ptr1,')')) && (ptr2-ptr1<64)) {
      ptr2++;
      strncpy(label,ptr1,ptr2-ptr1); label[ptr2-ptr1]='\0';
      strcpy(ptr1,ptr2);
    }
  };

  texbreak(buf);  /* output is in global external called bufout */
  strcat(bufout,"\\hfill \\leqno ");
  strcat(bufout,&label[6]); /* put label back */
} else {
  strcpy(bufout,buf);
};

/* do some LaTex conversions for TeXmacs */

/*      \root { a } \of { b } ---> \sqrt[ a ] { b }       */
/*  ptr1^ ptr2^   ^ptr3 ^          ^ ptr1^   ^            */

  while (ptr1=strstr(bufout,"\\root")) {
    ptr2=ptr1+5; while (*ptr2==' ') ptr2++;
    if ((*ptr2=='{') && (ptr3=strstr(ptr2,"}"))) {
      #ifdef DEBUG_OUT
        fprintf(stderr, "ptr1: %s\nptr2: %s\nptr3: %s\n",ptr1,ptr2,ptr3);
        fflush(stderr);
      #endif /* DEBUG_OUT */
      strncpy(ptr1,"\\sqrt[",6); ptr1=ptr1+6;
      strncpy(ptr1,ptr2+1,ptr3-ptr2-1); ptr1=ptr1+(ptr3-ptr2-1);
      strncpy(ptr1,"]",1); ptr1++;
      ptr3++; while (*ptr3==' ') ptr3++;
      #ifdef DEBUG_OUT
        fprintf(stderr, "ptr1: %s\nptr2: %s\nptr3: %s\n",ptr1,ptr2,ptr3);
        fflush(stderr);
      #endif /* DEBUG_OUT */
      if (strncmp(ptr3,"\\of",3)==0) {
        ptr3+=3; while (*ptr3==' ') ptr3++;
        if (*ptr3=='{') {
          strcpy(ptr1,ptr3);
        } else {
         error("No \\of { \n");
        }
      } else {
        error("No \\of \n");
      }
    } else {
      error("No \\root { } \n");
    }
  };

  if (!option_big_space) { /* use smaller spaces \ ---> \,   */
    while (ptr1=strstr(bufout,"\\ ")) {
      strncpy(ptr1,"\\,",2); /* use thin spaces */
    };
  };

/* other possible conversions    */

  if (!option_use_cdot) {
    while (ptr1=strstr(bufout,"\\cdot")) {
      strncpy(ptr1,"\\ ",2);
      strcpy(ptr1+2,ptr1+5);
    };
  };

  if (!option_big_paren) {
    while (ptr1=strstr(bufout,"\\left(")) {
      strncpy(ptr1,"(",1);
      strcpy(ptr1+1,ptr1+6);
    };
    while (ptr1=strstr(bufout,"\\right)")) {
      strncpy(ptr1,")",1);
      strcpy(ptr1+1,ptr1+7);
    };
  };
  if (!option_use_over) {
    while (ptr1=strstr(bufout,"\\over")) {
      strncpy(ptr1,"/",1);
      strcpy(ptr1+1,ptr1+5);
    };
  };
  #ifdef DEBUG_OUT
   fprintf(stderr, "HandleLatex:\n%s\n",bufout);
   fflush(stderr);
  #endif /* DEBUG_OUT */
  fputs(bufout,stdout);                /* send the LaTeX code ---> to TeXmacs */
}

unsigned __stdcall StdErrReadThread ( void* pArguments )
{
    int nExitCode = STILL_ACTIVE;
    int nRead;
    char szBuffer[OUT_BUFF_SIZE];

    GetExitCodeProcess ( hProcess, (unsigned long*) &nExitCode );

    while ( nExitCode == STILL_ACTIVE ) {
        #ifdef DEBUG_OUT
          fprintf ( stderr, " - Stderr read loop\n" ); fflush ( stderr );
        #endif /* DEBUG_OUT */        
        nRead = _read ( fdStdErrPipe[READ_HANDLE], szBuffer, OUT_BUFF_SIZE );
        #ifdef DEBUG_OUT
          fprintf ( stderr, "After _read on stderrpipe (%d bytes read)\n",
                    nRead );
          fflush ( stderr );
        #endif /* DEBUG_OUT */        
        if ( nRead ) {       /* just pass the message on through to TeXmacs */
            fwrite(szBuffer, 1, nRead, stderr);
        }
        GetExitCodeProcess ( hProcess, (unsigned long*) &nExitCode );
    }
    _endthreadex(0);
}

void pipe_write(int fdPipe, char Buffer[])
{
  _write(fdPipe, Buffer, strlen(Buffer) );
}

/* process texmacs options */

process_options(char line[]) {
  char *optargs[2];

  optargs[1]=line;
  if (*(optargs[1])!='\n') {
    parse_options(2,optargs);
  } else {                                  /* display current options */
    show_options();
  };
                            /* ask for some more work from ---> TeXmacs */
  fputs("\2channel:prompt\5\2latex:\\red$\\rightarrow$\\ \5\5",stdout);
  fflush(stdout);              /* Say "give me a prompt" ---> to TeXmacs */
  fputs("\2verbatim:",stdout);  /* maybe plain text next ---> to TeXmacs */

  #ifdef DEBUG_OUT
    fprintf ( stderr,
      "TM_OPENAXIOM=texbreak:%d,use_over:%d,use_cdot:%d,big_space:%d,big_paren:%d,\n",
      option_texbreak, option_use_over, option_use_cdot,
      option_big_space, option_big_paren );
    fprintf ( stderr,
      "         maxLineWidth:%d\n", maxLineWidth );
    fflush ( stderr );
  #endif /* DEBUG_OUT */
}

show_options() {
  fprintf(stdout,
"--------------------------- The texmacs Option ----------------------------\n\
\n\
 Description: options for display of OPENAXIOM output in TeXmacs\n\
\n\
 )set output texmacs is used to control the TeXmacs-OPENAXIOM interface\n\
The default values are controlled by environment variable TM_OPENAXIOM\n\
and may be overriden by command line options.\n\
\n\
Syntax:   )set output texmacs <arg>\n\
    where arg can be one or more of\n\
  break <on>|<off>      line-break algorithm\n\
  over <on>|<off>       convert 2-d \\over to 1-d /\n\
  cdot <on>|<off>       convert \\cdot to \\ (space)\n\
  space <on>|<off>      convert \\ (space) to \\,\n\
  big( <on>|<off>       convert \\left( \\right to ( )\n\
  width <9.99>          line width in inches\n\
\n\
    <on> may be on, yes, 1\n\
    <off> may be off, no , 0\n\
\n\
The current settings are:\n\
  break %d, over %d, cdot %d, space %d, big( %d, width %2.3f\n",
   option_texbreak, option_use_over, option_use_cdot,
   option_big_space, option_big_paren, maxLineWidth/1000.0 );

};

int main(int nargs, char *args[])
{
  HANDLE hStdOutReadThread;
  int fdStdOut, fdStdIn;
  unsigned threadIDOut;
  HANDLE hStdErrReadThread;
  int fdStdErr;
  unsigned threadIDErr;
  int i;
  char line[INP_BUFF_SIZE];

  parse_options(nargs,args);

  #ifdef DEBUG_OUT
    fprintf ( stderr, "CREATE PROCESS: %s\n", OPENAXIOM_cmd );
    fprintf ( stderr, "Setting env var: %s\n", ENV_OPENAXIOM );
    fprintf ( stderr,
     "TM_OPENAXIOM=texbreak:%d,use_over:%d,use_cdot:%d,big_space:%d,big_paren:%d,\n",
      option_texbreak, option_use_over, option_use_cdot,
      option_big_space, option_big_paren );
    fprintf ( stderr,
      "         maxLineWidth:%d\n", maxLineWidth );
    fflush ( stderr );
  #endif /* DEBUG_OUT */

  if ( -1 == _setmode( _fileno( stdin ), _O_BINARY ) ) {
    perror ( "machinfo: Cannot set stdin BINARY mode" ); exit(1);
  };
 if ( -1 == _setmode( _fileno( stdout ), _O_BINARY ) ) {
	perror ( "machinfo: Cannot set stdout BINARY mode" ); exit(1);
  };
  if ( -1 == _setmode( _fileno( stderr ), _O_BINARY ) ) {
    perror ( "machinfo: Cannot set stderr BINARY mode" ); exit(1);
  };

  /* Make pipes to be passed to the spawned process as stdin/out/err  */
  if ( _pipe ( fdStdOutPipe, 512, O_BINARY | O_NOINHERIT ) == -1 ) return   1;
  if ( _pipe ( fdStdInPipe,  512, O_BINARY | O_NOINHERIT ) == -1 ) return   1;
  if ( _pipe ( fdStdErrPipe, 512, O_BINARY | O_NOINHERIT ) == -1 ) return   1;

  /* Duplicate and save original stdin/out/err handles */
  fdStdOut = _dup ( _fileno(stdout) );
  fdStdIn  = _dup ( _fileno(stdin) );
  fdStdErr = _dup ( _fileno(stderr) );

  /* Duplicate write end of new pipes to current stdout/err handles,
   * read to stdin */
  if ( _dup2 ( fdStdOutPipe[WRITE_HANDLE], _fileno(stdout) ) != 0 ) return 2;
  if ( _dup2 ( fdStdInPipe[READ_HANDLE],   _fileno(stdin)  ) != 0 ) return 2;
  if ( _dup2 ( fdStdErrPipe[WRITE_HANDLE], _fileno(stderr) ) != 0 ) return 2;
  /* Close the duplicated handles to the new pipes */
  close ( fdStdOutPipe[WRITE_HANDLE] );
  close ( fdStdInPipe[READ_HANDLE] );
  close ( fdStdErrPipe[WRITE_HANDLE] );
    
  putenv ( ENV_OPENAXIOM );

  /* Zero startup and process info structures, take care of Windows
   * startup info structure future proofing. */
  ZeroMemory( &si, sizeof(si) );
  si.cb = sizeof(si);
  ZeroMemory( &pi, sizeof(pi) );

  /* Start the child process.  */
  if ( !CreateProcess(
      NULL,        /* No module name (use command line). */
      OPENAXIOM_cmd,                        /* Command line. */
      NULL,           /* Process handle not inheritable. */
      NULL,            /* Thread handle not inheritable. */
      TRUE,                 /* Allow handle inheritance. */
      0,                           /* No creation flags. */
      NULL,           /* Use parent's environment block. */
      NULL,          /* Use parent's starting directory. */
      &si,           /* Pointer to STARTUPINFO structure.*/
      &pi ) /* Pointer to PROCESS_INFORMATION structure. */ ) {
    fprintf(stderr, "CreateProcess failed: %s\n", args[1]);
    fflush(stderr);
    return -1;
  }
  hProcess = pi.hProcess;

  /* Now that the process is launched,
   * replace the original stdin/out/err handles */
  if ( _dup2 ( fdStdOut, _fileno ( stdout ) ) != 0 ) return 3;
  if ( _dup2 ( fdStdIn,  _fileno ( stdin  ) ) != 0 ) return 3;
  if ( _dup2 ( fdStdErr, _fileno ( stderr ) ) != 0 ) return 3;

  /* Close duplicates */
  close(fdStdOut);
  close(fdStdIn);
  close(fdStdErr);

  /* The child process will become the OpenAxiom read filter and
   * we will be the OpenAxiom write filter. */

  /* Create the OpenAxiom stderr listening thread. (Doesn't do much.) */
  hStdErrReadThread = (HANDLE)_beginthreadex( NULL, 0, &StdErrReadThread,
    NULL, 0, &threadIDErr );
  if ( 0 == hStdErrReadThread ) return 5;

                                /* * * * * * * * * */
                               /* start talking!  */
                              /* * * * * * * * * */

  IgnorePrompts = 5;    /* Tell the OpenAxiom Reader to ignore first 5 prompts */
                             /* then create the OpenAxiom stdout Reader thread.*/
  hStdOutReadThread = (HANDLE)_beginthreadex( NULL, 0, &StdOutReadThread,
    NULL, 0, &threadIDOut );
  if ( 0 == hStdOutReadThread ) return 5;
                                        /* start force-feeding ---> to OpenAxiom */
  pipe_write(fdStdInPipe[WRITE_HANDLE],")set message prompt plain\n" ); /* 1 */
  pipe_write(fdStdInPipe[WRITE_HANDLE],")set messages autoload off\n" );/* 2 */
  pipe_write(fdStdInPipe[WRITE_HANDLE],")set quit unprotected\n" );     /* 3 */
  pipe_write(fdStdInPipe[WRITE_HANDLE],")set output tex on\n" );        /* 4 */
  pipe_write(fdStdInPipe[WRITE_HANDLE],")set output algebra off\n" );   /* 5 */
  while (fgets(line,INP_BUFF_SIZE,stdin)!=NULL) {   /* wait <--- for TeXmacs */
    #ifdef DEBUG_OUT
      fprintf ( stderr, "Input:\n%s", line );
    #endif /* DEBUG_OUT */
    if (strncmp(line,")set output texmacs",
      strlen(")set output texmacs"))==0) {       /* process texmacs options */
      process_options(&line[strlen(")set output texmacs")]);
    } else {
      pipe_write(fdStdInPipe[WRITE_HANDLE], line );      /* Start ---> OpenAxiom */
    }
  };
  if (nBuffer) HandleOutput(szBuffer,nBuffer);         /* anything leftover? */
  pipe_write(fdStdInPipe[WRITE_HANDLE], ")quit\n" );            /* stop work */
  
  WaitForSingleObject ( hStdErrReadThread, INFINITE );
  WaitForSingleObject ( hStdOutReadThread, INFINITE );

  /* Wait until child process exits to block the terminal. */
  WaitForSingleObject( pi.hProcess, INFINITE );
    
  /* As we are using gebinthreadex/endthreadex,
   * we must close the thread handles. */
  CloseHandle ( hStdOutReadThread );
  CloseHandle ( hStdErrReadThread );

  return 0;
}
