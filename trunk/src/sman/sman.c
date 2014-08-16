/*
  Copyright (c) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2014, Gabriel Dos Reis.
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

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <pwd.h>
#include <fcntl.h>
#include <termios.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <signal.h>
#include <locale.h>

#include "open-axiom.h"
#include "sockio.h"
#include "com.h"
#include "bsdsignal.h"
#include "sman.h"

#include "sockio.h"
#include "openpty.h"
#include "cfuns.h"

using namespace OpenAxiom;

static void process_arguments(Command*, int, char**);
static int in_X(void);
static void set_up_defaults(void);
static void process_options(Command*, int, char**);
static void death_handler(int);
static void sman_catch_signals(void);
static void fix_env(int);
static void init_term_io(void);
static const char* strPrefix(const char* , const char*);
static void check_spad_proc(const char* , const char*);
static void clean_up_old_sockets(void);
static SpadProcess* fork_you(int);
static void exec_command_env(const char*);
static SpadProcess* spawn_of_hell(const char* , int);
static void start_the_spadclient(void);
static void start_the_local_spadclient(void);
static void start_the_session_manager(void);
static void start_the_hypertex(Command*);
static void start_the_graphics(Command*);
static void fork_Axiom(Command*);
static void start_the_Axiom(Command*);
static void clean_up_sockets(void);
static void clean_hypertex_socket(void);
static void read_from_spad_io(int);
static void read_from_manager(int);
static void manage_spad_io(int);
static SpadProcess* find_child(int);
static void kill_all_children(void);
static void monitor_children(void);

/* System defined pointer to array or environment variables.  */
extern char** environ;

int start_clef;                 /* start clef under spad */
int start_graphics;             /* start the viewman */
int start_ht;                   /* start hypertex */
int start_spadclient;           /* Start the client spad buffer */
int start_local_spadclient;     /* Start the client spad buffer */
int use_X;                      /* Use the X windows environment */
int server_num;                 /* OpenAxiom server number */

/************************************************/
/* definitions of programs which sman can start */
/************************************************/

const char    *GraphicsProgram        = "/lib/viewman";
const char    *HypertexProgram        = "/lib/hypertex";
const char    *ClefProgram            = "$AXIOM/bin/clef -f $AXIOM/lib/command.list -e ";
const char    *SessionManagerProgram  = "$AXIOM/lib/session";
const char    *SpadClientProgram      = "$AXIOM/lib/spadclient";
char    *PasteFile              = NULL;
char    *MakeRecordFile         = NULL;
char    *VerifyRecordFile       = NULL;

SpadProcess *spad_process_list = NULL;
/***************************/
/* sman defaults file name */
/***************************/

#define SpadDefaultFile "spadprof.input"

char ClefCommandLine[256];

#define BufSize      4096       /* size of communication buffer */
char big_bad_buf[BufSize];      /* big I/O buffer */

openaxiom_sio* session_io = NULL; /* socket connecting to session manager */

/***********************************************************/
/* Some characters used and externally defined in edible.h */
/***********************************************************/

unsigned char  _INTR, _QUIT, _ERASE, _KILL, _EOF, _EOL, _RES1, _RES2;

/*************************************/
/* Stuff for opening pseudo-terminal */
/*************************************/

int ptsNum, ptcNum;
char ptsPath[20];

int child_pid;                  /* child's process id */
struct termios oldbuf;           /* the original settings */
struct termios childbuf;         /* terminal structure for user i/o */


int death_signal = 0;

static void
process_arguments(Command* command, int argc,char ** argv)
{
  int arg;
  int other = 0;
  for (arg = 1; arg < argc; arg++) {
    if (strcmp(argv[arg], "-noclef")      == 0)
      start_clef = 0;
    else if (strcmp(argv[arg], "-clef")        == 0)
      start_clef = 1;
    else if (strcmp(argv[arg], "-gr") == 0
             || strcmp(argv[arg], "--graph") == 0 ) {
      if (!OPENAXIOM_HAVE_GRAPHICS)
        fprintf(stderr, "OpenAxiom was not build with Graphics support.\n");
      else
        start_graphics = 1;
    }
    else if (strcmp(argv[arg], "-nogr") == 0
             || strcmp(argv[arg], "--no-graph") == 0)
      start_graphics = 0;
    else if (strcmp(argv[arg], "-ht") == 0
             || strcmp(argv[arg], "--hyperdoc") == 0) {
      if (!OPENAXIOM_HAVE_GRAPHICS)
        fprintf(stderr, "OpenAxiom was not build with HyperDoc support.\n");
      else
        start_ht = 1;
    }
    else if (strcmp(argv[arg], "-noht") == 0
             || strcmp(argv[arg], "--no-hyperdoc") == 0)
      start_ht = 0;
    else if (strcmp(argv[arg], "-iw")          == 0)
      start_spadclient = 1;
    else if (strcmp(argv[arg], "-ihere")       == 0)
      start_local_spadclient = 1;
    else if (strcmp(argv[arg], "-noihere")     == 0)
      start_local_spadclient = 0;
    else if (strcmp(argv[arg], "-noiw")        == 0)
      start_spadclient = 0;
    else if (strcmp(argv[arg], "-nox") == 0
             || strcmp(argv[arg], "--no-gui") == 0) {
        use_X = 0;
        start_local_spadclient = 1;
        start_spadclient = 0;
        start_ht = 0;
        start_graphics = 0;
    }
    else if (strcmp(argv[arg], "-clefprog")    == 0) {
      strcpy(ClefCommandLine,argv[++arg]);
      ClefProgram = 
        strcat(ClefCommandLine, " -f $AXIOM/lib/command.list -e ");
    }
    else if (strcmp(argv[arg], "-rm")  == 0)
      MakeRecordFile = argv[++arg];
    else if (strcmp(argv[arg], "-rv")  == 0)
      VerifyRecordFile = argv[++arg];
    else if (strcmp(argv[arg], "-paste")  == 0)
      PasteFile = argv[++arg];
    else
       argv[other++] = argv[arg];
  }

  command->core.argv = argv;
  command->core.argc = other;

/* If there were no X libraries
 * at build-time, we proceed to
 * overwrite the defaults startup 
 * values to not start any of the 
 * graphical components of 
 * OpenAxiom (Hyperdoc, Graphics). */
  
  if (!OPENAXIOM_HAVE_GRAPHICS) {
    use_X = 0;
    start_local_spadclient = 1;
    start_ht = 0;
    start_graphics = 0;
  }
}

static int 
in_X(void)
{
  if (oa_getenv("DISPLAY")) return 1;
  return 0;
}

static  void
set_up_defaults(void)
{
  start_clef = 1;
  start_graphics = 1;
  start_ht = 1;
  start_spadclient = 0;
  start_local_spadclient = 1;
  use_X = isatty(0) && in_X();
}

static void
process_options(Command* command, int argc, char **argv)
{
  set_up_defaults();
  process_arguments(command, argc, argv);
   /* Complain about command line arguments unknown to Superman.  */
   if (command->core.argc > 0) {
      int i;
      for (i = 0; i < command->core.argc; ++i)
         fprintf(stderr,"command line error: %s\n", command->core.argv[i]);
      exit(-1);
   }
}

static void
death_handler(int sig)
{
  death_signal = 1;
}

static void
sman_catch_signals(void)
{
  
  /* Set up the signal handlers for sman */
  bsdSignal(SIGINT,  SIG_IGN,RestartSystemCalls);
  bsdSignal(SIGTERM, death_handler,RestartSystemCalls);
  bsdSignal(SIGQUIT, death_handler,RestartSystemCalls);
  bsdSignal(SIGHUP,  death_handler,RestartSystemCalls);
  bsdSignal(SIGILL,  death_handler,RestartSystemCalls);
  bsdSignal(SIGTRAP, death_handler,RestartSystemCalls);

#ifdef SIGABRT
  bsdSignal(SIGABRT, death_handler,RestartSystemCalls);
#else
 #ifdef SIGIOT
  bsdSignal(SIGIOT,  death_handler,RestartSystemCalls);
 #endif
#endif

  bsdSignal(SIGBUS,  death_handler,RestartSystemCalls);
  bsdSignal(SIGSEGV, death_handler,RestartSystemCalls);

}

static void
fix_env(int spadnum)
{
  char sn[20];
  sprintf(sn, "%d", spadnum);
  oa_setenv("SPADNUM", sn);
  oa_setenv("SPADSERVER", "TRUE");
}

static void
init_term_io(void)
{
  if(!isatty(0)) return;
  if( tcgetattr(0, &oldbuf) == -1) {
    perror("getting termios");
    return ;                    /*  exit(-1); */
  }
  if( tcgetattr(0, &childbuf) == -1) {
    perror("getting termios");
    return ;                    /*   exit(-1); */
  }
  _INTR = oldbuf.c_cc[VINTR];
  _QUIT = oldbuf.c_cc[VQUIT];
  _ERASE = oldbuf.c_cc[VERASE];
  _KILL = oldbuf.c_cc[VKILL];
  _EOF = oldbuf.c_cc[VEOF];
  _EOL = oldbuf.c_cc[VEOL];
}

static const char*
strPrefix(const char* prefix, const char* s)
{
  while (*prefix != '\0' && *prefix == *s) {
    prefix++;
    s++;
  }
  if (*prefix == '\0') return s;
  return NULL;
}

static void
check_spad_proc(const char *file, const char *prefix)
{
  const char *num;
  int pid;
  if ((num = strPrefix(prefix, file))) {
    pid = atoi(num);
    if (pid > 2) {
      kill(pid, 0);
      if (kill(pid, 0) == -1 && errno == ESRCH) {
        unlink(file);
      }
    }
  }
}

static void
clean_up_old_sockets(void)
{
  char com[512], tmp_file[128];
  FILE *file;
  int len;
  sprintf(tmp_file, "/tmp/socks.%d", server_num);
  sprintf(com, "ls /tmp/.d* /tmp/.s* /tmp/.i* /tmp/.h* 2> %s > %s",
          tmp_file, tmp_file);
  system(com);
  file = fopen(tmp_file, "r");
  if (file == NULL) {
    fprintf(stderr, "Can't open socket listing file\n");
    return;
  }
  while(fgets(com, 512, file) != NULL) {
    len = strlen(com);
    if (len) com[len-1] = '\0';
    else break;
    check_spad_proc(com, "/tmp/.d");
    check_spad_proc(com, "/tmp/.s");
    check_spad_proc(com, "/tmp/.i");
    check_spad_proc(com, "/tmp/.h");
  }
  fclose(file);
  unlink(tmp_file);
}

static SpadProcess *
fork_you(int death_action)
{
  /* fork a new process, giving it a default death action */
  /* return NULL in child, SpadProcess in parent          */
  int child_pid = fork();
  SpadProcess *proc;
  if (!child_pid) return NULL;
  proc = (SpadProcess *) malloc(sizeof(SpadProcess));
  proc->proc_id = child_pid;
  proc->death_action = death_action;
  proc->command = NULL;
  proc->next = spad_process_list;
  spad_process_list = proc;
  return proc;
}

static void
exec_command_env(const char *command)
{
  char new_command[512];
  sprintf(new_command, "exec %s", command);
  execle("/bin/sh","/bin/sh", "-c", new_command, (char*)NULL, environ);
}

static SpadProcess *
spawn_of_hell(const char *command, int death_action)
{
  SpadProcess *proc = fork_you(death_action);
  if (proc != NULL) {
    proc->command = command;
    return proc;
  }
  exec_command_env(command);
  return NULL;
}

static void
start_the_spadclient(void)
{
  char command[256];
  if (start_clef)
     sprintf(command, 
             "xterm -sb -sl 500 -name axiomclient -n OpenAxiom -T OpenAxiom -e %s %s",
             ClefProgram, SpadClientProgram);
  else
     sprintf(command, 
             "xterm -sb -sl 500 -name axiomclient -n OpenAxiom -T OpenAxiom -e %s", 
             SpadClientProgram);
  spawn_of_hell(command, NadaDelShitsky);
}

static void
start_the_local_spadclient(void)
{
  char command[256];
  if (start_clef)
    sprintf(command, "%s  %s", ClefProgram, SpadClientProgram);
  else
    sprintf(command, "%s", SpadClientProgram);
  spawn_of_hell(command, NadaDelShitsky);
}

static void
start_the_session_manager(void)
{
  spawn_of_hell(SessionManagerProgram, Die);
}

static void
start_the_hypertex(Command* cmd)
{
  const char* root_dir = cmd->root_dir;
  const char* command = oa_concatenate_string(root_dir,HypertexProgram);

  if (readablep(command) != 1) {
    fprintf(stderr, "Hypertex program not found.\n");
    return;
  } 
  if (PasteFile){
    command = oa_concatenate_string(command, " -s -k -ip ");
    command = oa_concatenate_string(command, PasteFile);
    spawn_of_hell(command, NadaDelShitsky);
  }
  else if (MakeRecordFile){
    command = oa_concatenate_string(command, " -s -k -rm ");
    command = oa_concatenate_string(command, MakeRecordFile);
    spawn_of_hell(command, NadaDelShitsky);
  }
  else if (VerifyRecordFile){
    command = oa_concatenate_string(command, " -s -k -rv ");
    command = oa_concatenate_string(command, VerifyRecordFile);
    spawn_of_hell(command, NadaDelShitsky);
  }
  else
    spawn_of_hell(command, CleanHypertexSocket);
}

static void
start_the_graphics(Command* cmd)
{
  const char* root_dir = cmd -> root_dir;
  const char* command = oa_concatenate_string(root_dir,GraphicsProgram);

  if (readablep(command) == 1)
    spawn_of_hell(command, DoItAgain);
  else
    fprintf(stderr, "Graphics program not found.\n");
}

/* Start the core executable session in a separate process, */
/* using a pseudo-terminal to catch all input and output */
static void 
fork_Axiom(Command* cmd)
{
  SpadProcess *proc;

  proc =  fork_you(Die);
  child_pid = (proc == NULL ? 0 : proc->proc_id);
  switch(child_pid) {
  case -1 :
    fprintf(stderr, "Can't create a new process \n");
    exit(0);
  case 0:
    /* Dissasociate from my parents group so all my child processes */
    /* look at my terminal as the controlling terminal for the      */
    /* group                                                        */

    if(setsid() < 0) {
      perror("Dissassociating from parents group");
      exit(-1);
    }

    close(ptsNum);
    /* Now reopen the server side, so that pg, su, etc. work properly */

    if ((ptsNum =  open(ptsPath, O_RDWR)) < 0 ) {
      perror("fork_Axiom: Failed to reopen server");
      exit(-1);
    }
#if defined(SUN4OS5platform) || defined(HP10platform)
    ioctl(ptsNum,I_PUSH,"ptem");
    ioctl(ptsNum,I_PUSH,"ldterm");
#endif

    /* since I am the child, I can close ptc, and dup pts for all its */
    /* standard descriptors                                           */

    if( (dup2(ptsNum, 0) == -1) ||
        (dup2(ptsNum, 1) == -1) ||
        (dup2(ptsNum, 2) == -1)  ) {
      perror("trying to dupe the child");
      exit(-1);
    }
    close(ptcNum);
    close(ptsNum);


    /* I also have to turn off echoing, since I am echoing all the */
    /* input myself                  */

    childbuf.c_lflag &= ~ECHO;
    if( tcsetattr(0, TCSAFLUSH, &childbuf) == -1) {
      perror("setting the term buffer");
      exit(-1); 
    }

    /* Tell the Core that it is being invoked in server mode.   */
    oa_allocate_process_argv(&cmd->core, 2);
    cmd->core.argv[0] = (char*) make_path_for(cmd->root_dir, Driver::core);
    cmd->core.argv[1] = (char*) "--role=server";
    execute_core(cmd, Driver::core);
  }
}

static void
start_the_Axiom(Command* cmd)
{
  server_num = make_server_number();
  clean_up_old_sockets();
  if (server_num == -1) {
    fprintf(stderr, "could not get an OpenAxiom server number\n");
    exit(-1);
  }
  if (ptyopen(&ptcNum, &ptsNum, ptsPath) == -1) {
    perror("start_the_Axiom: ptyopen failed");
    exit(-1);
  }
  fix_env(server_num);
  fork_Axiom(cmd);
  close(ptsNum);
}

static void
clean_hypertex_socket(void)
{
   char name[256];
   sprintf(name, "%s%d", MenuServerName, server_num);
   unlink(name); 
}

static void
clean_up_sockets(void)
{
  char name[256];
  sprintf(name, "%s%d", SpadServer, server_num);
  unlink(name);
  sprintf(name, "%s%d", SessionServer, server_num);
  unlink(name);
  sprintf(name, "%s%d", SessionIOName, server_num);
  unlink(name);
  clean_hypertex_socket();
}

static void
read_from_spad_io(int ptcNum)
{
  int ret_code = 0, i=0;
  static int mes_len =0; 
  ret_code = read(ptcNum, big_bad_buf, BufSize);
  if (ret_code == -1) {
    clean_up_sockets();
    exit(-1);
  }
  if (session_io == NULL) {
    if (ret_code < mes_len)
      mes_len -= ret_code;
    else {
      if (mes_len > 0) {
        i = mes_len;
        mes_len = 0;
      }
      else
        i = 0;
      ret_code = write(1, big_bad_buf+i, ret_code-i);
    }
  }
  else
    ret_code = swrite(session_io, byte_address(big_bad_buf), ret_code,
                      "writing to session man");
  if (ret_code == -1) {
    perror("writing output to session manager");
    clean_up_sockets();
    exit(-1);
  }
}

static void
read_from_manager(int ptcNum)
{
  int ret_code;
  ret_code = sread(session_io, byte_address(big_bad_buf), BufSize,
                   "reading session io");
  if (ret_code == -1) {
    return;
  }
  ret_code = write(ptcNum, big_bad_buf, ret_code);
  if (ret_code == -1) {
    return;
  }
}

static void
manage_spad_io(int ptcNum)
{
  int ret_code, p;
  fd_set rd;
  while (1) {
    rd = socket_mask;
    FD_SET(ptcNum, &rd);
    if (session_io != NULL)
      FD_SET(session_io->socket, &rd);
    ret_code = sselect(FD_SETSIZE, &rd, 0, 0, NULL);
    if (ret_code == -1) {
      perror("Session manager select");
      clean_up_sockets();
      exit(-1);
    }
    if (FD_ISSET(ptcNum, &rd)) {
      read_from_spad_io(ptcNum);
    }
    if (server.socket > 0 && FD_ISSET(server.socket, &rd)) {
       p = accept_connection();
       switch(p) {
       case SessionIO:
          session_io = purpose_table[SessionIO];
          /*  printf("connected session manager\n\r");*/
          printf("\n");
          break;
       default:
          printf("sman: Unknown connection request type: %d\n", p);
          break;
       }
    }
    if (session_io != NULL && FD_ISSET(session_io->socket, &rd)) {
      read_from_manager(ptcNum);
    }
  }
}

#if 0
static void
print_spad_process_list()
{
  SpadProcess *proc;
  for(proc = spad_process_list; proc != NULL; proc = proc->next)
    fprintf(stderr, "proc_id = %d, death_action = %d\n", proc->proc_id,
            proc->death_action);
}
#endif

static SpadProcess *
find_child(int proc_id)
{
  SpadProcess *proc;
  for(proc = spad_process_list; proc != NULL; proc = proc->next)
    if (proc->proc_id == proc_id) return proc;
  return NULL;
}

static void
kill_all_children(void)
{
  char name[256];
  SpadProcess *proc;
  
  
  for(proc = spad_process_list; proc != NULL; proc = proc->next) {
    kill(proc->proc_id, SIGTERM);
  }
  sprintf(name, "/tmp/hyper%d.input",server_num);
  unlink(name);

}

static void
monitor_children(void)
{
  int dead_baby, stat;
  SpadProcess *proc;
  while (1) {
    stat = 0;
    dead_baby = wait(&stat);
    /* Check the value of dead_baby, since wait may have returned
       a pid but subsequently we have received a signal.  Yeuch! */
    if (dead_baby == -1 && death_signal) {
      kill_all_children();
      clean_up_sockets();
      openaxiom_sleep(2);
      exit(0);
    }

    if (dead_baby == -1) {
      fprintf(stderr, "sman: wait returned -1\n");
      continue;
    }
    proc = find_child(dead_baby);
    if (proc == NULL) {
      /*      fprintf(stderr, "sman: %d is not known to be a child process\n",
              dead_baby);
              */
      continue;
    }
    switch(proc->death_action) {
    case Die:
      kill_all_children();
      clean_up_sockets();
      openaxiom_sleep(2);
      exit(0);
    case NadaDelShitsky:
      break;
    case DoItAgain:
      spawn_of_hell(proc->command, DoItAgain);
      break;
    case CleanHypertexSocket:
      clean_hypertex_socket();
      break;
    }
  }
}

int
main(int argc, char *argv[])
{
  Command command;
  command.root_dir = get_systemdir(argc, argv);
  process_options(&command, argc, argv);

  oa_setenv("LC_ALL", "C");
  setlocale(LC_ALL, "");
  bsdSignal(SIGINT,  SIG_IGN,RestartSystemCalls);
  init_term_io();
  spad_process_list = NULL;
  start_the_Axiom(&command);
  if (open_server(SessionIOName) == -2) {
    fprintf(stderr, "Fatal error opening I/O socket\n");
    clean_up_sockets();
    exit(-1);
  }
  start_the_session_manager();
  if (start_spadclient)       
    start_the_spadclient();
  if (start_local_spadclient) 
    start_the_local_spadclient();
  if (start_ht and in_X())
    start_the_hypertex(&command);
  if (start_graphics and in_X())
    start_the_graphics(&command);
  openaxiom_sleep(1);

  if (fork_you(Die) != NULL) {
    sman_catch_signals();
    monitor_children();
    exit(0);
  }
  manage_spad_io(ptcNum);
  return(0);
}


