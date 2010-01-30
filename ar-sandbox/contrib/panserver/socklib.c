/* My guide for sockets is 'Unix Network Programming' vol 1, third
 * edition by W. Richard Stevens, Bill Fenner and Andrew M. Rudoff
 * The header 'unp-abbrev.h' includes a few things from their much
 * larger header 'unp.h'.
 */


#include "unp-abbrev.h"

typedef int openaxiom_socket;
typedef int openaxiom_byte;

int Listener(int serverPort)
{
  int listenfd;
  struct sockaddr_in servaddr;

  if ( (listenfd = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
      return SOCKETERROR;
    }

  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
  servaddr.sin_port = htons(serverPort);

  if ( bind(listenfd, (SA *) &servaddr, sizeof(servaddr)) < 0 ) {
    return BINDERROR;
  }

  if ( listen(listenfd, LISTENQ) < 0 ) {
    return LISTENERROR;
  }

  return listenfd;
}

int ListenerAccept(int listenfd)
{
  int connfd;
  socklen_t clilen;
  struct sockaddr_in cliaddr;
  clilen = sizeof(cliaddr);
  if ( (connfd = accept(listenfd, (SA *) &cliaddr, &clilen)) < 0 ) {
    return ACCEPTERROR;
  }

  return connfd;
}


/*  This is used just for the timeout function */

int SpadSelect(openaxiom_socket sock, int secs, int usecs)
{
  int    maxfdp1;
  fd_set rset;
  struct timeval time;
  
  FD_ZERO(&rset);
  FD_SET(sock, &rset);
  maxfdp1 = sock + 1;
  time.tv_sec = secs;
  time.tv_usec = usecs;
  return select(maxfdp1, &rset, NULL, NULL, &time);
}


