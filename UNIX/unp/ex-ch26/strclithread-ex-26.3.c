/* orginal file: ../threads/strclithread.c */ 
#include "unpthread.h" 
 
void *copyto(void *); 
 
static int sockfd; /* global for both threads to access */ 
static FILE *fp; 
int done;

void 
str_cli(FILE *fp_arg, int sockfd_arg) 
{ 
	char recvline[MAXLINE]; 
	pthread_t tid; 
	int nbytes;
	
	fprintf(stderr, "called our version\n");
	sockfd = sockfd_arg; /* copy arguments to externals */ 
	fp = fp_arg; 
 
	Pthread_create(&tid, NULL, copyto, NULL); 
 
	while (( nbytes = Readline(sockfd, recvline, MAXLINE)) > 0) 
		Fputs(recvline, stdout);
	
	if ((nbytes == 0) && (done == 0))
		fprintf(stderr, "Server prematurely shutdown\n");
} 
 
void * 
copyto(void *arg) 
{ 
	char sendline[MAXLINE]; 
 
	while (Fgets(sendline, MAXLINE, fp) != NULL) 
		Writen(sockfd, sendline, strlen(sendline));

	done = 1;
	Shutdown(sockfd, SHUT_WR); /* EOF on stdin, send FIN */ 
 
	return(NULL); 
	/* 4return (i.e., thread terminates) when EOF on stdin */ 
} 
