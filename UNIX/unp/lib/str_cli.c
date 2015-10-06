/* Authors: W. R. Stevens, B. Fenner, A. M. Rudoff */ 
 
#include "unp.h" 
 
void 
str_cli(FILE *fp, int sockfd) 
{ 
	char sendline[MAXLINE], recvline[MAXLINE]; 
 
	while (Fgets(sendline, MAXLINE, fp) != NULL) { 
		/* fprintf(stderr, "Fgets returned non-null\n"); */
		Writen(sockfd, sendline, strlen(sendline)); 
 		/* fprintf(stderr, "Wniten returned\n"); */
		if (Readline(sockfd, recvline, MAXLINE) == 0) 
			err_quit("str_cli: server terminated prematurely"); 
  		/* fprintf(stderr, "Readline returned\n"); */
		Fputs(recvline, stdout); 
	} 
} 
