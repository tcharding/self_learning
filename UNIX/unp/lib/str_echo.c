/* Authors: W. R. Stevens, B. Fenner, A. M. Rudoff */ 
 
#include "unp.h" 
 
void 
str_echo(int sockfd) 
{ 
	ssize_t n; 
	char buf[MAXLINE]; 
 
again: 
	while ( (n = read(sockfd, buf, MAXLINE)) > 0) {
		/* buf[n] = '\0'; */
		/* fprintf(stderr, "read: %ld: %s\n", (long)n, buf); */
		Writen(STDERR_FILENO, buf, n); 
		Writen(sockfd, buf, n); 
 		/* fprintf(stderr, "Wniten returned\n");  */
	}
	if (n < 0 && errno == EINTR) 
		goto again; 
	else if (n < 0) 
		err_sys("str_echo: read error"); 
} 
