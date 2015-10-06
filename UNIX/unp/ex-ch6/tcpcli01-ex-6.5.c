/* orginal file: ../tcpcliserv/tcpcli01.c */ 
#include "unp.h" 

#define ALRM_MSG "Caught SIGALRM\n"

void sig_alrm(int signo);
static volatile sig_atomic_t flag; /* flag set in sig catcher */
int sockfd; 

int main(int argc, char **argv) 
{ 
	ssize_t n;
	struct sockaddr_in servaddr;
	char buf[BUFSIZ];

	flag = 0;
	if (argc != 2) 
		err_quit("usage: tcpcli <IPaddress>"); 

	if (signal(SIGALRM, sig_alrm) == SIG_ERR)
		err_sys("signal error");
	
	sockfd = Socket(AF_INET, SOCK_STREAM, 0); 
 
	bzero(&servaddr, sizeof(servaddr)); 
	servaddr.sin_family = AF_INET; 
	servaddr.sin_port = htons(SERV_PORT); /* chargen server */
	Inet_pton(AF_INET, argv[1], &servaddr.sin_addr); 
 
	Connect(sockfd, (SA *) &servaddr, sizeof(servaddr)); 
	/* alarm(3); */
	/* pause(); */
	alarm(3);
	while ((flag == 0) &&
	       (((n = read(sockfd, buf, BUFSIZ)) > 0) || (errno == EINTR))) {
		writen(STDOUT_FILENO, buf, n);
		write(STDOUT_FILENO, "\n", 1);
	}
	if (n < 0)
		err_sys("read error");
	exit(0); 
} 

void sig_alrm(int signo)
{
	int err = errno;

	err = write(STDERR_FILENO, ALRM_MSG, sizeof(ALRM_MSG));
	if (shutdown(sockfd, SHUT_RD) < 0)
		err_sys("flag error");
	flag = 1;
	pause();
	/* sleep(10); */
	errno = err;
}
