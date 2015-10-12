/* orginal file: ../lib/connect_timeo.c */ 

#include "unp.h" 
 
static void connect_alarm(int); 
 
int connect_timeo(int sockfd, const SA *saptr, socklen_t salen, int nsec) 
{ 
	Sigfunc *sigfunc; 
	int n, remaining;
 
	sigfunc = Signal(SIGALRM, connect_alarm); 
	remaining = alarm(nsec); /* 0 if no previous alarm */
	
	if ( (n = connect(sockfd, saptr, salen)) < 0) { 
		close(sockfd); 
		if (errno == EINTR) 
			errno = ETIMEDOUT; 
	} 
	alarm(remaining);     /* re-activate previous alarm or turn off alarm */
	Signal(SIGALRM, sigfunc); /* restore previous signal handler */ 
 
	return(n); 
} 
 
static void 
connect_alarm(int signo) 
{ 
	return; /* just interrupt the connect() */ 
} 
/* end connect_timeo */ 
 
void 
Connect_timeo(int fd, const SA *sa, socklen_t salen, int sec) 
{ 
	if (connect_timeo(fd, sa, salen, sec) < 0) 
		err_sys("connect_timeo error"); 
} 
