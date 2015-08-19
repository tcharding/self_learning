#include "socket.h"
#include <time.h>
#include <setjmp.h>

static void sig_alrm(int);	/* signal handler */
static jmp_buf env_alrm;

/* getnameinfo_timeo: adds timer to gotnameinfo */
int getnameinfo_timeo(const struct sockaddr *sa, socklen_t salen,
		      char *host, socklen_t hostlen,
		      char *serv, socklen_t servlen, int flags, int sec)
{
	int n;
	char *ptr;

	Signal(SIGALRM, sig_alrm);
	if (setjmp(env_alrm) != 0) { /* time out */
		if (flags & NI_NAMEREQD) {
			errno = ETIME;
			return -1;
		}
		if ( (ptr = Sock_ntop(sa, salen)) == NULL)
			return -1;
		host = ptr;	/* just return the address string */
		return 0;	/* success */
	}
	alarm(sec);		/* set alarm */

	sleep(100);		/* force sleep for testing */

	n = getnameinfo(sa, salen, host, hostlen, serv, servlen, flags);
	alarm(0);		/* we returned so deactivate alarm */

	return n;
}

/* sig_alrm: handle alarm signal */
static void sig_alrm(int sig)
{
	longjmp(env_alrm, 1);
} 
