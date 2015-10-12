#include "unp.h"


static void sig_alrm(int);

/* add time out to getnameinfo(3) */
int getnameinfo_timeo(const struct sockaddr *sa, socklen_t salen,
		char *host, socklen_t hostlen,
		char *serv, socklen_t servlen, int flags, int timeout)
{
	struct sigaction act, oact;
	int err = 0;
	int retval;
	struct sockaddr_in *addr;
	struct sockaddr_in6 *addr6;
	
	act.sa_handler = sig_alrm;
	sigemptyset(&act.sa_mask);
	act.sa_flags = 0;
	sigaction(SIGALRM, &act, &oact);

	alarm(timeout);
	if ((retval = getnameinfo(sa, salen, host, hostlen,
				  serv, servlen, flags)) != 0) {
		if (flags & NI_NAMEREQD) {
			err = EAI_FAIL;
		} else {
			switch (sa->sa_family) {
			case AF_INET:
				addr = (struct sockaddr_in *) sa;
				if (inet_ntop(addr->sin_family, &addr->sin_addr,
					      host, hostlen) == NULL)
					err = errno;
				break;
			case AF_INET6:
				addr6 = (struct sockaddr_in6 *) sa;
				if (inet_ntop(addr6->sin6_family, &addr6->sin6_addr,
					      host, hostlen) == NULL)
					err = errno;
				break;
			default:
				err_sys("family not supported");
			}

		}
	}
	sigaction(SIGALRM, &oact, NULL); /* restore */
	return err;
}

static void sig_alrm(int signo)
{
	;			/* just return */
}
