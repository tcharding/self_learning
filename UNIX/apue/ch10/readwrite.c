#include "apue.h"
#include <sys/resource.h>
#define BUFFSIZE 1024
#define SIG_MSG "signal SIGXFSZ caught\n"

void sig_xfsz(int);

/* read stdin write stdout */
int main(void)
{
	int readn, writen;
	char buf[BUFFSIZE];
	struct rlimit rl;

	if (signal_intr(SIGXFSZ, sig_xfsz) == SIG_ERR)
		err_sys("signal_intr error");

	if (getrlimit(RLIMIT_FSIZE, &rl) == -1)
		err_sys("getrlimit error");
	msg("old: rlim_cur:%ld\trlim_max:%ld",
	    (long)rl.rlim_cur, (long)rl.rlim_max);
	rl.rlim_cur = (rlim_t)1024;
	/* rl.rlim_max = (rlim_t)1024; */
	msg("new: rlim_cur:%ld\trlim_max:%ld",
	    (long)rl.rlim_cur, (long)rl.rlim_max);

	while ((readn = read(STDIN_FILENO, buf, BUFFSIZE)) > 0) {
		if ((writen = write(STDOUT_FILENO, buf, readn)) != readn)
			err_msg("write error: only wrote %d of %d bytes\n",
				writen, readn);
	}
	if (readn < 0)
		err_sys("read error");
	return 0;
}

/* sig catcher */
void sig_xfsz(int signo)
{
	int err = errno;
	(void)write(STDERR_FILENO, SIG_MSG, sizeof(SIG_MSG)-1);
	errno = err;
}
