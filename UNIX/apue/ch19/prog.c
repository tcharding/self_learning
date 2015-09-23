#include "apue.h"

#define SLEEP 10
#define TERM_BUF "SIGTERM caught\n"

static volatile sig_atomic_t sigwinch_caught;

static void sig_term(int signo)
{
				/* msg and return */
	(void)write(STDOUT_FILENO, TERM_BUF, sizeof(TERM_BUF));
}
static void sig_winch(int signo)
{
				/* set flag and return */
	sigwinch_caught = 1;
}

/* at tr:  Advanced Programming in the UNIX Environment - Stevens and Rago */
static void pr_winsize(int fd)
{
	struct winsize	size;

	if (ioctl(fd, TIOCGWINSZ, (char *) &size) < 0)
		err_sys("TIOCGWINSZ error");
	printf("window size: %d rows, %d columns\n", size.ws_row, size.ws_col);
}

int main(void)
{
	char *sigwinch_msg = "SIGWINCH caught: ";
	
	if (signal(SIGTERM, sig_term) == SIG_ERR)
		err_sys("error installing sig catcher");
	if (signal(SIGWINCH, sig_winch) == SIG_ERR)
		err_sys("error installing sig catcher");

	/* fprintf(stdout, "prog write\n"); */
	for ( ; ; ) {
		sleep(SLEEP);
		if (sigwinch_caught == 1) {
			if (write(STDOUT_FILENO, sigwinch_msg,
				  strlen(sigwinch_msg)+1) < 0)
				err_sys("write error");
			pr_winsize(STDOUT_FILENO);
			sigwinch_caught = 0; /* clear flag */
		}
	}
	return 0;

}
