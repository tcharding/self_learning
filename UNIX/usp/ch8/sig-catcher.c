#include "tch.h"
#include <sys/times.h>
#define SIGBUF 255

static void writed(double d);
static void sig_quit(int signo);
double ticks;

/* catch Control-C */
int main(void)
{
	if (signal(SIGQUIT, sig_quit) == SIG_ERR)
		err_sys("failed to add sig catcher");

	if ((ticks = (double) sysconf(_SC_CLK_TCK)) < 0)
		err_sys("failed to get clock ticks per second");

	for ( ; ; )
		(void)pause();
}

static void sig_quit(int signo)
{
	int err;
	struct tms tinfo;
	char *ut = "User time: ";
	char *st = "System time: ";
	int cnt;
	char *ptr;
	
	cnt = 0;
	err = errno;
	bzero(&tinfo, sizeof(struct tms));

	if (times(&tinfo) == (clock_t)-1)
		err_sys("times error");

	ptr = ut;
	while (*ptr++ != '\0')
		cnt++;
	write(STDERR_FILENO, ut, cnt);
	writed((double)tinfo.tms_utime/ticks);
	ptr = st;
	while (*ptr++ != '\0')
		cnt++;
	write(STDERR_FILENO, st, cnt);
	writed((double)tinfo.tms_stime/ticks);

	/* 		fprintf(stderr, "User time: %8.3f seconds\n", */
	/* 		(double)tinfo.tms_utime/ticks); */
	/* 	fprintf(stderr, "System time: %8.3f seconds\n", */
	/* 		(double)tinfo.tms_stime/ticks); */
	/* 	fprintf(stderr, "Children's user time: %8.3f seconds\n", */
	/* 		(double)tinfo.tms_cutime/ticks); */
	/* 	fprintf(stderr, "Children's system time: %8.3f seconds\n", */
	/* 		(double)tinfo.tms_cstime/ticks); */
	/* } */

	errno = err;
}

/* writed: write d to 3 decimal places to stderr */
static void writed(double d)
{
				/* TODO: implement using re-entrant functions */
	fprintf(stderr, "%.3f", d);
}
