/* attribution: UNIX Systems Programming - Robbins and Robbins */
#include "tch.h"
#include <sys/times.h>
#define NUM 1024000

/* showtimes: output CPU usage */
static void showtimes(void)
{
	double ticks;
	struct tms tinfo;

	bzero(&tinfo, sizeof(struct tms));
	if ((ticks = (double) sysconf(_SC_CLK_TCK)) < 0)
		err_sys("failed to get clock ticks per second");
	else if (times(&tinfo) == (clock_t)-1)
		err_sys("times error");
	else {
		fprintf(stderr, "User time: %8.3f seconds\n",
			(double)tinfo.tms_utime/ticks);
		fprintf(stderr, "System time: %8.3f seconds\n",
			(double)tinfo.tms_stime/ticks);
		fprintf(stderr, "Children's user time: %8.3f seconds\n",
			(double)tinfo.tms_cutime/ticks);
		fprintf(stderr, "Children's system time: %8.3f seconds\n",
			(double)tinfo.tms_cstime/ticks);
	}
}

int main(void)
{
	volatile val;
	int i;
	
	if (atexit(showtimes) != 0)
		err_sys("failed to add exit handler");

	/* useless work */
	for (i = 0; i < NUM; i++)
		val = i*i*i*i*i;
	return 0;
}
