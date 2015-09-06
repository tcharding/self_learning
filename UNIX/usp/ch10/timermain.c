#include "tch.h"
#include "virtualtimers.h"
#include "hardwaretimer.h"

int main(void)
{
	int nsec;
	struct timespec tspec;

	nsec = 0;
	bzero(&tspec, sizeof(struct timespec));
	if (virtt_init() != 0)
		err_quit("timerinit errro");

	for ( ; ; ) {
		if (scanf("%d", &nsec) < 0)
			err_sys("scanf error");
		tspec.tv_sec = nsec;
		(void)virtt_start(&tspec);
		virtt_wait();
		/* bug in spec (waitforevent has no return value) */
		fprintf(stderr, "Event recieved\n");
		
	}
}
