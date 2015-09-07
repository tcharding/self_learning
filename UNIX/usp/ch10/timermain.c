#include "tch.h"
#include "virtualtimers.h"
#include "hardwaretimer.h"
#include "show.h"

int main(void)
{
	int nsec;
	Timer t;
	struct timespec tspec;

	nsec = 0;
	bzero(&tspec, sizeof(struct timespec));
	if (virtt_init() != 0)
		err_quit("timerinit errro");

	for ( ; ; ) {
		while (scanf("%d %d", &t, &nsec) != EOF) {
			tspec.tv_sec = nsec;
			(void)virtt_startt(t, &tspec);
			virtt_wait();
		}
	}
}
