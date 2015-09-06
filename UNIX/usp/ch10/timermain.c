#include "tch.h"
#include "virtualtimers.h"
#include "hardwaretimer.h"

int main(void)
{
	int n, interval;
	
	if (timerinit() != 0)
		err_quit("timerinit errro");

	for ( ; ; ) {
		if (scanf("%d %d", &n, &interval) < 0)
			err_sys("scanf error");
		timerstart(n, interval);
		waitforevent();
		/* bug in spec (waitforevent has no return value) */
		fprintf(stderr, "Event recieved\n");
		
	}
}
