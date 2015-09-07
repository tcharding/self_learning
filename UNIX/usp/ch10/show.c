#include "tch.h"
#include "virtualtimers.h"
#include "hardwaretimer.h"
#define MILLION 1000000L

static int maxtimers;
static struct timespec initial;

static int getrelativetime(void)
{
	int rel = 0;
	struct timespec tval;

	if (clock_gettime(CLOCK_REALTIME, &tval) < 0)
		err_sys("clock_gettimer");
	else {
		rel = tval.tv_sec - initial.tv_sec;
	}
	return rel;
}

/* showtimerdata: display the timers data structure */
static void showtimerdata(void)
{
	virtt_write_running();
	printf(" ");
	virtt_write_active();
	printf(" ");
	virtt_write_events();
}

/* ------------ Public Functions ------------ */

/*
 *
 * BUGGY: not async safe, should use write(2)
 *
 */ 
/* show: displays timers with message for evtype */
void show(int traceflag, const char *msg, long val1, long val2, int blockedflag)
{
	int wasblockedflag;

	if (!traceflag)
		return;
	wasblockedflag = ht_block();
	printf("RT %d: ",  getrelativetime());
	printf("%s ", msg);
	if (val1 >= 0)
		printf("%ld ", val1);
	if (val2 >= 0)
		printf("%ld ", val2);
	if (blockedflag)
		printf("B ");
	else
		printf("U ");
	if (blockedflag != wasblockedflag)
		printf("**** ");
	showtimerdata();
	printf("\n");
	fflush(stdout);
	if (!wasblockedflag)
		ht_unblock();
}

/* show_init: set initialtod to seconds since epoch */
void show_init(int maxt)
{
	if (clock_gettime(CLOCK_REALTIME, &initial) < 0)
		err_sys("clock_gettime");
	maxtimers = maxt;
}
