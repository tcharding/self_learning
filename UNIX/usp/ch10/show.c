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
		fprintf(stderr, "gettimeofday error");
	else {
		rel = tval.tv_sec - initial.tv_sec;
	}
	return rel;
}

/* showtimerdata: display the timers data structure */
static void showtimerdata(void)
{
	int i;
	Timer t;
	Event ev;

	t = virtt_running();
	if (t == OFF) {
		printf("(-:-)");
	} else {
		printf("(%d:%d)", t, virtt_value(t));
	}
       
	for (t = 0; t < maxtimers; t++) 
		virtt_write(t);
	ev = virtt_getnumevents();
	printf(" (%dE", ev);
	for (i = 0; i < ev; i++)
		printf(" %d", virtt_getevent(i));
	printf(")\n");
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
	printf("**** %d: ",  getrelativetime());
	printf("%s ", msg);
	if (val1 >= 0)
		printf("%ld ", val1);
	if (val2 >= 0)
		printf("%ld ", val2);
	if (blockedflag)
		printf("B");
	else
		printf("U");
	if (blockedflag != wasblockedflag)
		printf(" ****");
	showtimerdata();
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
