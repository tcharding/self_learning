#include "tch.h"
#include "virtualtimers.h"
#include "hardwaretimer.h"
#define MILLION 1000000L

static double initialtod = 0.0;
static int maxtimers;
static double gettime(void);
static double timetodouble(long interval);

static double getrelativetime(void)
{
	return gettime() - initialtod;
}

static double gettime(void)
{
	double thistime = 0.0;
	struct timeval tval;

	if (gettimeofday(&tval, NULL))
		fprintf(stderr, "gettimeofday error");
	else
		thistime = tval.tv_sec + (double)tval.tv_usec / MILLION;
	return thistime;
}

/* showtimerdata: display the timers data structure */
static void showtimerdata(void)
{
	int i;

	printf("(%d,%.3f) A:", getrunning(),
	       timetodouble(getvalue(getrunning())));
	for (i = 0; i < maxtimers; i++)
		if (getvalue(i) >= 0)
			printf("(%d,%.3f) ",
			       i, timetodouble(getvalue(i)));
	printf(" (%dE", getnumevents());
	for (i = 0; i < getnumevents(); i++)
		printf(" %d", getevent(i));
	printf(")\n");
}

/* timetodouble: microseconds to seconds */
static double timetodouble(long interval)
{
	return (double)interval / MILLION;
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
	wasblockedflag = blockinterrupt();
	printf("**** %8.4f: ",  getrelativetime());
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
		printf("****");
	showtimerdata();
	fflush(stdout);
	if (!wasblockedflag)
		unblockinterrupt();
}

/* showinit: set initialtod to seconds since epoch */
void showinit(int maxt)
{
	initialtod = gettime();
	maxtimers = maxt;
}
