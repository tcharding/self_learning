#include "tch.h"
#include "virtualtimers.h"
#define MAXTIMERS 5
#define OFF -1			/* timers are OFF when not running */
#define EMPTY -1		/* event slots are EMPTY when not in use */

typedef struct timerdata {
	long active[MAXTIMERS];	/* expiration time of running timers */
	int events [MAXTIMERS];	/* que of expired timer numbers */
	int numevents;		/* num entries in events */
	int running;		/* next active timer to expire */
} timerdata_t;

/*
 * Virtual Timers
 */
static timerdata_t timers;	

/* static function prototypes */
static void timerhandler(void);	/* called externally by callback */
static void start_next_running();
static int addevent(int n);
static int rmevent(int n);
static void clear_events();

/* unit tests */
static void t_addrm_event();
static void dump_events();
static void dump_active();

/* getevent: return timer number of event 'eventnum' */
int getevent(int eventnum)
{
	if (eventnum < 0 || eventnum >= MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}
	return timers.events[eventnum];
}

/* getnumevents: return number of expired timers */
int getnumevents(void)
{
	return timers.numevents;
}

/* getrunning: get the current running timer, -1 if none running */
int getrunning(void)
{
	return timers.running;
}

/* getvalue: return current value of timer n if active, -1 if not */
long getvalue(int n)
{
	if (n < 0 || n >= MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}
	return timers.active[n];
}
/* removetop: remove the top event (timer) from events
    return timer number or -1 if no events available */
int removetop(void)
{
	int t;			/* timer number */
	
	if (timers.numevents == 0)
		return -1;
	t = timers.events[0];
	rmevent(t);
	return t;
}
/* timerinit: initialise timers to zero */
int timerinit(void)
{
	int i;

	for (i = 0; i < MAXTIMERS; i++) {
		timers.active[i] = -1;
	}
	timers.running = -1;
	clear_events();

	/*
	  catchsetup()

	  showinit()
	*/
	return 0;
}
/* timerstart: set timer 'n' to expire in 'interval' microseconds */
void timerstart(int n, long interval)
{
	if (n < 0 || n >= MAXTIMERS) 
		return;

	timers.running = n;
	timers.active[n] = interval;
	/* sethardwaretimer() */
}
/* timerstop: stop timer 'n' if running, remove event if present */
void timerstop(int n)
{
	if (n < 0 || n >= MAXTIMERS) 
		return;
	if (timers.running == n) {
		timers.active[n] = OFF;
		start_next_running();	/* TODO */
	}
	(void)rmevent(n);
}

/* waitforevent: */
void waitforevent(void)
{
	/* waitforinterupt() */
	return;
}

/* checktimer: check to see if a timer is running
    1 if active
    0 if expired
    -1 if neither (timer was not set)
    -1 and errno on error
*/
int checktimer(int n)
{
	int i;
	
	if (n < 0 || n > MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}
	
	if (timers.active[n] != OFF)
		return 1;
	for (i = 0; i < timers.numevents; i++) {
		if (timers.events[i] == n)
			return 0; /* timer event present */
	}
	return -1;		/* neither active nor expired */
}
/* -------- static functions ----------- */

/* timerhandler: handle signal, called externally via callback */
static void timerhandler(void)
{
	int t;

	t = timers.running;
	addevent(t);
	timers.active[t] = OFF;
	start_next_running();	/* TODO */
				/* move next timer to running? */
}

/* start_next_running: move the next active timer to running */
static void start_next_running()
{
	int next;		/* next candidate timer to run */
	int i;
	long min;		

	next = -1;
				/* get first active */
	for (i = 0; i < MAXTIMERS && timers.active[i] != -1; i++) {
		next = i;
		min = timers.active[i];
		break;
	}
	if (next != -1) {	/* get next to expire */
		for (i = 0; i < MAXTIMERS && timers.active[i] != -1; i++) {
			if (timers.active[i] < min) {
				min = timers.active[i];
				next = i;
			}
		}
	}
	timers.running = next;		/* -1 if none running */
}
/* addevent: add to end of events[] update numevents
    return new numevents */
static int addevent(int n)
{
	if (n < 0 || n >= MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}
	(void)rmevent(n);
	timers.events[timers.numevents++] = n;
	return timers.numevents;
}

/* rmevent: remove event from array, update numevents
    return new numevents */
static int rmevent(int n)
{
	int i, j;

				/* find event */
	for (i = 0; i < timers.numevents; i++) {
		if (timers.events[i] == n) {
			for (j = i; j < timers.numevents-1; j++) { 
				timers.events[j] = timers.events[j+1];
			}
			timers.events[j] = -1;
			return --timers.numevents;
		}
	}
	return timers.numevents; /* event not present */
}

/* --------- unit tests ---------- */

/* vt_unit_tests: exported for test runner */
int vt_unit_tests(void)
{
	int failed = 0;
	
	if (VERBOSE) {
		t_addrm_event();
	}
	return failed;
}
static void t_addrm_event()
{
				/* test rmevent first */
	clear_events();
	T_EQ(addevent(2), 1);
	T_EQ(addevent(4), 2);
	T_EQ(addevent(3), 3);
	T_EQ(addevent(0), 4);
	T_EQ(addevent(1), 5);
	msgn("------  [ 2 4 3 0 1 ]");
	dump_events();
	T_EQ(rmevent(3), 4);
	msgn("------  [ 2 4 0 1 . ]");
	dump_events();
	T_EQ(rmevent(2), 3);
	msgn("------  [ 4 0 1 . . ]");
	dump_events();
	T_EQ(rmevent(1), 2);
	msgn("------  [ 4 0 . . . ]");
	dump_events();
				/* now test addevent edge cases */
	clear_events();
	T_EQ(addevent(2), 1);
	T_EQ(addevent(2), 1);
	T_EQ(addevent(4), 2);
	msgn("Expect: [ 2 4 ... ]");
	dump_events();
	T_EQ(addevent(3), 3);
	T_EQ(addevent(2), 3);
	msgn("Expect: [ 4 3 2 ... ]");
	dump_events();
	clear_events();
}

/* dump events array */
static void dump_events()
{
	int i;
	
	fprintf(stderr, "Events: [");
	for (i = 0; i < MAXTIMERS; i++) {
		fprintf(stderr, " %d", timers.events[i]);
	}
	fprintf(stderr, " ]\n");
}
/* dump active array */
static void dump_active()
{
	int i;
	
	fprintf(stderr, "Active: [");
	for (i = 0; i < MAXTIMERS; i++) {
		fprintf(stderr, " %ld", timers.active[i]);
	}
	fprintf(stderr, " ]\n");	
}
/* clear_events: remove all events */
static void clear_events()
{
	int i;
	
	timers.numevents = 0;
	for (i = 0; i < MAXTIMERS; i++)
		timers.events[i] = -1;
	
}
