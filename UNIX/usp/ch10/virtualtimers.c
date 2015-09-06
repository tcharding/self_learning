#include "tch.h"
#include "virtualtimers.h"
#include "hardwaretimer.h"
#include "show.h"
#include "helper.h"

struct timerdata {
	struct timespec active[MAXTIMERS]; /* active timers */
	Timer events [MAXTIMERS];	/* que of expired timers */
	int numevents;			/* number of entries in events */
	int running;			/* next active timer to expire */
};
/* Virtual Timers */
static struct timerdata timers;	

enum { UNSET, SET };		/* timer set or not set (i.e active/non-active) */

/* static function prototypes */
static void virtt_sig_alrm(int signo);	/* called externally by callback */

/* static void start_next_running(void); */
static int add_event(Timer t);
static int rm_event(Timer t);
static void clear_events(void);
static void clear_timer(Timer t);
static int timer_state(Timer t);

/* test helpers */
static void dump_events();
/* static void dump_active(); */
/* unit tests */
static void t_addrm_event();

/* virtt_init: initialise Timers to zero */
int virtt_init(void)
{
	int i;

	for (i = 0; i < MAXTIMERS; i++) {
		clear_timer(i);
	}
	timers.running = OFF;
	clear_events();
	ht_init(virtt_sig_alrm); /* init hardware sig catcher */
	show_init(MAXTIMERS);	 /* init show library */

	return 0;
}
/* virtt_wait: */
void virtt_wait(void)
{
	ht_wait();
	return;
}
/* virtt_start: set next available timer to expire after interval tp 
    return Timer number set, -1 if none available */
Timer virtt_start(struct timespec *tp)
{
	Timer t;
	
	for (t = 0; t < MAXTIMERS; t++) {
		if (timer_state(t) == UNSET) {
			virtt_startt(t, tp);
			return t;
		}
	}
	return -1;		/* no free timer */
}
/* virtt_start: set t to expire after interval tp */
void virtt_startt(Timer t, struct timespec *tp)
{
	/* 
	 * currently only supports one timer
	 */
	if (t < 0 || t >= MAXTIMERS) 
		return;
	show(TRACEFLAG, "virtt_start Enter", t, tp->tv_sec, 0);
	(void)rm_event(t);
	timers.running = t;
	timers.active[t].tv_sec = tp->tv_sec;
	timers.active[t].tv_nsec = tp->tv_nsec;
	ht_set(tp);
	show(TRACEFLAG, "virtt_start Exit", t, tp->tv_sec, 0);
}
/* virtt_stop: stop t if running, remove event if present */
void virtt_stop(Timer t)
{
	if (t < 0 || t >= MAXTIMERS) 
		return;
	
	if (timers.running == t) 
		timers.running = OFF;
	clear_timer(t);
	(void)rm_event(t);
}

/* /\* virtt_check: check to see if a t exists */
/*     1 if active */
/*     0 if expired */
/*     -1 if neither (timer was not set) */
/*     -1 and errno on error */
/* *\/ */
/* int virtt_check(Timer n) */
/* { */
/* 	int i; */
	
/* 	if (n < 0 || n > MAXTIMERS) { */
/* 		errno = EINVAL; */
/* 		return -1; */
/* 	} */
	
/* 	if (timers.active[n] != OFF) */
/* 		return 1; */
/* 	for (i = 0; i < timers.numevents; i++) { */
/* 		if (timers.events[i] == n) */
/* 			return 0; /\* timer event present *\/ */
/* 	} */
/* 	return -1;		/\* neither active nor expired *\/ */
/* } */

/* virtt_running: get the current running timer, -1 if none running */
Timer virtt_running(void)
{
	return timers.running;
}

/* virtt_value: return seconds till expiry of t if active, -1 if not */
int virtt_value(Timer t)
{
	if (t < 0 || t >= MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}
	return timers.active[t].tv_sec;
}
/* virtt_getnumevents: return number of expired timers (numevents) */
int virtt_getnumevents(void)
{
	return timers.numevents;
}

/* virtt_getevent: return Timer for event n*/
Timer virtt_getevent(Event n)
{
				/* only access events that have occurred */
	if (n < 0 || n >= timers.numevents) {
		errno = EINVAL;
		return -1;
	}
	return timers.events[n];
}

/* virtt_rmhead: remove the 'oldest' event (Timer) from events
    return timer number or -1 if no events available */
Timer virtt_rmhead(void)
{
	Timer t;			
	
	if (timers.numevents == 0)
		return -1;
	t = timers.events[0];
	if (rm_event(t) != 0)
		err_quit("virtt_rmhead: unexplained error"); /* shouldn't get here */
	return t;
}

/* -------- static functions ----------- */

/* timer_state: return ON if timer is active, OFF if not active */
static int timer_state(Timer t)
{
	struct timespec *tp;

	if (t < 0 || t >= MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}
	tp = &timers.active[t];
	if (tp->tv_sec == 0 && tp->tv_nsec == 0)
		return UNSET;
	return SET;
}
/* clear_timer: deactivate timer t */
static void clear_timer(Timer t)
{
	struct timespec *tp;

	if (t < 0 || t >= MAXTIMERS) {
		errno = EINVAL;
		return;
	}
	tp = &timers.active[t];
	tp->tv_sec = 0;
	tp->tv_nsec = 0;
}

/* virtt_sig_alrm: handle signal, called externally via callback */
static void virtt_sig_alrm(int signo)
{
	Timer t;
/*
 * ? Do we need sig_atomic_t vars in timerdata_t ?
 */
	(void)signo;		/* quiet lint */
	show(TRACEFLAG, "virtt_sig_alrm Enter", timers.running, -1, 1);
	if (timers.running == OFF)
		return;		/* alarm signal not raised by us */
	t = timers.running;
	timers.running = OFF;
	add_event(t);
	clear_timer(t);
	/* start_next_running(); */
	show(TRACEFLAG, "virtt_sig_alrm Enter", timers.running, -1, 1);
}

/* /\* start_next_running: move the next active timer to running *\/ */
/* static void start_next_running() */
/* { */
/* 	int next;		/\* next candidate timer to run *\/ */
/* 	int i; */
/* 	long min;		 */

/* 	next = -1; */
/* 				/\* get first active *\/ */
/* 	for (i = 0; i < MAXTIMERS && timers.active[i] != -1; i++) { */
/* 		next = i; */
/* 		min = timers.active[i]; */
/* 		break; */
/* 	} */
/* 	if (next != -1) {	/\* get next to expire *\/ */
/* 		for (i = 0; i < MAXTIMERS && timers.active[i] != -1; i++) { */
/* 			if (timers.active[i] < min) { */
/* 				min = timers.active[i]; */
/* 				next = i; */
/* 			} */
/* 		} */
/* 	} */
/* 	timers.running = next;		/\* -1 if none running *\/ */
/* } */

/* add_event: add to end of events[] update numevents
    return new numevents */
static int add_event(Timer t)
{
	if (t < 0 || t >= MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}

	if (rm_event(t) >= 0)
		--timers.numevents;
	timers.events[timers.numevents] = t;
	return timers.numevents;
}
/* rm_event: remove t from events, update numevents
    return Event number or -1 if not present */
static int rm_event(Timer t)
{
	int i, j;
				/* find event */
	for (i = 0; i < timers.numevents; i++) {
		if (timers.events[i] == t) {
			for (j = i; j < timers.numevents-1; j++) { 
				timers.events[j] = timers.events[j+1];
			}
			timers.events[j] = OFF;
			return (Event)i;
		}
	}
	return -1;		/* event not present */
}
/* clear_events: remove all events */
static void clear_events(void)
{
	int i;
	
	timers.numevents = 0;
	for (i = 0; i < MAXTIMERS; i++)
		timers.events[i] = OFF;
}

/* --------- unit tests ---------- */

/* virtt_unit_tests: exported for test runner */
int virtt_unit_tests(void)
{
	int failed = 0;
	
	if (VERBOSE) {
		t_addrm_event();
	}
	return failed;
}
static void t_addrm_event()
{
				/* test rm_event first */
	clear_events();
	T_EQ(add_event(2), 1);
	T_EQ(add_event(4), 2);
	T_EQ(add_event(3), 3);
	T_EQ(add_event(0), 4);
	T_EQ(add_event(1), 5);
	msgn("------  [ 2 4 3 0 1 ]");
	dump_events();
	T_EQ(rm_event(3), 4);
	msgn("------  [ 2 4 0 1 . ]");
	dump_events();
	T_EQ(rm_event(2), 3);
	msgn("------  [ 4 0 1 . . ]");
	dump_events();
	T_EQ(rm_event(1), 2);
	msgn("------  [ 4 0 . . . ]");
	dump_events();
				/* now test add_event edge cases */
	clear_events();
	T_EQ(add_event(2), 1);
	T_EQ(add_event(2), 1);
	T_EQ(add_event(4), 2);
	msgn("Expect: [ 2 4 ... ]");
	dump_events();
	T_EQ(add_event(3), 3);
	T_EQ(add_event(2), 3);
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

/* /\* dump active array *\/ */
/* static void dump_active() */
/* { */
/* 	int i; */
	
/* 	fprintf(stderr, "Active: ["); */
/* 	for (i = 0; i < MAXTIMERS; i++) { */
/* 		fprintf(stderr, " %ld", timers.active[i]); */
/* 	} */
/* 	fprintf(stderr, " ]\n");	 */
/* } */


void virtt_write(Timer t)
{
	
	fprintf(stderr, "(T%d ", t);
	write_tspec(&timers.active[t]);
	fprintf(stderr,")");
}
