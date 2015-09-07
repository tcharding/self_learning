#include "tch.h"
#include "virtualtimers.h"
#include "hardwaretimer.h"
#include "show.h"
#define MILLION 1000000L
enum {FALSE, TRUE};		/* 0 = FALSE, 1 = TRUE */

struct timerdata {
	struct timespec active[MAXTIMERS]; /* active timers */
	Timer events[MAXTIMERS];	/* que of expired timers */
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
static void update_active(Timer t, struct timespec *tp);
static long tspec_to_micro(struct timespec *tp);
static void micro_to_tspec(struct timespec *tp, long ms);

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
/* virtt_start: set t to expire after interval 'new' */
void virtt_startt(Timer t, struct timespec *new)
{
	long rem, req, total, start;
	struct timespec tmp;

	if (t < 0 || t >= MAXTIMERS) 
		return;
	show(TRACEFLAG, "virtt_start BEGIN (t, sec)", t, new->tv_sec, FALSE);
	(void)rm_event(t);	      /* no active t in events, by definition */
	if ((timers.running = OFF) || /* no active timers */
	    (timers.running = t)) {   /* t is already running */
		timers.running = t;
		update_active(t, new); /* copy values from new to t */
		ht_set(new);
		return;
	} 
	rem = ht_get();	/* time remaining in microseconds */
	req = tspec_to_micro(new);
	if (rem < req) {	/* new timer expires after running timer */
		start = tspec_to_micro(&timers.active[timers.running]) - rem;
		total = start + tspec_to_micro(new);
		micro_to_tspec(&tmp, total);
		update_active(t, &tmp);
	} else {		/* new timer expires before running timer */
		total = rem - tspec_to_micro(new);
		micro_to_tspec(&tmp, total);
		update_active(timers.running, &tmp);
		timers.running = t;
		update_active(t, new);
		ht_set(new);
	}
	
	show(TRACEFLAG, "virtt_start END (t, sec)", t, new->tv_sec, FALSE);
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

/* /\* virtt_getevent: return Timer for event n*\/ */
/* Timer virtt_getevent(Event n) */
/* { */
/* 				/\* only access events that have occurred *\/ */
/* 	if (n < 0 || n >= timers.numevents) { */
/* 		errno = EINVAL; */
/* 		return -1; */
/* 	} */
/* 	return timers.events[n]; */
/* } */

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
/* 
 * Output functions, all write to stdout
 */ 
static void write_tspec(struct timespec *tp)
{
	printf("[%d:%d]", (int)tp->tv_sec, (int)tp->tv_nsec);
}
static void write_timer(Timer t)
{
	printf("(%d", t);
	write_tspec(&timers.active[t]);
	printf(")");
}
void virtt_write_active(void)
{
	Timer t;

	printf("A( ");
	for (t = 0; t < MAXTIMERS; t++)
		if (timer_state(t) == SET)
			write_timer(t);
	printf(")");
}
void virtt_write_events(void)
{
	int nev, i;

	nev = virtt_getnumevents();
	printf(" %dE(", nev);
	for (i = 0; i < nev; i++)
		printf(" %d", timers.events[i]);
	printf(")");
}
void virtt_write_running(void)
{
	Timer t = timers.running;
	
	printf("R");
	if (t == OFF)
		printf("[-:-]");
	else
		write_timer(t);
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

	if (t < 0 || t >= MAXTIMERS) {
		errno = EINVAL;
		return;
	}
	update_active(t, NULL);
}

/* update_active: set timespec of t to tp */
static void update_active(Timer t, struct timespec *new)
{
	struct timespec *old;
	
	if (t < 0 || t >= MAXTIMERS) {
		errno = EINVAL;
		return;
	}
	old = &timers.active[t];
	if (new == NULL) {	/* clear timer */
		old->tv_sec = 0;
		old->tv_nsec = 0;
	} else {
		old->tv_sec = new->tv_sec;
		old->tv_nsec = new->tv_nsec;
	}
}
/* virtt_sig_alrm: handle signal, called externally via callback */
static void virtt_sig_alrm(int signo)
{
	Timer t;
/*
 * ? Do we need sig_atomic_t vars in timerdata_t ?
 */
	(void)signo;		/* quiet lint */
	show(TRACEFLAG, "virtt_sig_alrm BEGIN (running, 0)",
	     timers.running, 0, TRUE);
	if (timers.running == OFF)
		return;		/* alarm signal not raised by us */
	t = timers.running;
	timers.running = OFF;
	add_event(t);
	clear_timer(t);
	/* start_next_running(); */
	show(TRACEFLAG, "virtt_sig_alrm END (running, numevents)",
	     timers.running, timers.numevents, TRUE);
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
	timers.events[timers.numevents++] = t;
	return timers.numevents;
}
/* rm_event: remove t from events, update numevents
    return Event number or -1 if not present */
static int rm_event(Timer t)
{
	int i, j;

	if (t < 0 || t >= MAXTIMERS) {
		errno = EINVAL;
		return -1;
	}
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

/* tspec_to_micro: convert timespec to microseconds
   looses resolution */
static long tspec_to_micro(struct timespec *tp)
{
	long retval;

	retval = tp->tv_sec * MILLION;
	retval += tp->tv_nsec / 1000; /* lossy */
	return retval;
}

/* micro_to_tspec: convect microseconds to timespec */
static void micro_to_tspec(struct timespec *tp, long ms)
{
	tp->tv_sec = ms / MILLION;
	tp->tv_nsec = (ms % MILLION) * 1000;
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
	printf("-- [ 2 4 3 0 1 ]\n");
	virtt_write_events();
	printf("\n");
	T_EQ(rm_event(3), 2);	/* event number 2 */
	T_EQ(rm_event(3), -1);
	printf("-- [ 2 4 0 1 . ]\n");
	virtt_write_events();
	printf("\n");
	T_EQ(rm_event(2), 0);
	printf("-- [ 4 0 1 . . ]\n");
	virtt_write_events();
	printf("\n");
	T_EQ(rm_event(1), 2);
	printf("-- [ 4 0 . . . ]\n");
	virtt_write_events();
	printf("\n");
	
				/* now test add_event edge cases */
	clear_events();
	T_EQ(add_event(2), 1);
	T_EQ(add_event(2), 1);
	T_EQ(add_event(4), 2);
	printf("-- [ 2 4 ... ]\n");
	virtt_write_events();
	printf("\n");
	T_EQ(add_event(3), 3);
	T_EQ(add_event(2), 3);
	printf("-- [ 4 3 2 ... ]\n");
	virtt_write_events();
	printf("\n");
	clear_events();
}

