#ifndef VIRT_TIMERS_H
#define VIRT_TIMERS_H

#define VERBOSE 1 		/* verbose test output */
#define TRACEFLAG 1		/* used for debugging (show()) */

#define MAXTIMERS 5
#define OFF -1

typedef int Timer;
typedef int Event;

/* initialise */
int virtt_init(void);

void virtt_wait(void);

/* Manipulate Timer */
Timer virtt_start(struct timespec *tp);
void virtt_startt(Timer t, struct timespec *tp);
void virtt_stop(Timer t);
/* int virtt_check(Timer n); */
Timer virtt_running(void);
int virtt_value(Timer t);

/* Manipulate Event */
int virtt_getnumevents(void);
/* Timer virtt_getevent(Event n); */
Timer virtt_rmhead(void);

/* testing */
int virtt_unit_tests(void);
void virtt_write_running(void);
void virtt_write_active(void);
void virtt_write_events(void);

#endif	/* VIRT_TIMERS_H */
