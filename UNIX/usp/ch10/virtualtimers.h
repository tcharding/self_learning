#ifndef VIRT_TIMERS_H
#define VIRT_TIMERS_H

#define VERBOSE 1 		/* verbose test output */
#define TRACEFLAG 1		/* used for debugging (show()) */

#define MAXTIMERS 5

typedef int Timer;
typedef int Event;


/* initialise */
int vt_init(void);

void waitforevent(void);

/* Manipulate Event */
int vt_getnumevents(void);
Timer vt_getevent(Event n);
Timer vt_rmhead(void);

/* Manipulate Timer */
void vt_start(Timer n, struct timespec *tp);
void vt_stop(Timer n);
/* int vt_check(Timer n); */
Timer vt_running(void);
struct timeval vt_value(Timer n);

/* testing */
int vt_unit_tests(void);

#endif	/* VIRT_TIMERS_H */
