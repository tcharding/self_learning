#ifndef VIRT_TIMERS_H
#define VIRT_TIMERS_H

#define VERBOSE 1 		/* verbose test output */

int getevent(int eventnumber);
int getnumevents(void);
int getrunning(void);
long getvalue(int n);
int removetop(void);
int timerinit(void);
void timerstart(int n, long interval);
void timerstop(int n);
void waitforevent(void);
int checktimer(int n);
/* testing */
int vt_unit_tests(void);

#endif	/* VIRT_TIMERS_H */
