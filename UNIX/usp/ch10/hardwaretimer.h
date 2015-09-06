#ifndef HARD_TIMER_H
#define HARD_TIMER_H

int catchsetup(Sigfunc *handler);
int is_interuptblocked(void);
int blockinterrupt(void);
void unblockinterrupt(void);

struct timeval gethardwaretimer(void);
void sethardwaretimer(struct timeval interval);
void stophardwaretimer(void);

void waitforinterrupt(void);

#endif	/* HARD_TIMER_H */
