#ifndef HARD_TIMER_H
#define HARD_TIMER_H

int ht_init(Sigfunc *handler);
int ht_isblocked(void);
int ht_block(void);
void ht_unblock(void);

long ht_get(void);
void ht_set(struct timespec *tp);
void ht_stop(void);

void ht_wait(void);

#endif	/* HARD_TIMER_H */
