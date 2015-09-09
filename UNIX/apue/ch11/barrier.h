#ifndef BARRIER_H
#define BARRIER_H
#define BARRIER_SERIAL_THREAD -1

typedef struct barrier {
	unsigned b_cnt;
	unsigned b_here;
	pthread_t *tids;
	pthread_mutex_t *lock;
} barrier_t;

void barrier_init(barrier_t *b, unsigned count);
int barrier_wait(barrier_t *b);
void barrier_destroy(barrier_t *b);

#endif	/* BARRIER_H */
