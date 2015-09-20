#ifndef PIPE_H
#define PIPE_H
#include <pthread.h>
#include <semaphore.h>

#define PIPE_BUF 255

typedef struct pipe {
	pthread_mutex_t p_lock;
	char p_data[PIPE_BUF];
	int p_count;		/* bytes in pipe */
	char *p_start;		/* first byte to read */
	int p_eof;		/* true if pipe closed for write */
	sem_t *p_rsem;		/* read semaphore */
	sem_t *p_wsem;		/* write semaphore */
} pipe_t;

pipe_t *pipe_open(void);
int pipe_read(pipe_t *p, char *buf, int nbytes);
int pipe_write(pipe_t *p, char *buf, int nbytes);
int pipe_close(pipe_t *p, int how);

#endif	/* PIPE_H */
