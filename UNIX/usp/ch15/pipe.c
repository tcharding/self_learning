#include "tch.h"
#include "pipe.h"
#include <fcntl.h>
#include <pthread.h>
#include <semaphore.h>
#include <sys/mman.h>

#define SIZE sizeof(pipe_t)	/* size of shared memory area */
#define PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

enum { FALSE, TRUE };

char *rsem_name = "/pipe_rsem";
char *wsem_name = "/pipe_wsem";

static void pipe_clean(void);

/* create pipe */
pipe_t *pipe_open(void)
{
	int fd, err;
	void *area;
	pipe_t *p;

	pipe_clean();		/* clean up from previous run */
	if ((fd = open("/dev/zero", O_RDWR)) < 0)
		err_sys("open error");
	if ((area = mmap(0, SIZE, PROT_READ | PROT_WRITE, MAP_SHARED,
			 fd, 0)) == MAP_FAILED)
		err_sys("mmap error");
	close(fd);		/* can close /dev/zero now it is mapped */

	p = (pipe_t *)area;
	err = pthread_mutex_init(&p->p_lock, NULL);
	if (err != 0)
		err_sys("pthread_mutex_init error");
	p->p_eof = FALSE;
	p->p_rsem = sem_open(rsem_name, O_CREAT | O_EXCL, PERMS, 0);
	p->p_wsem = sem_open(wsem_name, O_CREAT | O_EXCL, PERMS, 0);
	if ((p->p_rsem == SEM_FAILED) ||
	    (p->p_wsem == SEM_FAILED))
		err_sys("sem_open error");
	sem_post(p->p_wsem);	/* pipe ready to write */
	return p;
}

/* read upto nbytes from p into buf */
int pipe_read(pipe_t *p, char *buf, int nbytes)
{
	int nread;
	
	if (sem_wait(p->p_rsem) == -1)
		return -1;
	
	pthread_mutex_lock(&p->p_lock);
	if (p->p_eof == TRUE) {	/* pipe closed for write */
		pthread_mutex_unlock(&p->p_lock);
		return 0;
	}

	if (nbytes < p->p_count) { /* partial read */
		strncpy(buf, p->p_start, nbytes);	
		p->p_count -= nbytes;
		p->p_start += nbytes;
		pthread_mutex_unlock(&p->p_lock);
		return nbytes;
	}
				/* read all data */
	nread = p->p_count;
	strncpy(buf, p->p_start, nread);
	p->p_count = 0;
	pthread_mutex_unlock(&p->p_lock);
	sem_post(p->p_wsem);
	return nread;	
}

/* write to pipe created with pipe_open */
int pipe_write(pipe_t *p, char *buf, int total)
{
	int nbytes;

	while (total > 0) {
		if (sem_wait(p->p_wsem) == -1)
			return -1;
		if (pthread_mutex_lock(&p->p_lock) != 0)
			return -1;
		nbytes = (total < PIPE_BUF) ? total : PIPE_BUF;
		strncpy(p->p_data, buf, nbytes);
		p->p_count = nbytes;
		p->p_start = &p->p_data[0];
		total -= nbytes;
		if (pthread_mutex_unlock(&p->p_lock) != 0)
			return -1;
		if (sem_post(p->p_rsem) == -1)
			return -1;
	}
	return 0;
}

/* close pipe created with pipe_open */
int pipe_close(pipe_t *p, int how)
{
	if (how & O_WRONLY) {
		if (pthread_mutex_lock(&p->p_lock) != 0)
			return -1;
		p->p_eof = TRUE;
		if (pthread_mutex_unlock(&p->p_lock) != 0)
			return -1;
	} else {
		if (pthread_mutex_lock(&p->p_lock) != 0)
			return -1;
		if (pthread_mutex_destroy(&p->p_lock) != 0)
			err_msg("failed to destroy mutex");
		sem_close(p->p_rsem);
		sem_close(p->p_wsem);
		free(p);
		pipe_clean();
	}
	return 0;
}

/* unlink all semaphores */
static void pipe_clean(void)
{
	(void)sem_unlink(rsem_name);
	(void)sem_unlink(wsem_name);
}
