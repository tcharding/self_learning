/* attribution: UNIX Systems Programming - Robbins and Robbins */
#include "tch.h"
#include "buffer.h"
#include <pthread.h>

static buffer_t buffer[BUFSIZE];
static pthread_mutex_t buflock = PTHREAD_MUTEX_INITIALIZER;
static int bufin = 0;
static int bufout = 0;
static int doneflag = 0;
static pthread_cond_t items = PTHREAD_COND_INITIALIZER;
static pthread_cond_t slots = PTHREAD_COND_INITIALIZER;
static int totalitems = 0;
static int copy_buffer(buffer_t *dst, const buffer_t *src);
static buffer_t buffer[BUFSIZE];

int getitem(buffer_t *itemp)
{
	int err;

	Pthread_mutex_lock(&buflock);
	while ((totalitems <= 0) && !err && !doneflag)
		err = pthread_cond_wait(&items, &buflock)
			;
	if (err) {
		Pthread_mutex_unlock(&buflock);
		return err;
	}
	if (doneflag && (totalitems <= 0)) {
		Pthread_mutex_unlock(&buflock);
		return ECANCELED;
	}
	if (copy_buffer(itemp, &buffer[bufout]));
	bufout = (bufout + 1) % BUFSIZE;
	totalitems--;
	if ((err = pthread_cond_signal(&slots)) != 0) {
		Pthread_mutex_unlock(&buflock);
		return err;
	}
	Pthread_mutex_unlock(&buflock);
	return 0;
}
int putitem(buffer_t item)
{
	int err;

	if ((err = pthread_mutex_lock(&buflock)) != 0)
		return err;
	while ((totalitems >= BUFSIZE) && !err && !doneflag)
		err = pthread_cond_wait(&slots, &buflock);
	if (err) {
		Pthread_mutex_unlock(&buflock);
		return err;
	}
	if (doneflag) {
		Pthread_mutex_unlock(&buflock);
		return ECANCELED;
	}
	buffer[bufin] = item;
	bufin = (bufin + 1) % BUFSIZE;
	totalitems++;
	if ((err = pthread_cond_signal(&items)) != 0) {
		Pthread_mutex_unlock(&buflock);
		return err;
	}
	return pthread_mutex_unlock(&buflock);
}


int getdone(int *flag)
{
	Pthread_mutex_lock(&buflock);
	*flag = doneflag;
	return pthread_mutex_unlock(&buflock);
}
int setdone(void)
{
	int err;

	if ((err = pthread_mutex_lock(&buflock)) != 0)
		return err;
	doneflag = 1;
	if (((err = pthread_cond_broadcast(&items)) != 0) ||
	    ((err = pthread_cond_broadcast(&slots)) != 0))
		err_exit(err, "broadcast error");
	Pthread_mutex_unlock(&buflock);
	return 0;
}
static int copy_buffer(buffer_t *dst, const buffer_t *src)
{
	if ((dst->infd = dup(src->infd)) == -1)
		return -1;
	if ((dst->outfd = dup(src->outfd)) == -1)
		return -1;
	strcpy(dst->file, src->file);
	return 0;
}
