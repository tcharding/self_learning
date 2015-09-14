#include "apue.h"
#include <pthread.h>

void Pthread_create(pthread_t *thread, const pthread_attr_t *attr,
                          void *(*start_routine) (void *), void *arg)
{
	int err;

	err = pthread_create(thread, attr, start_routine, arg);
	if (err != 0)
		err_exit(err, "pthread_creat error");
}

void Pthread_join(pthread_t thread, void **retval)
{
	int err;

	err = pthread_join(thread, retval);
	if (err != 0)
		err_exit(err, "pthread_join error");
}

void Pthread_mutex_lock(pthread_mutex_t *lock)
{
	int err;

	err = pthread_mutex_lock(lock);
	if (err != 0)
		err_exit(err, "pthread_mutex_lock error");
}

void Pthread_mutex_unlock(pthread_mutex_t *lock)
{
	int err;

	err = pthread_mutex_unlock(lock);
	if (err != 0)
		err_exit(err, "pthread_mutex_unlock error");
}
