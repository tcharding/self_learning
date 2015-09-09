#ifndef PTHREADS_H
#define PTHREADS_H

/* prototypes for pthread wrapper functions (wrappthread.c) */
void Pthread_create(pthread_t *thread, const pthread_attr_t *attr,
		    void *(*start_routine) (void *), void *arg);
void Pthread_join(pthread_t thread, void **retval);
void Pthread_mutex_unlock(pthread_mutex_t *lock);
void Pthread_mutex_lock(pthread_mutex_t *lock);

#endif	/* PTHREADS_H */
