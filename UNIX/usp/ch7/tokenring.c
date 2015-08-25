#include "tch.h"
#include "tokenring.h"

/*
 * Token ring implementation using FIFO's
 */

/* tr_init: initialise ring, -1 on error with errno set */
int tr_init(struct ring *r)
{
	if (pipe(r->fd) == -1)
		return -1;
	if (((r->in = dup(r->fd[IN])) == -1) ||
	    ((r->out = dup(r->fd[OUT])) == -1))
		return -1;
	if (tr_close_fd(r) == -1)
		return -1;
	return 0;
}
/* tr_dup_in: duplicate read file descriptor, -1 on error with errno set */
int tr_dup_in(struct ring *r)
{
	if (dup2(r->fd[IN], r->in) == -1)
		return -1;
	return 0;
}
/* tr_dup_in: duplicate write file descriptor, -1 on error with errno set */
int tr_dup_out(struct ring *r)
{
	if (dup2(r->fd[OUT], r->out) == -1)
		return -1;
	return 0;
}
/* tr_close: close file descriptors, -1 on error with errno set */
int tr_close_fd(struct ring *r)
{
	if ((close(r->fd[IN]) == -1) ||
	    (close(r->fd[OUT]) == -1))
		return -1;
	return 0;
}
 
