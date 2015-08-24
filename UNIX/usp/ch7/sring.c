#include "tch.h"
#include <stdarg.h>

enum { IN, OUT };

struct ring {
	int in;
	int out;
	int fd[2];
};

static void pprintt(pid_t tp[], int size);
static int msg_all(int in, int out, pid_t buf[], int nodes);
static void ring_init(struct ring *);
static void ring_dup_in(struct ring *);
static void ring_dup_out(struct ring *);
static void ring_close_fd(struct ring *);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nproc;
	struct ring r;
	pid_t pid;
	int i, status;
	pid_t *tp;

	status = nproc = pid = 0;
	if (argc != 2 || ((nproc = atoi(argv[1])) <= 0))
		err_quit("Uasge: %s processes", argv[0]);

	fprintf(stderr, "Creating ring of %d processes\n", nproc);
				/* create initial ring */
	ring_init(&r);
	
	for (i = 1; i < nproc; i++) {
		Pipe(r.fd);
		if ((pid = Fork()) > 0) {
			ring_dup_out(&r);
		} else {
			ring_dup_in(&r);
		}
		ring_close_fd(&r);
		if (pid > 0)
			break;
	}

	if ((tp = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	(void)msg_all(r.in, r.out, tp, nproc);
	
	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	pprintt(tp, nproc);
	free(tp);

	return 0;
}
/* msg_all: pass tokens around ring to all nodes
   write to out, read from in, store in buf */
static int msg_all(int in, int out, pid_t buf[], int nodes)
{
	pid_t id;
	int i;
	ssize_t res;
	
	id = getpid();
	buf[0] = id;
	for (i = 1; i < nodes; i++) {
		res = write(out, &id, sizeof(id));
		if ((res == -1) || ((size_t)res != sizeof(id)))
			err_sys("write error");
		res = read(in, &id, sizeof(id));
		if ((res == -1) || (size_t)res != sizeof(id))
			err_sys("read error");
		buf[i] = id;
	}
	return 0;
}
/* pprintt: print token buffer (ring messages) tp to stderr */
static void pprintt(pid_t tp[], int size)
{
	int i;

	fprintf(stderr, "[ ");
	for (i = 0; i < size; i++)
		fprintf(stderr, "%ld ", (long)tp[i]);
	fprintf(stderr, "]\n");
}
/* ring_init: initialise ring */
static void ring_init(struct ring *r)
{
	Pipe(r->fd);
	r->in = dup(r->fd[IN]);
	r->out = dup(r->fd[OUT]);
	ring_close_fd(r);
}
/* ring_dup_in: duplicate read file descriptor */
static void ring_dup_in(struct ring *r)
{
	Dup2(r->fd[IN], r->in);
}
/* ring_dup_in: duplicate write file descriptor */
static void ring_dup_out(struct ring *r)
{
	Dup2(r->fd[OUT], r->out);
}
/* ring_close: close file descriptors */
static void ring_close_fd(struct ring *r)
{
	Close(r->fd[IN]);
	Close(r->fd[OUT]);
}
