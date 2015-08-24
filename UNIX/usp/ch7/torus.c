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
	struct ring n, s, e, w;	/* north, south, east, west 'connections' */
	pid_t pid;
	int i, status;
	pid_t *ntp, *stp, *etp, *wtp;

	bzero(&n, sizeof(struct ring));
	bzero(&s, sizeof(struct ring));
	bzero(&e, sizeof(struct ring));
	bzero(&w, sizeof(struct ring));
	status = nproc = pid = 0;
	if (argc != 2 || ((nproc = atoi(argv[1])) <= 0))
		err_quit("Uasge: %s processes", argv[0]);

	fprintf(stderr, "Creating ring of %d processes\n", nproc);
	ring_init(&n);
	ring_init(&s);
	ring_init(&e);
	ring_init(&w);
	
	for (i = 1; i < nproc; i++) {
		Pipe(n.fd);
		Pipe(s.fd);
		Pipe(e.fd);
		Pipe(w.fd);
		if ((pid = Fork()) > 0) {
			ring_dup_out(&n);
			ring_dup_in(&s);
			ring_dup_out(&e);
			ring_dup_in(&w);
		} else {
			ring_dup_in(&n);
			ring_dup_out(&s);
			ring_dup_in(&e);
			ring_dup_out(&w);

		}
		ring_close_fd(&n);
		ring_close_fd(&s);
		ring_close_fd(&e);
		ring_close_fd(&w);
		if (pid > 0)
			break;
	}

	if ((ntp = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	(void)msg_all(n.in, n.out, ntp, nproc);

	if ((stp = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	(void)msg_all(s.in, s.out, stp, nproc);

	if ((etp = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	(void)msg_all(e.in, e.out, etp, nproc);

	if ((wtp = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	(void)msg_all(w.in, w.out, wtp, nproc);
	
	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	pprintt(ntp, nproc);
	pprintt(stp, nproc);
	pprintt(etp, nproc);
	pprintt(wtp, nproc);
	free(ntp);
	free(stp);
	free(etp);
	free(wtp);
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
