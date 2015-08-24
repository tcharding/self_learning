#include "tch.h"
#include <stdarg.h>

enum { IN, OUT };

static void pprintt(pid_t tp[], int size);
static int msg_all(int in, int out, pid_t buf[], int nodes);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nproc;
	int rio_in, rio_out;	/* reverse ring IO */
	int fd[2], rfd[2];
	pid_t pid;
	int i, status;
	pid_t *tp, *rtp;

	status = nproc = pid = 0;
	bzero(fd, sizeof(int)*2);
	bzero(rfd, sizeof(int)*2);
	if (argc != 2 || ((nproc = atoi(argv[1])) <= 0))
		err_quit("Uasge: %s processes", argv[0]);

	fprintf(stderr, "Creating ring of %d processes\n", nproc);
				/* create initial ring */
	Pipe(fd);
	Dup2(fd[IN], STDIN_FILENO);
	Dup2(fd[OUT], STDOUT_FILENO);
	Close(fd[IN]), Close(rfd[OUT]);
				/* create initial reverse ring */
	Pipe(fd);
	rio_in = dup(fd[IN]);
	rio_out = dup(fd[OUT]);
	Close(fd[IN]), Close(fd[OUT]);
	
	for (i = 1; i < nproc; i++) {
		Pipe(fd);
		Pipe(rfd);
		if ((pid = Fork()) > 0) {
			Dup2(fd[OUT], STDOUT_FILENO);
			Dup2(rfd[IN], rio_in);
		} else {
			Dup2(fd[IN], STDIN_FILENO);
			Dup2(rfd[OUT], rio_out);
		}
		Close(fd[IN]), Close(fd[OUT]);
		Close(rfd[IN]), Close(rfd[OUT]);
		if (pid > 0)
			break;
	}

	if ((tp = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	(void)msg_all(STDIN_FILENO, STDOUT_FILENO, tp, nproc);
	if ((rtp = calloc((size_t)nproc, sizeof(pid_t))) == NULL)
		err_sys("calloc error");
	(void)msg_all(rio_in, rio_out, rtp, nproc);
	
	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	pprintt(tp, nproc);
	pprintt(rtp, nproc);
	free(tp);
	free(rtp);
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
