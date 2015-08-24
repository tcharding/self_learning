#define _XOPEN_SOURCE
#include "tch.h"
#ifndef PIPE_BUF
#define PIPE_BUF 255
#endif
#define TOKEN 'g'

enum { IN, OUT };

static void critical_section(int nid, int i);
static void ptrastr(int fd, const char *s, int n);
static void wastesometime(int n);
static void read_token(int nid);
static void write_token(int nid);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nid;
	int nproc;
	int fd[2];
	pid_t pid;
	int i, n;
	char buf[1];
	ssize_t res;
	int m;
	int rand;

	nproc = n = pid = 0;
	fd[0] = fd[1] = 0;
	buf[0] = (char)0;
	if (argc != 3 ||
	    ((nproc = atoi(argv[1])) <= 0) ||
	    ((m = atoi(argv[2])) <= 0))
		err_quit("Uasge: %s processes multiplier", argv[0]);

	fprintf(stderr, "Creating ring of %d processes (m:%d)\n", nproc, m);
	nid = 0;
	srand48((long)getpid()); /* seed random number generator */
	
				/* set up initial ring */
	Pipe(fd);
	Dup2(fd[IN], STDIN_FILENO);
	Dup2(fd[OUT], STDOUT_FILENO);
	Close(fd[IN]), Close(fd[OUT]);
	
	for (i = 1; i < nproc; i++) {
		Pipe(fd);
		if ((pid = Fork()) > 0)
			Dup2(fd[1], STDOUT_FILENO);
		else {
			Dup2(fd[0], STDIN_FILENO);
			nid = i;
		}
		Close(fd[0]);
		Close(fd[1]);
		if (pid > 0)
			break;
	}
	if (nid == 0) {		/* start passing the token */
		buf[0] = TOKEN;
		res = write(STDOUT_FILENO, buf, 1);
		if (res == -1 || (int)res != 1)
			err_sys("write error");
		
	}
	rand = (int)((double)m * drand48());
	rand = 5;
	for (i = 0; i < rand; i++) {
		read_token(nid);
		critical_section(nid, i);
		write_token(nid);
	}
	return 0;
}

/* read_token: get mutex token */
static void read_token(int nid)
{
	ssize_t res;
	char buf[1] = "";
	
	res = read(STDIN_FILENO, buf, 1);
	if (res == -1 || (int)res != 1)
		err_sys("read error");
	if (buf[0] != TOKEN)
		err_msg("nid: %d token errro: %d", nid, (int)buf[0]);
}
/* write_token: pass on mutex token */
static void write_token(int nid)
{
	ssize_t res;
	char buf[1];

	buf[0] = TOKEN;
	res = write(STDOUT_FILENO, buf, 1);
	if (res == -1 || (int)res != 1)
		err_sys("nid: %d write error", nid);
}

static void critical_section(int nid, int i)
{
	char buf[BUFSIZ];
	struct timeval tv;

	if (gettimeofday(&tv, NULL) == -1)
		err_sys("time error");
	if (snprintf(buf, BUFSIZ,"nid: %d i: %d %s\n", nid, i, ctime(&tv)) < 0)
		err_sys("snprintf error");
	
	ptrastr(STDERR_FILENO, buf, (int)strlen(buf));

}
static void ptrastr(int fd, const char *s, int n)
{

	while (*s != '\0') {
		if (write(fd, s, 1) == -1)
			err_sys("write error");
		s++;
		wastesometime(n);
	}
}

/* wastesometime: */
static void wastesometime(int n)
{
	static volatile int dummy = 0;
	int i;

	for (i = 0; i < n; i++)
		dummy++;
}
