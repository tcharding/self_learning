#include "tch.h"
#include "restart.h"

int main(int argc, char *argv[])
{
	int n, s;
	double sleep, num;
	ssize_t nbytes;
	char buf[BUFSIZ];

	bzero(buf, sizeof(buf));
	s = n = 0;
	if (argc != 3)
		err_quit("Usage: %s n s\n", argv[0]);

	if (((n = atoi(argv[1])) <= 0) ||
	    ((s = atoi(argv[2])) <= 0))
		err_sys("atoi error");

	srand48((long)getpid()); /* set seed */
	/* sleep = (drand48() * s); */
	sleep = (double)s;
	fprintf(stderr, "sleep: %g\n", sleep);
	
	for ( ; ; ) {
		nbytes = readtimed(STDIN_FILENO, buf, BUFSIZ-1, sleep);

		if (nbytes == -1) { /* timedout */
			num = (drand48() * n);
			fprintf(stderr, "%d", (int)num);
			if (fflush(stdout) == -1)
				err_msg("fflush error");
		} else {
			if (buf[nbytes-1] == '\n')
				buf[nbytes-1] = '\0';
			else
				buf[nbytes] = '\0';
			fprintf(stderr, "%s\n", buf);
		}
	}
	return 0;
}


