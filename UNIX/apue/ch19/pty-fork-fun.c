#include "apue.h"
#include <stropts.h>
#include <sys/stat.h>

static void	set_noecho(int);	/* at the end of this file */

int main(void)
{
	int fdm, noecho, nread;
	pid_t pid;
	char slave_name[20];	/* apue picks this size? */
	char buf[BUFSIZ];
	
	/* struct termios	orig_termios; */
	/* struct winsize	size; */

	/* if (interactive) {	/\* fetch current termios and window size *\/ */
	/* 	if (tcgetattr(STDIN_FILENO, &orig_termios) < 0) */
	/* 		err_sys("tcgetattr error on stdin"); */
	/* 	if (ioctl(STDIN_FILENO, TIOCGWINSZ, (char *) &size) < 0) */
	/* 		err_sys("TIOCGWINSZ error"); */
	/* 	pid = pty_fork(&fdm, slave_name, sizeof(slave_name), */
	/* 		       &orig_termios, &size); */
	/* } else { */

	noecho = 0;
	
	pid = pty_fork(&fdm, slave_name, sizeof(slave_name),
			NULL, NULL);

	if (pid < 0) {
		err_sys("fork error");
	} else if (pid == 0) {		/* child */
		if (noecho)
			set_noecho(STDIN_FILENO);	/* stdin is slave pty */

		if (execl("./prog", "./prog", NULL) < 0)
			err_sys("can't execute cmd");
		err_sys("child should not get here");
	}
				/* parent */
	msg("parent got pid %ld for slave\n", (long)pid);
	
	/* if (ioctl(fdm, TIOCSIG, SIGTERM) == -1) */
	/* 	err_sys("ioctl error"); */
	if ((nread = read(fdm, buf, BUFSIZ)) < 0)
		err_sys("read error");
	fprintf(stderr, "parent writing return from slave: ");
	if (write(STDOUT_FILENO, buf, nread) < 0)
		err_sys("write error");

	return 0;
}

/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */
static void set_noecho(int fd)		/* turn off echo (for slave pty) */
{
	struct termios	stermios;

	if (tcgetattr(fd, &stermios) < 0)
		err_sys("tcgetattr error");

	stermios.c_lflag &= ~(ECHO | ECHOE | ECHOK | ECHONL);

	/*
	 * Also turn off NL to CR/NL mapping on output.
	 */
	stermios.c_oflag &= ~(ONLCR);

	if (tcsetattr(fd, TCSANOW, &stermios) < 0)
		err_sys("tcsetattr error");
}

