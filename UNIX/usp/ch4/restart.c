/* attribution: UNIX Systems Programming - Robbins and Robbins */
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <sys/select.h>
#include <sys/time.h>
#include <sys/wait.h>
#include "restart.h"

#ifdef PIPE_BUF
#define BLKSIZE PIPE_BUF
#else
#define BLKSIZE 4096
#endif

#define MILLION 1000000L
#define D_MILLION 1000000.0

/* Private functions */

static int gettimeout(struct timeval end, struct timeval *timeoutp)
{
	gettimeofday(timeoutp, NULL);
	timeoutp->tv_sec = end.tv_sec - timeoutp->tv_sec;
	timeoutp->tv_usec = end.tv_usec - timeoutp->tv_usec;
	if (timeoutp->tv_usec >= MILLION) {
		timeoutp->tv_sec++;
		timeoutp->tv_usec -= MILLION;
	}
	if (timeoutp->tv_usec < 0) {
		timeoutp->tv_sec--;
		timeoutp->tv_usec += MILLION;
	}
	if ((timeoutp->tv_sec < 0) ||
	    ((timeoutp->tv_sec == 0) && (timeoutp->tv_usec == 0))) {
		errno = ETIME;
		return -1;
	}
	return 0;
}

/* Restart versions of traditional functions */

int r_close(int fd)
{
	int retval;
	
	while (retval = close(fd), retval == -1 && errno == EINTR)
		;
	return retval;
}
int r_dup2(int fd, int fd2)
{
	int retval;
	
	while (retval = dup2(fd, fd2), retval == -1 && errno == EINTR)
		;
	return retval;
}
int r_open2(const char *path, int oflag)
{
	int retval;
	
	while (retval = open(path, oflag), retval == -1 && errno == EINTR)
		;
	return retval;
}
int r_open3(const char *path, int oflag, mode_t mode)
{
	int retval;
	
	while (retval = open(path, oflag, mode), retval == -1 && errno == EINTR)
		;
	return retval;
}
ssize_t r_read(int fd, void *buf, size_t count)
{
       	int retval;
	
	while (retval = read(fd, buf, count), retval == -1 && errno == EINTR)
		;
	return retval;
}	
pid_t r_wait(int *stat_loc)
{
	int retval;

	while (((retval = wait(stat_loc)) == -1) && (errno == EINTR))
		;
	return retval;
}

pid_t r_waitpid(pid_t pid, int *stat_loc, int options)
{
	int retval;

	while (((retval = waitpid(pid, stat_loc, options)) == -1) && (errno == EINTR))
		;
	return retval;
}
ssize_t r_write(int fd, void *buf, size_t size)
{
	char *bufp;
	size_t bytestowrite;
	ssize_t byteswritten;
	size_t totalbytes;

	for (bufp = buf, bytestowrite = size, totalbytes = 0;
	     bytestowrite > 0;
	     bufp += byteswritten, bytestowrite -= byteswritten) {
		byteswritten = write(fd, buf, bytestowrite);
		if ((byteswritten) == -1 && (errno != EINTR))
			return -1;
		if (byteswritten == -1)
			byteswritten = 0;
		totalbytes += byteswritten;
	}
	return totalbytes;
}

/* Utility functions */

struct timeval add2currenttime(double seconds)
{
	struct timeval newtime;

	gettimeofday(&newtime, NULL);
	newtime.tv_sec += (int)seconds;
	newtime.tv_usec += (int)((seconds - (int)seconds)*D_MILLION + 0.5);
	if (newtime.tv_usec >= MILLION) {
		newtime.tv_sec++;
		newtime.tv_usec -= MILLION;
	}
	return newtime;
}

int copyfile(int fromfd, int tofd)
{
	int bytesread;
	int totalbytes = 0;

	while ((bytesread = readwrite(fromfd, tofd)) > 0)
		totalbytes += bytesread;
	return totalbytes;
}

ssize_t readblock(int fd, void *buf, size_t size)
{
	char *bufp;
	ssize_t bytesread;
	size_t bytestoread;
	size_t totalbytes;

	for (bufp = buf, bytestoread = size, totalbytes = 0;
	     bytestoread > 0;
	     bufp += bytesread, bytestoread -= bytesread) {
		bytesread = read(fd, bufp, bytestoread);
		if ((bytesread == 0) && (totalbytes == 0)) {
			return 0;
		}
		if (bytesread == 0) {
			errno = EINVAL;
			return -1;
		}
		if ((bytesread) == -1 && (errno != EINTR)) {
			return -1;
		}
		if (bytesread == -1)
			bytesread = 0;
		totalbytes += bytesread;
	}
	return totalbytes;
}

/* readline: read fd up to nbytes into buf, removes newline character */
int readline(int fd, char *buf, int nbytes)
{
	int nread = 0;
	int retval;

	while (nread < nbytes - 1) {
		retval = read(fd, buf + nread, 1);
		if ((retval == -1) && (errno == EINTR))
			continue;
		if ((retval == 0) && (nread == 0))
			return 0;
		if (retval == 0)
			break;
		if (retval == -1)
			return -1;
		nread++;
		if (buf[nread-1] == '\n') {
			buf[nread] = '\0';
			return nread;
		}
	}
	errno = EINVAL;
	return -1;
}

ssize_t readtimed(int fd, void *buf, size_t nbytes, double seconds)
{
	struct timeval timedone;

	timedone = add2currenttime(seconds);
	if (waitfdtimed(fd, timedone) == -1)
		return -1;
	return r_read(fd, buf, nbytes);
}
/* readwrite: read up to BLKSIZE bytes from fromfd and write to tofd */
int readwrite(int fromfd, int tofd)
{
	char buf[BLKSIZE];
	int bytesread;

	if ((bytesread = r_read(fromfd, buf, BLKSIZE)) < 0)
		return -1;
	if (bytesread == 0)
		return 0;
	if (r_write(tofd, buf, bytesread) < 0)
		return -1;
	return bytesread;
}

int readwriteblock(int fromfd, int tofd, char *buf, int size)
{
	int bytesread;

	bytesread = readblock(fromfd, buf, size);
	if (bytesread != size)	/* can only be 0 or -1 */
		return bytesread;
	return r_write(tofd, buf, size);
}

int waitfdtimed(int fd, struct timeval end)
{
	fd_set readset;
	int retval;
	struct timeval timeout;

	if ((fd < 0) || (fd >= FD_SETSIZE)) {
		errno = EINVAL;
		return -1;
	}
	FD_ZERO(&readset);
	FD_SET(fd, &readset);
	if (gettimeout(end, &timeout) == -1)
		return -1;
	while (((retval = select(fd+1, &readset, NULL, NULL, &timeout)) == -1)
	       && (errno == EINTR)) {
		if (gettimeout(end, &timeout) == -1)
			return -1;
		FD_ZERO(&readset);
		FD_SET(fd, &readset);
	}
	if (retval == 0) {
		errno = ETIME;
		return -1;
	}
	if (retval == -1)
		return -1;
	return 0;
}
