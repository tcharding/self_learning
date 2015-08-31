#include "tch.h"
#include "asyncmonitorsignal.h"
#include "restart.h"
#include <aio.h>

static struct aiocb aiocb;
static sig_atomic_t doneflag;
static int fdout, globalerror, totalbytes;
static int readstart(void);
static void seterror(int error);

/* ARGSUSED */
static void aiohandler(int signo)
{
	int myerrno, mystatus, serrno;

	serrno = errno;
	myerrno = aio_error(&aiocb);
	err_msg("in aiohandler");
	if (myerrno == EINPROGRESS) {
		errno = serrno;
		return;
	}
	if (myerrno) {
		seterror(myerrno);
		errno = serrno;
		return;
	}
	mystatus = aio_return(&aiocb);
	totalbytes += mystatus;
	aiocb.aio_offset += mystatus;
	if (mystatus == 0)
		doneflag = 1;
	else if (r_write(fdout, (char *)aiocb.aio_buf, mystatus) == -1)
		seterror(errno);
	else if (readstart() == -1)
		seterror(errno);
	errno = serrno;
}

/* readstart: start asynchronous read */
static int readstart(void)
{
	int error;

	if ((error = aio_read(&aiocb)))
		seterror(errno);
	return error;
}

/* seterror: update globalerror if zero */
static void seterror(int error)
{
	if (!globalerror)
		globalerror = error;
	doneflag = 1;
}

/* ------------------------Public Functions------------------------ */

/* getbytes: return totalbytes if doneflag */
int getbytes(void)
{
	if (doneflag)
		return totalbytes;
	errno = EINVAL;
	return -1;
}

/* getdone: check for done */
int getdone(void)
{
	return doneflag;
}

/* geterror: return globalerror value if doneflag */
int geterror(void)
{
	if (doneflag)
		return globalerror;
	errno = EINVAL;
	return -1;
	
}

/* */
int initread(int fdread, int fdwrite, int signo, char *buf, int bufsize)
{
	int error;
	sigset_t oldset, fullset;

	if ((sigfillset(&fullset) == -1) ||
	    (sigprocmask(SIG_SETMASK, &fullset, &oldset) == -1)) {
		seterror(errno);
		return -1;
	}
				/* set up structure */
	aiocb.aio_fildes = fdread;
	aiocb.aio_offset = 0;
	aiocb.aio_buf = (void *)buf;
	aiocb.aio_nbytes = bufsize;
	aiocb.aio_sigevent.sigev_notify = SIGEV_SIGNAL;
	aiocb.aio_sigevent.sigev_value.sival_ptr = &aiocb;
	fdout = fdwrite;
	doneflag = 0;
	globalerror = 0;
	totalbytes = 0;
	error = readstart();	/* start first read */
	if (sigprocmask(SIG_SETMASK, &oldset, NULL) == -1) {
		seterror(errno);
		return -1;
	}
	return error;
}

/* initsignal: set up the handle for the async I/O */
int initsignal(int signo)
{
	if (signal(signo, aiohandler) == SIG_ERR)
		return -1;
	return 0;
}

/* supenduntilmaybeready: retun 1 if done, 0 otherwise */
int supenduntilmaybeready(void)
{
	const struct aiocb *aiocblist;

	aiocblist = &aiocb;
	aio_suspend(&aiocblist, 1, NULL);
	return doneflag;
}
