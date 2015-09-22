/*
 * Our own header, to be included before all standard system headers.
 */
#ifndef	_APUE_H
#define	_APUE_H

#define _POSIX_C_SOURCE 200809L

#if defined(SOLARIS)		/* Solaris 10 */
#define _XOPEN_SOURCE 600
#else
#define _XOPEN_SOURCE 700
#endif

#include <sys/types.h>		/* some systems still require this */
#include <sys/stat.h>
#include <sys/termios.h>	/* for winsize */
#if defined(MACOS) || !defined(TIOCGWINSZ)
#include <sys/ioctl.h>
#endif

#include <stdio.h>		/* for convenience */
#include <stdlib.h>		/* for convenience */
#include <stddef.h>		/* for offsetof */
#include <string.h>		/* for convenience */
#include <unistd.h>		/* for convenience */

#include <error.h>		/* for perror */
#include <errno.h>
#include <signal.h>		/* for SIG_ERR */
#include <time.h>		/* timespec{} for pselect */

#include <sys/types.h>		/* basic system data types */
#include <sys/select.h>		/* for select / pselect */
#include <sys/time.h>		/* timeval{} for select */
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <arpa/inet.h>		/* inet {3} functions */

#define	MAXLINE	4096			/* max line length */

/*
 * Default file access permissions for new files.
 */
#define	FILE_MODE	(S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

/*
 * Default permissions for new directories.
 */
#define	DIR_MODE	(FILE_MODE | S_IXUSR | S_IXGRP | S_IXOTH)

typedef	void Sigfunc(int);	/* for signal handlers */

#define	min(a,b)	((a) < (b) ? (a) : (b))
#define	max(a,b)	((a) > (b) ? (a) : (b))

/* Define bzero() as macro. Stevens, Fenner, Rudoff [2004] */
#define bzero(ptr, n) memset(ptr, 0, (size_t)(n))

/*
 * Prototypes for our own functions.
 */
int buf_args(char *, int (*func)(int, char **)); /* bufargs.c */
char *path_alloc(size_t *);		    /* pathalloc.c */
long open_max(void);			    /* openmax.c */
void pr_mask(const char *);		    /* prmask.c */
int set_cloexec(int);			    /* setfd.c */
void clr_fl(int, int);			    /* clrfl.c */
void set_fl(int, int);			    /* setfl.c */
void pr_exit(int);			    /* prexit.c */
Sigfunc *signal(int signo, Sigfunc *func);  /* signal.c */
Sigfunc	*signal_intr(int, Sigfunc *);	    /* signalintr.c */
void daemonize(const char *);		    /* daemonize.c */
void sleep_us(unsigned int);		    /* sleepus.c */
ssize_t readn(int, void *, size_t);	    /* readn.c */
ssize_t writen(int, const void *, size_t);  /* writen.c */
int fd_pipe(int *);			    /* spipe.c */
int recv_fd(int, ssize_t (*func) (int, const void *, size_t)); /* recvfd.c */
int send_fd(int, int);					       /* sendfd.c */
int send_err(int, int, const char *);			       /* senderr.c */
int serv_listen(const char *);	/* servlisten.c */
int serv_accept(int, uid_t *);	/* servaccept.c */
int cli_conn(const char *);	/* cliconn.c */

int tty_cbreak(int);			  /* {Prog raw} */
int tty_raw(int);			  /* {Prog raw} */
int tty_reset(int);			  /* {Prog raw} */
void tty_atexit(void);			  /* {Prog raw} */
struct termios *tty_termios(void);	  /* {Prog raw} */
int ptym_open(char *, int);		  /* {Prog ptyopen} */
int ptys_open(char *);			  /* {Prog ptyopen} */
#ifdef	TIOCGWINSZ
pid_t pty_fork(int *, char *, int, const struct termios *,
	       const struct winsize *);	/* {Prog ptyfork} */
#endif

int lock_reg(int, int, int, off_t, int, off_t); /* lockreg.c */
#define	read_lock(fd, offset, whence, len) \
			lock_reg((fd), F_SETLK, F_RDLCK, (offset), (whence), (len))
#define	readw_lock(fd, offset, whence, len) \
			lock_reg((fd), F_SETLKW, F_RDLCK, (offset), (whence), (len))
#define	write_lock(fd, offset, whence, len) \
			lock_reg((fd), F_SETLK, F_WRLCK, (offset), (whence), (len))
#define	writew_lock(fd, offset, whence, len) \
			lock_reg((fd), F_SETLKW, F_WRLCK, (offset), (whence), (len))
#define	un_lock(fd, offset, whence, len) \
			lock_reg((fd), F_SETLK, F_UNLCK, (offset), (whence), (len))

pid_t lock_test(int, int, off_t, int, off_t); /* locktest.c */
#define	is_read_lockable(fd, offset, whence, len) \
			(lock_test((fd), F_RDLCK, (offset), (whence), (len)) == 0)
#define	is_write_lockable(fd, offset, whence, len) \
			(lock_test((fd), F_WRLCK, (offset), (whence), (len)) == 0)

void err_msg(const char *, ...); /* error.c */
void err_dump(const char *, ...) __attribute__((noreturn));
void err_quit(const char *, ...) __attribute__((noreturn));
void err_cont(int, const char *, ...);
void err_exit(int, const char *, ...) __attribute__((noreturn));
void err_ret(const char *, ...);
void err_sys(const char *, ...) __attribute__((noreturn));

void log_msg(const char *, ...); /* errorlog.c */
void log_open(const char *, int, int);
void log_quit(const char *, ...) __attribute__((noreturn));
void log_ret(const char *, ...);
void log_sys(const char *, ...) __attribute__((noreturn));
void log_exit(int, const char *, ...) __attribute__((noreturn));

/*
 * Parent/child synchronisation functions (tellwait.c)
 *
 *   *_PARENT called by child process 
 *   *_CHILD called by parent process 
 */
void TELL_WAIT(void);		/* init */
void TELL_PARENT(void);	/* tell parent we're done */
void TELL_CHILD(pid_t);		/* tell child we're done */
void WAIT_PARENT(void);		/* wait for parent to call TELL_CHILD */
void WAIT_CHILD(void);		/* wait for child to call TELL_PARENT */

/* prototypes for our stdio wrapper functions (wrapstio.c) */
void  Fclose(FILE *);
FILE *Fdopen(int, const char *);
char *Fgets(char *, int, FILE *);
FILE *Fopen(const char *, const char *);
void  Fputs(const char *, FILE *);

/* prototypes for our Unix wrapper functions (lib/wrapunix.c) */
void *Calloc(size_t, size_t);
void Close(int);
void Dup2(int, int);
int Fcntl(int, int, int);
void Gettimeofday(struct timeval *, void *);
int Ioctl(int, int, void *);
pid_t Fork(void);
void *Malloc(size_t);
int Mkstemp(char *);
void *Mmap(void *, size_t, int, int, int, off_t);
int Open(const char *, int, mode_t);
void Pipe(int *fds);
ssize_t Read(int, void *, size_t);
void Sigaddset(sigset_t *, int);
void Sigdelset(sigset_t *, int);
void Sigemptyset(sigset_t *);
void Sigfillset(sigset_t *);
int Sigismember(const sigset_t *, int);
void Sigpending(sigset_t *);
void Sigprocmask(int, const sigset_t *, sigset_t *);
char *Strdup(const char *);
long Sysconf(int);
/* void Sysctl(int *, uint32_t, void *, size_t *, void *, size_t); */
void Unlink(const char *);
pid_t Wait(int *);
pid_t Waitpid(pid_t, int *, int);
void Write(int, void *, size_t);

/* 
 * Tobin's functions
 */

/* prototypes for pthread wrapper functions (wrappthread.c) */
void Pthread_create(pthread_t *thread, const pthread_attr_t *attr,
		    void *(*start_routine) (void *), void *arg);
void Pthread_join(pthread_t thread, void **retval);
void Pthread_mutex_unlock(pthread_mutex_t *lock);
void Pthread_mutex_lock(pthread_mutex_t *lock);

/* prototypes for functions */
void msg(const char *fmt, ...);

#endif	/* _APUE_H */
