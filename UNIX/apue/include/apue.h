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


#include <sys/stat.h>
#include <sys/termios.h>	/* for winsize */
#if defined(MACOS) || !defined(TIOCGWINSZ)
#include <sys/ioctl.h>
#endif
#include <stddef.h>		/* for offsetof */
#include <stdio.h>		/* for convenience */
#include <stdlib.h>		/* for convenience */
#include <string.h>		/* for convenience */
#include <unistd.h>		/* for convenience */

#include <sys/types.h>		/* basic system data types */
#include <error.h>		/* for perror */
#include <errno.h>

/* including fcntl opens the door to hideous IO bugs when one mixes up 
   calls to open and fopen */ /* #include <fcntl.h> */ 

#include <signal.h>
#include <sys/select.h>		/* for select / pselect */
#include <time.h>		/* timespec{} for pselect */
#include <sys/time.h>		/* timeval{} for select */
#include <arpa/inet.h>		/* inet {3} functions */
#include <sys/ioctl.h>
#include <sys/wait.h>


#define	MAXLINE	4096			/* max line length */

/*
 * Default file access permissions for new files.
 */
#define	FILE_MODE	(S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

/*
 * Default permissions for new directories.
 */
#define	DIR_MODE	(FILE_MODE | S_IXUSR | S_IXGRP | S_IXOTH)

typedef	void	Sigfunc(int);	/* for signal handlers */

#define	min(a,b)	((a) < (b) ? (a) : (b))
#define	max(a,b)	((a) > (b) ? (a) : (b))

/* Define bzero() as macro. Stevens, Fenner, Rudoff [2004] */
#define bzero(ptr, n) memset(ptr, 0, (size_t)(n))

/*
 * Memory Streams. Tobin Harding
 */
struct mems {			
	FILE *stream;
	char *buf;
	size_t size;
};
/* int m_open_memstream(struct mems *m); */
/* int m_insert(struct mems *m, char *fmt, ...); */

/* prototypes for Tobin's functions */
void msg(const char *fmt, ...);

/*
 * Prototypes for our own functions.
 */
char *path_alloc(size_t *);	/* {Prog pathalloc} */
long open_max(void);		/* {Prog openmax} */
void pr_mask(const char *);	/* {Prog prmask} */

int		 set_cloexec(int);					/* {Prog setfd} */
void	 clr_fl(int, int);
void	 set_fl(int, int);					/* {Prog setfl} */

void	 pr_exit(int);						/* {Prog prexit} */


Sigfunc	*signal_intr(int, Sigfunc *);		/* {Prog signal_intr_function} */

void	 daemonize(const char *);			/* {Prog daemoninit} */

void	 sleep_us(unsigned int);			/* {Ex sleepus} */
ssize_t	 readn(int, void *, size_t);		/* {Prog readn_writen} */
ssize_t	 writen(int, const void *, size_t);	/* {Prog readn_writen} */

int		 fd_pipe(int *);					/* {Prog sock_fdpipe} */
int		 recv_fd(int, ssize_t (*func)(int,
		         const void *, size_t));	/* {Prog recvfd_sockets} */
int		 send_fd(int, int);					/* {Prog sendfd_sockets} */
int		 send_err(int, int,
		          const char *);			/* {Prog senderr} */
int		 serv_listen(const char *);			/* {Prog servlisten_sockets} */
int		 serv_accept(int, uid_t *);			/* {Prog servaccept_sockets} */
int		 cli_conn(const char *);			/* {Prog cliconn_sockets} */
int		 buf_args(char *, int (*func)(int,
		          char **));				/* {Prog bufargs} */

int		 tty_cbreak(int);					/* {Prog raw} */
int		 tty_raw(int);						/* {Prog raw} */
int		 tty_reset(int);					/* {Prog raw} */
void	 tty_atexit(void);					/* {Prog raw} */
struct termios	*tty_termios(void);			/* {Prog raw} */

int		 ptym_open(char *, int);			/* {Prog ptyopen} */
int		 ptys_open(char *);					/* {Prog ptyopen} */
#ifdef	TIOCGWINSZ
pid_t	 pty_fork(int *, char *, int, const struct termios *,
		          const struct winsize *);	/* {Prog ptyfork} */
#endif

int		lock_reg(int, int, int, off_t, int, off_t); /* {Prog lockreg} */

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

pid_t	lock_test(int, int, off_t, int, off_t);		/* {Prog locktest} */

#define	is_read_lockable(fd, offset, whence, len) \
			(lock_test((fd), F_RDLCK, (offset), (whence), (len)) == 0)
#define	is_write_lockable(fd, offset, whence, len) \
			(lock_test((fd), F_WRLCK, (offset), (whence), (len)) == 0)

void	err_msg(const char *, ...);			/* {App misc_source} */
void	err_dump(const char *, ...) __attribute__((noreturn));
void	err_quit(const char *, ...) __attribute__((noreturn));
void	err_cont(int, const char *, ...);
void	err_exit(int, const char *, ...) __attribute__((noreturn));
void	err_ret(const char *, ...);
void	err_sys(const char *, ...) __attribute__((noreturn));

void	log_msg(const char *, ...);			/* {App misc_source} */
void	log_open(const char *, int, int);
void	log_quit(const char *, ...) __attribute__((noreturn));
void	log_ret(const char *, ...);
void	log_sys(const char *, ...) __attribute__((noreturn));
void	log_exit(int, const char *, ...) __attribute__((noreturn));

void	TELL_WAIT(void);		/* parent/child from {Sec race_conditions} */
void	TELL_PARENT(pid_t);
void	TELL_CHILD(pid_t);
void	WAIT_PARENT(void);
void	WAIT_CHILD(void);

/* prototypes for our stdio wrapper functions (wrapstio.c) */
void	 Fclose(FILE *);
FILE	*Fdopen(int, const char *);
char	*Fgets(char *, int, FILE *);
FILE	*Fopen(const char *, const char *);
void	 Fputs(const char *, FILE *);

/* prototypes for our Unix wrapper functions (lib/wrapunix.c) */
void *Calloc(size_t, size_t);
void  Close(int);
void  Dup2(int, int);
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

#endif	/* _APUE_H */
