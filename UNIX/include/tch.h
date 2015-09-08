#ifndef	TCH_H			/* Tobin C Harding */
#define	TCH_H
/*
 * This header was created based on ideas, functions, and principles
 * garnered while studying the following texts;
 *
 * * Advanced Programming in the UNIX Environment 
 * *  - Stevens and Rago, 3rd Edition
 *
 * * UNIX SYSTEMS Programming
 * *  - Robbins and Robbins
 *
 * * UNIX Network Programming
 * *  - Stevens, Fenner, Rudoff, Volume 1, Third Edition
 */
/* #define _GNU_SOURCE defined in makefile as gcc flag */

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
void Sysctl(int *, uint32_t, void *, size_t *, void *, size_t);
void Unlink(const char *);
pid_t Wait(int *);
pid_t Waitpid(pid_t, int *, int);
void Write(int, void *, size_t);

/*
 * Prototypes for library routines. Stevens and Rago [2013]
 */				
void	err_msg(const char *, ...); /* error routines */
void	err_dump(const char *, ...) __attribute__((noreturn));
void	err_quit(const char *, ...) __attribute__((noreturn));
void	err_cont(int, const char *, ...);
void	err_exit(int, const char *, ...) __attribute__((noreturn));
void	err_ret(const char *, ...);
void	err_sys(const char *, ...) __attribute__((noreturn));

void	TELL_WAIT(void);		/* parent/child from {Sec race_conditions} */
void	TELL_PARENT(pid_t);
void	TELL_CHILD(pid_t);
void	WAIT_PARENT(void);
void	WAIT_CHILD(void);

/* prototypes for our own library functions. see file for attribution */
Sigfunc *signal(int signo, Sigfunc *func);
Sigfunc *signal_intr(int signo, Sigfunc *func);
ssize_t readn(int fd, void *vptr, size_t n);
ssize_t writen(int fd, const void *vptr, size_t n);
int makeargv(const char *s, const char *delim, char ***argvp);

/* prototypes for our own library  wrapper functions */
Sigfunc *Signal(int, Sigfunc *);
ssize_t Readn(int fd, void *ptr, size_t nbytes);
void Writen(int fd, void *ptr, size_t nbytes);
/*
 * Tobin's functions
 */

/* string library */
char *s_dup(const char *s);
char *s_dupfmt(const char *fmt, ...);

/* output helpers */
void debug(const char *fmt, ...);
void msgn(const char *fmt, ...); /* adds newline */
void msg(const char *fmt, ...);

/* unit test framework */
void tf_equ(int a, int b, int line);

#define FAIL()						\
	fprintf(stderr, "FAIL line: %d\n", __LINE__)
#define T_EQ(a, b)				\
	tf_equ(a, b, __LINE__)

#endif	/* TCH_H */
