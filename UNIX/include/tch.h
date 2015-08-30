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

/* Macro to print debugging info */
/* #define PUT(fmt, ...) \ */
/*            do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while (0)  */

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
 * Prototypes for library routines. Stevens and Rago [2013]
 */				
void	err_msg(const char *, ...); /* error routines */
void	err_dump(const char *, ...) __attribute__((noreturn));
void	err_quit(const char *, ...) __attribute__((noreturn));
void	err_cont(int, const char *, ...);
void	err_exit(int, const char *, ...) __attribute__((noreturn));
void	err_ret(const char *, ...);
void	err_sys(const char *, ...) __attribute__((noreturn));

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

#endif	/* TCH_H */
