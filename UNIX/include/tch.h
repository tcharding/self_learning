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
#include <stdio.h>		/* for convenience */
#include <stdlib.h>		/* for convenience */
#include <string.h>		/* for convenience */
#include <unistd.h>		/* for convenience */
#include <signal.h>		/* for SIG_ERR */
#include <sys/types.h>		/* some systems still require this */
#include <sys/stat.h>

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
 * Prototypes for library routines. Stevens and Rago [2013]
 */				
void	err_msg(const char *, ...); /* error routines */
void	err_dump(const char *, ...) __attribute__((noreturn));
void	err_quit(const char *, ...) __attribute__((noreturn));
void	err_cont(int, const char *, ...);
void	err_exit(int, const char *, ...) __attribute__((noreturn));
void	err_ret(const char *, ...);
void	err_sys(const char *, ...) __attribute__((noreturn));


#endif	/* TCH_H */
