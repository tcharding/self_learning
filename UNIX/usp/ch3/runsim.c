#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include "restart.h"

#ifndef MAX_CANON
#define MAX_CANON 4096
#endif

int makeargv(const char *s, const char *delim, char ***argvp); /* makeargv.c */

/* static void writev(char **v); */

int main(int argc, char *argv[])
{
	int pr_limit;		/* maximum running processes */
	int pr_count;		/* current running processes */
	char readln[MAX_CANON];
	char delim[] = " \t";
	char **cmdv;
	pid_t childpid;
	int makeargv(const char *s, const char *delim, char ***argvp);
	pid_t r_wait(int *stat_loc);
	
	if (argc != 2) {
		fprintf(stderr, "Usage: %s n\n", argv[0]);
		return 1;
	}
	pr_limit = atoi(argv[1]);
	pr_count = 0;

	while ((fgets(readln, MAX_CANON, stdin)) != NULL) {
		if (pr_count >= pr_limit) {
			wait(NULL);
			--pr_count;
		}
		if ((childpid = fork()) < 0) {
			perror("fork");
			return 1;
		} else if (childpid == 0) { /* child */
			if (*(readln + strlen(readln) - 1) == '\n')
				*(readln + strlen(readln) - 1) = '\0';
			if (makeargv(readln, delim, &cmdv) < 0) {
				fprintf(stderr, "makeargv error pid:%ld\n",
					(long)getpid());
				return 1;
			}
			/* writev(cmdv); */
			execvp(*cmdv, cmdv);
			fprintf(stderr, "execvp fail pid:%ld\n",
				(long)getpid()); /* shouldn't get here */
		} else {			 /* parent */
			pr_count++;
			while ((childpid = waitpid(-1, NULL, WNOHANG)))
				if ((childpid == -1) && (errno == EINTR))
					break;
		}
	}
	while(r_wait(NULL) > 0)
		;
	return 0;
}

/* writev: write v to stdout 
static void writev(char **v)
{
	if (v) {
		printf("start:|");
		while(*v)
			printf(" %s |", *v++);
		printf("end\n");
	}
}
*/
