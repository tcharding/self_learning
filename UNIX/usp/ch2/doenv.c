#define _GNU_SOURCE
#include "vector.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#define MAX_CMD 1024

extern char **environ;

/*static int pprint(char *s);*/
static void writev(char **v);
static int process_nv(struct dv *p, char ***envpp, int iflag);

int main(int argc, char *argv[])
{
	int iflag;		/* 1 if '-i' option present */
	struct dv *nv;		/* name=value */
	struct dv *utl;		/* utility */
	char **envp;
	
	iflag = 0;
	nv = creatdv();
	utl = creatdv();
	while (--argc > 0) {
		++argv;
		if ((!strcmp(*argv, "-i")) || (!strcmp(*argv, "-")))
			iflag = 1;
		else if (strchr(*argv, '=')) 
			adds(nv, *argv);
		else 
			adds(utl, *argv);
	}
	process_nv(nv, &envp, iflag); 
	if (utl->v)		/* execute utility */
		execvpe(*utl->v, utl->v, envp);
	else			/* print environment */
		writev(envp);

	freedv(nv);
	freedv(utl);
	return 0;
}

/* pprint: pretty print 
static int pprint(char *s)
{
	printf("%s\n", s);
	return 0;
}
*/

/* writev: write v to stdout */
static void writev(char **v)
{
	if (v)
		while(*v)
			printf("%s\n", *v++);
}

/* process_nv: set envp depending on iflag and process name=value pairs */
static int process_nv(struct dv *p, char ***envpp, int iflag)
{
	char **v, **save;
	char *name, *value;

	if (p == NULL)
		return -1;
	
	if (iflag) {		/* clear environment */
		*envpp = (p->v) ? p->v : NULL;
	} else {		/* setenv */
		if ((save = dupv(p)) != NULL) {
			for (v = save ; *v; v++) {
				name = *v;
				value = strchr(name, '='); /* no need to test return */
				*value++ = '\0';
				setenv(name, value, 1); /* 1 to overwrite */
			}
			freev(save);
		}
		*envpp = environ;
	}
	return 0;
}
