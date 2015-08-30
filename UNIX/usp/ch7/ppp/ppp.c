#include "tch.h"
#include "vector.h"
#include "data.h"
#include "string.h"
#include <ctype.h>

int parse_config(const char *file, vec_t *v);

/* create ring of n processes */
int main(int argc, char *argv[])
{
	int fd[2];
	pid_t pid;
	int i, status, nproc;
	int n, stages;
	char *infile, *outfile, *config;
	int nid;
	vec_t v;		/* transformations from config file */
		
	n = nproc = pid = status = fd[0] = fd[1] = 0;
	if (argc != 5) 
		err_quit("Uasge: %s stages config.in file.in file.out", argv[0]);
	if ((n = atoi(argv[1])) < 0)
		err_sys("atoi failed with %s", argv[1]);
	config = argv[2], infile = argv[3], outfile = argv[4];

	v_init(&v);
	if ((stages = parse_config(config, &v)) == -1)
		err_sys("parse_config error");
	if (stages != n)
		err_quit("config file and command args do not agree n:%d stages:%d",
			 n, stages);
	nproc = n + 2;
	/* fprintf(stderr, "Printing v:\n"); */
	/* v_foreach(&v, adt_pprint); */
				/* create initial ring */
	nid = 0;
	Pipe(fd);
	Dup2(fd[0], STDIN_FILENO);
	Dup2(fd[1], STDOUT_FILENO);
	Close(fd[0]);
	Close(fd[1]);
				/* add process to ring */
	for (i = 1; i < nproc; i++) {
		Pipe(fd);
		if ((pid = Fork()) > 0)
			Dup2(fd[1], STDOUT_FILENO);
		else {
			Dup2(fd[0], STDIN_FILENO);
			nid = i;
		}
		Close(fd[0]);
		Close(fd[1]);
		if (pid > 0)
			break;
	}

	if (pid > 0)
		if (wait(&status) == -1)
			err_sys("wait error");
	
	if (nid == 0) {		/* handle input */
		fprintf(stderr, "nid: %d, pid: %ld ppid: %ld\n",
			nid, (long)getpid(), (long)getppid());
	} else if (nid == nproc - 1) { /* handle output */
		fprintf(stderr, "nid: %d, pid: %ld ppid: %ld\n",
			nid, (long)getpid(), (long)getppid());
	}
	else {			/* do transformations */
		fprintf(stderr, "nid: %d, pid: %ld ppid: %ld, c: %c s: %s\n",
			nid , (long)getpid(), (long)getppid(),
			v.data[nid-1]->c, v.data[nid-1]->s);
	}
	return 0;
}
