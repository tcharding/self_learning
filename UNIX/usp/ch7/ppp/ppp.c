#include "tch.h"
#include "vector.h"
#include "data.h"
#include "string.h"
#include <ctype.h>
#include <fcntl.h>

#define PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
#define VERBOSE 0
int parse_config(const char *file, vec_t *v);
static int read_infile(const char *file);
static int write_outfile(const char *file);
static int do_trans(int c, const char *t);
static void write_through(void);
static void pinfo(vec_t *v, int nid);
static int myc(vec_t *v, int nid);
static int myt(vec_t *v, int nid);

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
	if (VERBOSE) {
		fprintf(stderr, "Pipline Pre-processor:\n");
		fprintf(stderr, "\tstages: %d\n \tconfig file: %s\n", n, config);
		fprintf(stderr, "\tinput file: %s\n \toutput file :%s\n\n", infile, outfile);
	}
	
	v_init(&v);
	if ((stages = parse_config(config, &v)) == -1)
		err_sys("parse_config error");
	if (stages != n)
		err_quit("config file and command args do not agree n:%d stages:%d",
			 n, stages);
	nproc = n + 2;
	/* fprintf(stderr, "Parsed config, v:\n"); */
	/* v_foreach(&v, adt_pprint); */
	/* fprintf(stderr, "--------\n"); */
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

	/* if (pid > 0)		/\* synchronize processes *\/ */
	/* 	if (wait(&status) == -1) */
	/* 		err_sys("wait error"); */
	
	if (nid == 0) {		/* handle input */
		/* pinfo(nid); */
		read_infile(infile);
	} else if (nid == nproc - 1) { /* handle output */
		/* pinfo(nid); */
		write_outfile(outfile);
	}
	else {			/* do transformations */
		pinfo(&v, nid);
		do_trans(myc(&v, nid), myt(&v, nid));
	}
	return 0;
}
/* pinfo: dump process info to stderr */
static void pinfo(vec_t *v, int nid)
{
	if (VERBOSE) {
		fprintf(stderr, "nid: %d, pid: %ld ppid: %ld c:%c t:%s\n",
			nid , (long)getpid(), (long)getppid(),
			myc(v, nid), myt(v, nid));
	}

}
/* read_infile: read file and write to stdout */
static int read_infile(const char *file)
{
	int fd;

	fd = Open(file, O_RDONLY, 0);
	Dup2(fd, STDIN_FILENO);
	Close(fd);

	(void)write_through();
	return 0;
}

/* write_outfile: write stdin to file */
static int write_outfile(const char *file)
{
	int fd;
	int c;

	fd = Open(file, O_WRONLY | O_CREAT, PERMS);
	Dup2(fd, STDOUT_FILENO);
	Close(fd);

	while ((c = getc(stdin)) != EOF) {
		/* fputc(c, stderr); */
		if (fputc(c, stdout) == EOF)
			err_sys("output error");
	}
	if (ferror(stdin))
		err_sys("input error");
	/* write_through(); */
	return 0;
}

/* TODO */
static int do_trans(int c, const char *t)
{
	int readc, retval;

	while ((readc = getc(stdin)) != EOF) {
		if (readc == c) {
			retval = fputs(t, stdout);
		} else {
			retval = fputc(readc, stdout);
		}
		if (retval == EOF)
			err_sys("output error");
	}
	if (ferror(stdin))
		err_sys("input error");
	
	return retval;
}

/* write_through: write stdin to stdout */
static void write_through(void)
{
	int c;

	while ((c = getc(stdin)) != EOF)
		if (fputc(c, stdout) == EOF)
			err_sys("output error");
	if (ferror(stdin))
		err_sys("input error");

}

/* myc: wrapper to access character from transformation vector */
static int myc(vec_t *v, int nid)
{
	return v->data[nid-1]->c;
}

/* myt: wrapper to access t from transformation vector */
static int myt(vec_t *v, int nid)
{
	return v->data[nid-1]->s;
}

	
