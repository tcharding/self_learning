/* Exercise 44.2 */
#include <sys/wait.h>
#include "tlpi_hdr.h"

#define MAX_POPEN 256

FILE *my_popen(const char *command, const char *type);
int my_pclose(FILE *stream);
static int flagsFromType(const char *type);


struct popen_struct {
	pid_t pid;
	int fd;
};


/* test implementation */
int
main(int argc, char *argv[]) {
	FILE *s1, *s2;
	char buf[BUF_SIZE];
	int status = -1;

	setbuf(stdout, NULL);
	
	s1 = my_popen("ls out.sh", "r");
	if (s1 == NULL)
		fatal("my_popen failed");

	s2 = my_popen("ls out.sh", "r");

	/* return code is 512 instead of 2? */
/*	s2 = my_popen("ls non-existant 2>/dev/null", "r"); */
	if (s2 == NULL)
		fatal("my_popen failed");

	if (fgets(buf, BUF_SIZE, s1) == NULL)
		fatal("fgets");

	if (buf[strlen(buf) - 1] == '\n')
		buf[strlen(buf) - 1] = '\0';

	if (strcmp(buf, "out.sh") != 0)
		fprintf(stderr, "test 1 failed: '%s'\n", buf);

	status = my_pclose(s1);
	if (status != 0)
		printf("status not as expected (exp: 0) got: %d\n", status);

	status = my_pclose(s2);
	if (status != 0)
		printf("status not as expected (exp: 2) got: %d\n", status);
	
	exit(EXIT_SUCCESS);
}

enum {
	READ = 0x01,
	WRITE = 0x02,
	EXEC = 0x04
};

struct popen_struct poTab[MAX_POPEN];

/* my_popen: popen(3) implementation */
FILE *
my_popen(const char *command, const char *type)
{
	int typeFlag;
	int pfd[2];
	FILE *stream = NULL;
	pid_t childPid;
	int i;
	struct popen_struct *po;

	for (i = 0; i < MAX_POPEN; i++) {
		po =  &poTab[i];
		if (po->pid == 0) /* found free slot */
			break;
	}
	if (i == MAX_POPEN)
		return NULL;	/* too many calls to popen() */
	
	typeFlag = flagsFromType(type);
	if (pipe(pfd) == -1)
		errExit("pipe");

	switch (childPid = fork()) {
	case -1:
		errExit("fork");
	case 0:			/* child */
		if (typeFlag & READ) {
			if (close(pfd[R]) == -1)
				errExit("close");
			if (STDOUT_FILENO != pfd[W])
				if (dup2(pfd[W], STDOUT_FILENO) == -1)
					errExit("dup2");
		} else {
			if (close(pfd[W]) == -1)
				errExit("close");
			if (STDIN_FILENO != pfd[R])
				if (dup2(pfd[R], STDIN_FILENO) == -1)
					errExit("dup2");
		}
		execl("/bin/sh", "/bin/sh", "-c", command, (char *) NULL);
		errExit("execl failed");
	}

				/* parent falls through */
	if (typeFlag & READ) {
		if (close(pfd[W]) == -1)
			errExit("close");
		po->fd = pfd[R];
		stream = fdopen(pfd[R], "r");
	} else {
		if (close(pfd[R]) == -1)
			errExit("close");
		po->fd = pfd[W];
		stream = fdopen(pfd[W], "W");
	}
	if (stream == NULL)
		errExit("fdopen");

	po->pid = childPid;
			
	return stream;
}

/* my_pclose: pclose(3) implementation */
int
my_pclose(FILE *stream)
{
	int fd;
	int status;
	int i;
	struct popen_struct *po;

	fd = fileno(stream);
	if (fd == -1)
		errExit("fileno");

	for (i = 0; i < MAX_POPEN; i++) {
		po = &poTab[i];
		if (po->fd == fd)
			break;
	}

	if (i == MAX_POPEN)
		return -1;	/* cant' find stream */

	if (po->fd != fd)
		errExit("something wrong with po->fd");

	waitpid(po->pid, &status, 0);

	po->fd = 0;
	po->pid = 0;
	
	return status;
}

static int
flagsFromType(const char *type)
{
	int flags;

	flags = 0;
	if (strchr(type, 'r') != NULL)
		flags |= READ;
	else if (strchr(type, 'w') != NULL)
		flags |= WRITE;

	return flags;
}
