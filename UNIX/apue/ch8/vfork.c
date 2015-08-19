#include "apue.h"

int globvar = 6;

int main(void)
{
	int var, res;
	pid_t pid;

	var = 88;
	printf("before vfork\n"); /* we do not flush stdio */
	if ((pid = vfork()) < 0) {
		err_sys("fork error");
	} else if (pid == 0) {	/* child */
		globvar++;
		var++;
				/* simulate close of stdio streams */
		fclose(stdout);
		fclose(stdin);
		fclose(stderr);
		_exit(0);	/* child terminates */
	}
				/* parent continues here */
	res = printf("pid = %ld, glob = %d, var = %d\n", (long)getpid(), globvar, var);
	if (res == -1) {	/* stdout closed */
		FILE *fp;
		char *err_file = "error.log";
		if ((fp = fopen(err_file, "w+")) == NULL)
			err_sys("fopen error for %s", err_file);
		if ((fputs("stdout appears not to be open\n", fp)) == EOF)
			err_sys("fputs error");
		fprintf(fp, "pid = %ld, glob = %d, var = %d\n", (long)getpid(), globvar, var);
	}
	exit(0);
}
