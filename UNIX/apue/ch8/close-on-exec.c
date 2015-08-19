#include "apue.h"
#include <dirent.h>

void print_coe(DIR *dp);

/* verify close on exec */
int main(void)
{
	pid_t pid;
	DIR *dp;

	if ((dp = opendir("/")) == NULL)
		fprintf(stderr, "opendir error");
	fprintf(stderr, "before fork: ");
	print_coe(dp);

	if ((pid = fork()) < 0) 
		err_sys("fork error");

	print_coe(dp);

	return 0;
}

void print_coe(DIR *dp)
{
	/* CAN NOT LOCATE DEFINITION OF STRUCT DIR */
	fprintf(stderr, "pid: %ld\t\n", (long)getpid());
}
