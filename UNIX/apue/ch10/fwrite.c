#include "apue.h"
#include <setjmp.h>
#define SIG_MSG "signal SIGALRM caught\n"
#define NUM 1024*1024*100

void sig_alrm(int);

int main(void)
{
	char *file = "out.file";
	FILE *stream;
	char *buf = "this is i string";
	int i;
	
	if (signal(SIGALRM, sig_alrm) == SIG_ERR)
		err_sys("signal error");
	stream = Fopen(file, "w");
	alarm(1);
	for (i = 0; i < NUM; i++) {
		if (fwrite(buf, strlen(buf), 1, stream) == -1)
			err_sys("fwrite error");
	}
	

	return 0;
}

void sig_alrm(int signo)
{
	int err = errno;
	write(STDOUT_FILENO, SIG_MSG, sizeof(SIG_MSG)-1);
	errno = err;
}
