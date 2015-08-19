#include "apue.h"
#define MAX 4

int main(void)
{
	char buf[MAX];

	while (fgets(buf, MAX, stdin) != NULL) 
		if (fputs(buf, stdout) == EOF)
			err_sys("output error");
	if (ferror(stdin))
		err_sys("input error");
	exit(0);
}
