#include "apue.h"

#define MAXARGC 50
#define WHITE " \t\n"

int buf_args(char *buf, int (*optfunc)(int, char **))
{
	char *ptr, **argv;
	int argc, cnt;
	char *cpy;

	cpy = Malloc(strlen(buf)+1);
	strcpy(cpy, buf);
	if (strtok(cpy, WHITE) == NULL) {
		free(cpy);
		return -1;
	}
	cnt = 1;
	while (strtok(NULL, WHITE) != NULL)
		cnt++;
	free(cpy);

	argv = (char **)calloc(cnt, sizeof(char *));
	if (argv == NULL)
		err_sys("calloc error");
	
	argv[argc = 0] = strtok(buf, WHITE);
	while ((ptr = strtok(NULL, WHITE)) != NULL) {
		argv[++argc] = ptr;
	}
	argv[++argc] = NULL;

	return ((*optfunc)(argc, argv));
}
