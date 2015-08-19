#include <stdio.h>
#include <stdlib.h>
#define DELIM ":"

char **getpaths(void);

/* test getpath */
int main(void)
{
	char **v;

	v = getpaths();
	while (*v != NULL) 
		printf("%s\n", *v++);
	return 0;
}

/* getpath: create vector from $PATH */
char **getpaths(void)
{
	char **v;
	int makeargv(const char *s, const char *delim, char ***argvp);
	char *pathp;

	if ((pathp = getenv("PATH")) == NULL) 
		return NULL;
	if (makeargv(pathp, DELIM, &v) == -1)
		return NULL;
	else
		return v;
}
