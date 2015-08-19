/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 2.2 */
#include <errno.h>
#include <stdlib.h>
#include <string.h>

/* makeargv: create argv vector from s */
int makeargv(const char *s, const char *delim, char ***argvp)
{
	int error, i, ntokens;
	const char *snew;
	char *t;

	if ((s == NULL) || (delim == NULL) || (argvp == NULL)) {
		errno = EINVAL;
		return -1;
	}
	*argvp = NULL;
	snew = s + strspn(s, delim); /* snew is real start of string */
	if ((t = malloc(strlen(snew) + 1)) == NULL)
		return -1;
	strcpy(t, snew);
	ntokens = 0;
	if (strtok(t, delim) != NULL) /* count the number of tokens */
		for (ntokens = 1; strtok(NULL, delim) != NULL; ntokens++)
			;
				/* create argument array for ptrs to the tokens */
	if ((*argvp = malloc(ntokens + 1)) == NULL) {
		error = errno;
		free(t);
		errno = error;
		return -1;
	}
				/* insert ptrs to tokens into the argument array */
	if (ntokens == 0)
		free(t);
	else {
		strcpy(t, snew);
		**argvp = strtok(t, delim);
		for (i = 1; i < ntokens; i++)
			*((*argvp) + i) = strtok(NULL, delim);
	}
	*((*argvp) + ntokens) = NULL; /* add final NULL pointer */
	return ntokens;
}

/* freemakeargv: free memory allocated by makeargv */
void freemakeargv(char **argv)
{

	if (argv != NULL) {
		if (*argv != NULL)
			free(*argv);
		free(argv);
	}
}
