/* Exercise 6.3 */
#include "tlpi_hdr.h"

#ifndef BUF_SIZE
#define BUF_SIZE 1024
#endif

Boolean is_env_equal(const char *key, const char *value);
int my_setenv(const char *name, const char *vaule, int overwrite);
int my_unsetenv(const char *name);

/* test my_setenv and my_unsetenv implementation */
int
main(void)
{

	if (!is_env_equal("HOME", "/home/tobin"))
		errExit("env home");

	if (is_env_equal("NON_EXISTANT", NULL))
		errExit("env non-existant");

	putenv("HOME=/big/bills/home/directory");

	if (!is_env_equal("HOME", "/big/bills/home/directory"))
		errExit("env home");

	if (my_setenv("HOME", "/home/tobin", 0) == -1)
		errExit("my_setenv");
	if (is_env_equal("HOME", "/home/tobin"))
		errExit("env home shouldn't have re-written");

	if (my_setenv("HOME", "/home/tobin", 1) == -1)
		errExit("my_setenv");
	if (!is_env_equal("HOME", "/home/tobin"))
		errExit("env home should have re-written");

	if (my_unsetenv("HOME") == -1)
		errExit("unsetenv");
	
	if (is_env_equal("HOME", "/home/tobin"))
		errExit("unsetenv failed");

	exit(EXIT_SUCCESS);
}

Boolean
is_env_equal(const char *key, const char *value)
{
	char *s;

	if ((s = getenv(key)) == NULL)
		return FALSE;
	
	if (strcmp(s, value) != 0)
		return FALSE;

	return TRUE;
}

/* my_setenv: implement setenv (suspect error handling)*/
int
my_setenv(const char *name, const char *value, int overwrite)
{
	char *s;
	char *envVar;
	size_t totalLength;
	int err;

	totalLength = (strlen(name) + strlen(value) + 2);
	if (totalLength > BUF_SIZE)
		return 1;

	envVar = malloc(totalLength);
	if (envVar == NULL)
		errExit("malloc");
	
	s = getenv(name);
	if (s && !overwrite)
		return 2;

	strncpy(envVar, name, strlen(name));
	envVar[strlen(name)] = '=';
	strncpy(envVar+strlen(name)+1, value, strlen(value));
	envVar[totalLength] = '\0';

	err = putenv(envVar);
	if (err == -1)
		return errno;

	return 0;
}

/* my_unsetenv: implement unsetenv */
int
my_unsetenv(const char *name)
{
	char *s, *empty;
	int err;
	
	s = getenv(name);
	if (s == NULL)
		return 0;

	empty = malloc(strlen(name) + 2);
	strcpy(empty, name);
	strcpy(empty+strlen(name), "=");
	empty[strlen(name)+1] = '\0';

	err = putenv(empty);
	if (err == -1)
		return errno;
	
	return 0;
}
