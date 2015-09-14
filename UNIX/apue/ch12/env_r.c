#include "apue.h"
#include <pthread.h>

extern char **environ;
static pthread_mutex_t env_mutex;
static pthread_once_t init_done = PTHREAD_ONCE_INIT;

/* prototypes */
static void thread_init(void);
int getenv_r(const char *name, char *buf, int buflen);
int putenv_r(const char *s);

/* */
int putenv_r(const char *s)
{
	pthread_once(&init_done, thread_init);
	pthread_mutex_lock(&env_mutex);
	putenv(s_dup(s)); /* cheat so I don't have to reallocate environ memory */
	pthread_mutex_unlock(&env_mutex);
	return (ENOENT);
	
}
/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */
int getenv_r(const char *name, char *buf, int buflen)
{
	int i, len, olen;

	pthread_once(&init_done, thread_init);
	len = strlen(name);
	pthread_mutex_lock(&env_mutex);
	for (i = 0; environ[i] != NULL; i++) {
		if ((strncmp(name, environ[i], len) == 0) &&
		    (environ[i][len] == '=')) {
			olen = strlen(&environ[i][len+1]);
			if (olen >= buflen) {
				pthread_mutex_unlock(&env_mutex);
				return(ENOSPC);
			}
			strcpy(buf, &environ[i][len+1]);
			pthread_mutex_unlock(&env_mutex);
			return 0;
		}
	}
	pthread_mutex_unlock(&env_mutex);
	return (ENOENT);
}
	

/* */
static void thread_init(void)
{
	pthread_mutexattr_t attr;

	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	pthread_mutex_init(&env_mutex, &attr);
}

