#include "tch.h"
#include <fcntl.h>
#include <semaphore.h>
#include <sys/stat.h>

#define DEBUG 1

#define PERMS (mode_t)(S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
#define FLAGS (O_CREAT | O_EXCL)

static int getnamed(const char *name, sem_t **sem, int val);

sem_t *sem;
char *sem_name;

int getlicense(void)
{
	return sem_wait(sem);
}
int returnlicense(void)
{
	return sem_post(sem); 
}
int initlicense(void)
{
	FILE *stream;
	size_t size;
	char *buf;

	if ((stream = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");
	if ((fprintf(stream, "/tmp.license.%ld", (long)getuid()) < 0) ||
	    (fclose(stream) < 0))
		err_sys("stream error");
	
	msg("buf:%s\n", buf);
	sem_name = buf;
	if (getnamed(buf,  &sem, 0) == -1)
		err_sys("getnamed error");
	
	return 0;
}
int addtolicense(int n)
{
	for ( ; n > 0; n--) {
		if (sem_post(sem) == -1)
			err_sys("sem_post error");
	}
	return 0;
}
int removelicenses(int n)
{
	for ( ; n > 0; n--) {
		if (sem_wait(sem) == -1)
			err_sys("sem_wait error");
	}
	return 0;
}

/* clean up resources allocated with initlicense() */
int destroylicense(void)
{
	if ((sem_close(sem) == -1) ||
	    (sem_unlink(sem_name) == -1)) {
		debug("failed to clean up semaphore");
		return -1;
	}
	return 0;
}

/* attr: usp program 14.7 */
static int getnamed(const char *name, sem_t **sem, int val)
{
	while (((*sem = sem_open(name, FLAGS, PERMS, val)) == SEM_FAILED) &&
		errno == EINTR)
		;
	if (*sem != SEM_FAILED)
		return 0;
	if (errno != EEXIST)
		return -1;
	while (((*sem = sem_open(name, 0)) == SEM_FAILED) && (errno == EINTR))
		;
	if (*sem != SEM_FAILED)
		return 0;
	return -1;
}
