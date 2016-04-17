/* Exercise 45.1 */
#include <stdint.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <sys/stat.h>
#include "tlpi_hdr.h"

char *tab[] = {
	"/bin/abook",
	"/etc/drirc",
	"/etc/bash.bashrc",
	NULL
};

static void verify(key_t (*fn)(const char *, int));
static void dumpKeyPathProj(key_t key, const char *path, int proj);
static key_t my_ftok(const char *path, int proj);

int
main(int argc, char *argv[]) {

	verify(ftok);
	verify(my_ftok);

	exit(EXIT_SUCCESS);
}

static void
verify(key_t (*fn)(const char *, int))
{
	key_t key;
	char **ptr;
	int proj;
	
	for (ptr = tab; *ptr != NULL; ptr++) {
		proj = 1;
		key = fn(*ptr, proj);
		dumpKeyPathProj(key, *ptr, proj);

		proj++;
		key = fn(*ptr, proj);
		dumpKeyPathProj(key, *ptr, proj);
	}
	printf("-----------\n");
}

/* dumpKeyPathProj: pretty print */
static void
dumpKeyPathProj(key_t key, const char *path, int proj)
{
	struct stat sb = { 0 };
	ino_t inode;
	
	if (stat(path, &sb) == -1)
		errExit("stat");
	inode = sb.st_ino;
	printf("key: %x proj: %x inode: %x dev: %x\n", key, proj,
	       (int) inode, (int) sb.st_dev);
}

/* my_ftok: implement ftok(3) */
static key_t
my_ftok(const char *path, int proj)
{
	struct stat sb = { 0 };
	uint32_t key = 0;

	if (stat(path, &sb) == -1)
		return (key_t) -1; /* errno set by stat */

	key = proj & ~(~0 << 8);
	key <<= 8;
	key += sb.st_dev & ~(~0 << 8);
	key <<= 16;
	key += sb.st_ino  & ~(~0 << 16);

	return (key_t) key;
}
