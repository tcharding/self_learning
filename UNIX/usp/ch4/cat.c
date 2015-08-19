#include "restart.h"
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#define BLKSIZE 4096		/* this should be from a system header */

int cat(int fd, size_t bytes);
void usage(char *cmd);

int main(int argc, char *argv[])
{
	int fd;
	char *file, *prog;
	int opt;		/* for getopt */
	int bytes;		/* for '-u' option */

	bytes = 0;
	prog = argv[0];
	while ((opt = getopt(argc, argv, "hu:")) != -1) {
		switch (opt) {
		case 'u':
			bytes = atoi(optarg);
			break;
		case 'h':
			usage(prog);
			exit(EXIT_SUCCESS);
		default: 
			usage(prog);
			exit(EXIT_FAILURE);
               }
	}

	if (argv[optind] == NULL) {
		cat(STDIN_FILENO, bytes);
		return 0;
	}
				/* process file arguments */
	for (file = argv[optind]; file != NULL; file = argv[++optind]) {
		if (!strcmp(file, "-")) {
			fd = STDIN_FILENO;
		} else {
			fd = open(file, O_RDONLY);
			if (fd == -1) {
				fprintf(stderr, "open error: %s\n", file);
				return 1;
			}
		}
		cat(fd, bytes);
		if (fd != STDIN_FILENO)
			close(fd);
	}
	return 0;
}

/* cat: write fd to stdout, 
   if bytes is non-zero flush buffer after 'bytes' data is read */
int cat(int fd, size_t bytes)
{
	char *buf;
	int size, n;

	size = (bytes == 0) ? BLKSIZE : bytes;

	if ((buf = malloc(size)) == NULL) 
		return -1;

	while ((n = read(fd, buf, size)) > 0) {
		if (write(STDOUT_FILENO, buf, n) == -1) {
			perror("write error");
			return 1;
		}
		if (n == -1) {
			fprintf(stderr, "read error\n");
			return 1;
		}
	}
	free(buf);
	return 0;
}

void usage(char *prog)
{
	fprintf(stderr, "Usage: %s [-u bytes] [file]...\n", prog);
}
