/*************************************************************************\
*                  Copyright (C) Michael Kerrisk, 2015.                   *
*                                                                         *
* This program is free software. You may use, modify, and redistribute it *
* under the terms of the GNU General Public License as published by the   *
* Free Software Foundation, either version 3 or (at your option) any      *
* later version. This program is distributed without any warranty.  See   *
* the file COPYING.gpl-v3 for details.                                    *
\*************************************************************************/

/* tee.c

   Copy stdin to a new file named in argv[1] and also to stdout.
*/
#include <sys/stat.h>
#include <fcntl.h>
#include "tlpi_hdr.h"

#ifndef BUF_SIZE        /* Allow "cc -D" to override definition */
#define BUF_SIZE 1024
#endif

int open_file(const char *filenam, Boolean append);

int
main(int argc, char *argv[])
{
	char *filename;
	int fd;
	ssize_t numRead;
	char buf[BUF_SIZE];
	Boolean append;
	int opt;

	append = FALSE;
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s [-a] file\n", argv[0]);

	while ((opt = getopt(argc, argv, "a")) != -1) {
		switch (opt) {
		case 'a':
			append = TRUE;
			break;
		default: /* '?' */
			usageErr("%s [-a] file\n", argv[0]);
		}
	}

	if (optind >= argc) {
		fprintf(stderr, "Expected file argument after options\n");
		exit(EXIT_FAILURE);
	}
	filename = argv[optind];
	fd = open_file(filename, append);

	while ((numRead = read(STDIN_FILENO, buf, BUF_SIZE)) > 0) {
		if (write(fd, buf, numRead) != numRead)
			fatal("couldn't write whole buffer to file");
		if (write(STDOUT_FILENO, buf, numRead) != numRead)
			fatal("couldn't write whole buffer to stdout");
	}

	if (numRead == -1)
		errExit("read");

	if (close(fd) == -1)
		errExit("close output");

	exit(EXIT_SUCCESS);
}

#define RW_RW_R__ S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH

int
open_file(const char *file, Boolean append)
{
	int fd;
	int flags = O_CREAT | O_WRONLY;
	
	if (append)
		flags |= O_APPEND;
	else
		flags |= O_TRUNC;
			
	fd = open(file, flags, RW_RW_R__);
	if (fd == -1)
		errExit("opening file %s", file);

	return fd;
}
