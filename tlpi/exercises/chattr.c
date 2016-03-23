/* Exercise 15.7 */
#include <stdarg.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <linux/fs.h>
#include "tlpi_hdr.h"

static int chattr(int fd, const int attr, int action, Boolean info);

enum {
	SET = 1,
	UNSET = 2,
	EQUALS = 3,
};

/* implement chattr */
int
main(int argc, char *argv[]) {

	int opt;
	int fd;
	int attr;
	char *file, *mode, *p;
	Boolean info = TRUE;	/* error messages */
	int action;

	if (argc < 2 || strcmp(argv[1], "--help") == 0) {
		eprintf("Usage: %s [-f] mode file", argv[0]);
		fprintf(stderr, "\t mode: +-=[aAcCdDeijsStTu]");
		exit(EXIT_FAILURE);
	}

	while ((opt = getopt(argc, argv, "f")) != -1) {
		switch (opt) {
		case 'f':
			info = FALSE;
			break;
		default: /* '?' */
			usageErr("%s [-a] file\n", argv[0]);
		}
	}

	if (optind+2 > argc) {
		eprintf("Error: optind: %d argc: %d\n", optind, argc);
		exit(EXIT_FAILURE);
	}
	mode = argv[optind];
	file = argv[optind+1];

	/* Build bit mask based on attribute string in argv[1] */
	switch (*mode) {
	case '+': action = SET; break;
	case '-': action = UNSET; break;
	case '=': action = EQUALS; break;
	default:
		eprintf("unknow action: %s\n", mode);
		exit(EXIT_FAILURE);
	}
	
	attr = 0;
	for (p = ++mode; *p != '\0'; p++) {
		switch (*p) {
		case 'a': attr |= FS_APPEND_FL;         break;
		case 'A': attr |= FS_NOATIME_FL;        break;
		case 'c': attr |= FS_COMPR_FL;          break;
		case 'd': attr |= FS_NODUMP_FL;         break;
		case 'D': attr |= FS_DIRSYNC_FL;        break;
		case 'i': attr |= FS_IMMUTABLE_FL;      break;
		case 'j': attr |= FS_JOURNAL_DATA_FL;   break;
		case 's': attr |= FS_SECRM_FL;          break;
		case 'S': attr |= FS_SYNC_FL;           break;
		case 't': attr |= FS_NOTAIL_FL;         break;
		case 'T': attr |= FS_TOPDIR_FL;         break;
		case 'u': attr |= FS_UNRM_FL;           break;
		default:
			eprintf("unknown option: %c\n", *p);
			exit(EXIT_FAILURE);
		}
	}
	
	fd = open(file, O_RDONLY);
	if (fd == -1)
		errExit("open failed: %s\n", file);
	
	chattr(fd, attr, action, info);

	if (close(fd) == -1)
		errExit("close");
	
	exit(EXIT_SUCCESS);
}

static int
chattr(int fd, const int newAttr, int action, Boolean info)
{
	int attr;
	
	if (ioctl(fd, FS_IOC_GETFLAGS, &attr) == -1) {
		fprintf(stderr, "ioctl failed to get flags");
		return 1;
	}

	switch(action) {
	case SET: attr |= newAttr; break;
	case UNSET: attr &= ~newAttr; break;
	case EQUALS: attr = newAttr;
	default: errExit("chattr");
	}

	if (ioctl(fd, FS_IOC_SETFLAGS, &attr) == -1) {
		errMsg("ioctl");
		fprintf(stderr, "ioctl failed to set flags");
		return 1;
	}
		
	return 0;
}

