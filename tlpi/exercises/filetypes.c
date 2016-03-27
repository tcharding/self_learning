/* Exercise 18.7 */
#include <ftw.h>
#include "tlpi_hdr.h"

#define MAX_FD 20

static int fn(const char *fpath, const struct stat *sb,
	      int typeflag, struct FTW *ftwbuf); /* nftw */


int total;
int regular;
int dir;
int soft;
int fifo;
int blk;
int chr;
int socket;

int
main(int argc, char *argv[]) {
	int opt;
	char *path;
	int typeflag;

	typeflag = 0;
	
	if (argc < 2 || strcmp(argv[1], "--help") == 0)
		usageErr("%s [-d] path\n", argv[0]);

	while (( opt = getopt(argc, argv, "d")) != -1) {
		switch(opt) {
		case 'd':
			typeflag |= FTW_DEPTH;
			break;
		default:
			errExit("unknow option: %s", argv[optind]);
		}
	}
	if (optind >= argc)
		errExit("expected pathname");
	path = argv[optind];

	
	if (nftw(path, fn, MAX_FD, typeflag) == -1)
		errExit("nftw");

	printf("Directory Tree Walked: %s\n", path);
	printf("\tTotal: %d\n", total);
	printf("\tRegular: %d\n", regular);
	printf("\tDirectories: %d\n", dir);
	printf("\tSoft Links: %d\n", soft);
	printf("\tBlock Devices: %d\n", blk);
	printf("\tCharacter Devices: %d\n", chr);
	printf("\tFIFO's: %d\n", fifo);
	printf("\tSockets: %d\n", socket);

	exit(EXIT_SUCCESS);
}

/* BUG: doesn't always count soft links */
static int
fn(const char *fpath, const struct stat *sb, int typeflag, struct FTW *ftwbuf)
{
	if (S_ISDIR(sb->st_mode))
		dir++;
	if (S_ISREG(sb->st_mode))
		regular++;
	if (S_ISCHR(sb->st_mode))
		chr++;
	if (S_ISBLK(sb->st_mode))
		blk++;
	if (S_ISFIFO(sb->st_mode))
		fifo++;
	if (typeflag & FTW_SL)
		soft++;
	if (S_ISSOCK(sb->st_mode))
		socket++;

	total++;

	return 0;
}

    
