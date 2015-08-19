#include "apue.h"
#include <fcntl.h>

/* copy file passing over file holes */
int main(int argc, char *argv[])
{
	char *infile, *outfile;
	int infd, outfd;
	char *buf[1];
	int n;
	
	if (argc != 3) {
		fprintf(stderr, "Usage: %s infile outfile\n", argv[0]);
		return 0;
	}
	infile = argv[1];
	outfile = argv[2];
	infd = open(infile, O_RDONLY);
	if (infd == -1) {
		fprintf(stderr, "failed to open file: %s\n", infile);
		return 0;
	}
	umask(0);
	outfd = open(outfile, O_WRONLY | O_CREAT | O_TRUNC, FILE_MODE);
	if (outfd == -1) {
		fprintf(stderr, "failed to open file:%s\n", outfile);
		return 0;
	}
	
	while ((n = read(infd, buf, 1)) == 1) {
		if (buf[0] == 0) {
			fprintf(stderr, "continue");
			continue;
		}
		if (write(outfd, buf, 1) == -1) {
			perror("write error");
			return 1;
		}
		if (n == -1) {
			perror("read error");
			return 1;
		}
	}
	return 0;
}
