#define _XOPEN_SOURCE
#include "tch.h"
#include <fcntl.h>
#include <pwd.h>

/*
 * Question does not take into consideration 'Maildir' mail directory format
 */
#define MAIL_DEFAULT "/var/mail/"
#define MAILCHECK_DEFAULT 600

void usage(char *prog);

int main(int argc, char *argv[])
{
	struct stat statbuf;
	struct passwd *p;
	char *path, *prog, *s;
	int opt, nsec;
	FILE *fp;
	char *buf;
	size_t size;
	
	nsec = 0;
	path = NULL;
	prog = argv[0];
	while ((opt = getopt(argc, argv, "hs:p:")) != -1) {
		switch (opt) {
		case 'h':
			usage(prog);
			return 0;
			break;
		case 's':
			nsec = atoi(optarg);
			break;
		case 'p':
			path = optarg;
			break;
		default:
			usage(prog);
			return 1;
			break;
		}
	}
	
	if (path == NULL) {
		path = getenv("MAIL");
		if (path == NULL)
			path = MAIL_DEFAULT;
	}

	if (nsec == 0) {
		s = getenv("MAILCHECK");
		nsec = (s != NULL) ? atoi(s) : MAILCHECK_DEFAULT;
	}
	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("ms_open failed");

	if ((p = getpwuid(getuid())) == NULL)
		err_sys("getpwuid failed");

				/* build absolute pathname */

	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream failed");

	fprintf(fp, "%s", path);
	fflush(fp);

	if (*(buf + strlen(buf) - 1) != '/')
		fprintf(fp, "/");
	fprintf(fp, "%s", p->pw_name);
	fclose(fp);

	fprintf(stderr, "%s\n", buf);

	for ( ; ; ) {
		if (stat(buf, &statbuf) == 0) {
			if (statbuf.st_size != 0)
				fprintf(stderr, "beep\n");
		}
		sleep(nsec);
	}
}

void usage(char *prog)
{
	fprintf(stderr, "Usage: %s [-s n] [-p pathname]", prog);
}
