/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */
/*
 * The client command for printing documents.  Opens the file
 * and sends it to the printer spooling daemon.  Usage:
 * 	print [-t] filename
 */
#include "apue.h"
#include "print.h"
#include <fcntl.h>
#include <pwd.h>

/*
 * Needed for logging funtions.
 */
int log_to_stderr = 1;

void submit_file(int fd, int sockfd, const char *fname, size_t nbytes,
		 int text, const char *orientation, int sides);
static void set_orientation(int *oloc, const char *s);
static void set_sides(int *oloc, int sides);

int main(int argc, char *argv[])
{
	int fd, sfd, err, c;
	struct stat sbuf;
	char *host, *file;
	struct addrinfo	*ailist, *aip;
	char *orient;
	int text;
	int sides;		/* 0=default 1=one-sided */
				/* 2=two-sided-long-edge  3=two-sided-short-edge */
	
	sides = 0;
	text = 0;
	err = 0;
	while ((c = getopt(argc, argv, "s:to:")) != -1) {
		switch (c) {
		case 's':
			sides = atol(optarg);
			if (sides < 1 || sides > 3)
				err_sys("print: invalid sides option");
			break;
		case 'o':
			orient = optarg;
			break;
		case 't':
			text = 1;
			break;

		case '?':
			err = 1;
			break;
		}
	}
	if (err || (optind != argc - 1))
		err_quit("usage: print [-t] filename");
	file = argv[optind];
	if ((fd = open(file, O_RDONLY)) < 0)
		err_sys("print: can't open %s", file);
	if (fstat(fd, &sbuf) < 0)
		err_sys("print: can't stat %s", file);
	if (!S_ISREG(sbuf.st_mode))
		err_quit("print: %s must be a regular file", file);
	/*
	 * Get the hostname of the host acting as the print server.
	 */
	if ((host = get_printserver()) == NULL)
		err_quit("print: no print server defined");
	if ((err = getaddrlist(host, "print", &ailist)) != 0)
		err_quit("print: getaddrinfo error: %s", gai_strerror(err));

	for (aip = ailist; aip != NULL; aip = aip->ai_next) {
		if ((sfd = connect_retry(AF_INET, SOCK_STREAM, 0,
		  aip->ai_addr, aip->ai_addrlen)) < 0) {
			err = errno;
		} else {
			submit_file(fd, sfd, file, sbuf.st_size,
				    text, orient, sides);
			exit(0);
		}
	}
	err_exit(err, "print: can't contact %s", host);
}

/*
 * Send a file to the printer daemon.
 */
void submit_file(int fd, int sockfd, const char *fname, size_t nbytes,
		 int text, const char *orientation, int sides)
{
	int nr, nw, len;
	struct passwd *pwd;
	struct printreq	req;
	struct printresp res;
	char buf[IOBUFSZ];
	int oflag;		/* orientation flag */
	int sflag;		/* sides flag */
	
	bzero(&req, sizeof(req));
	bzero(&res, sizeof(res));
	oflag = sflag = 0;
	/*
	 * First build the header.
	 */
	if ((pwd = getpwuid(geteuid())) == NULL) {
		strcpy(req.usernm, "unknown");
	} else {
		strncpy(req.usernm, pwd->pw_name, USERNM_MAX-1);
		req.usernm[USERNM_MAX-1] = '\0';
	}
	if (orientation != NULL)
		set_orientation(&oflag, orientation);
	if (sides != 0)
		set_sides(&sflag, sides);
	req.size = htonl(nbytes);
	req.flags |= htonl(PR_TEXT);
	req.flags |= htonl(oflag); /* turn on orientation flags */
	req.flags |= htonl(sflag);

	if ((len = strlen(fname)) >= JOBNM_MAX) {
				/* Truncate th=e filename (+-5 for ellipsis + null) */
		strcpy(req.jobnm, "... ");
		strncat(req.jobnm, &fname[len-JOBNM_MAX+5], JOBNM_MAX-5);
	} else {
		strcpy(req.jobnm, fname);
	}

	/*
	 * Send the header to the server.
	 */
	nw = writen(sockfd, &req, sizeof(struct printreq));
	if (nw != sizeof(struct printreq)) {
		if (nw < 0)
			err_sys("can't write to print server");
		else
			err_quit("short write (%d/%d) to print server",
			  nw, sizeof(struct printreq));
	}

	/*
	 * Now send the file.
	 */
	while ((nr = read(fd, buf, IOBUFSZ)) != 0) {
		nw = writen(sockfd, buf, nr);
		if (nw != nr) {
			if (nw < 0)
				err_sys("can't write to print server");
			else
				err_quit("short write (%d/%d) to print server",
				  nw, nr);
		}
	}

	/*
	 * Read the response.
	 */
	if ((nr = readn(sockfd, &res, sizeof(struct printresp))) !=
	  sizeof(struct printresp))
		err_sys("can't read response from server");
	if (res.retcode != 0) {
		printf("rejected: %s\n", res.msg);
		exit(1);
	} else {
		printf("job ID %ld\n", (long)ntohl(res.jobid));
	}
}

/* set orientation oloc based on s */
static void set_orientation(int *oloc, const char *s)
{
	if (strcmp(s, "portrait") == 0)
		*oloc = PR_ORIENT_P;
	else if (strcmp(s, "reverse-portrait") == 0)
		*oloc = PR_ORIENT_RP;
	else if (strcmp(s, "landscape") == 0)
		*oloc = PR_ORIENT_L;
	else if (strcmp(s, "reverse-landscape") == 0)
		*oloc = PR_ORIENT_RL;
	else {
		*oloc = 0;
		fprintf(stderr, "set_orientation error: Unknown option %s", s);
	}
}
/* set sides sloc based on sides */
static void set_sides(int *sloc, int sides)
{
	if (sides == 1)
		*sloc = PR_SIDES_ONE;
	else if (sides == 2)
		*sloc = PR_SIDES_TWO_LONG;
	else if (sides == 3)
		*sloc = PR_SIDES_TWO_SHORT;
}
