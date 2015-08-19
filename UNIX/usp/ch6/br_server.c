#include "tch.h"
#include "barrier.h"
#include <fcntl.h>
#include <errno.h>

#define REL_CHAR 'g'

static int loop(int n, const char *request, const char *release);

/* barrier server */
int main(int argc, char *argv[])
{
	char *req, *rel;
	char *name, *prog;	
	int n;
	
	prog = argv[0];
	if (argc != 3)
		err_quit("Usage: %s name size", prog);
	name = argv[1];
	n = atoi(argv[2]);

	if (br_pipenames(name, &req, &rel) == -1) 
		err_sys("br_pipnames error");

	if (br_createpipes(rel, req) != 0)
		err_sys("br_createpipes error");

	if (loop(n, req, rel) == -1)
		err_sys("Server:");
	exit(1);		/* shouldn't get here */
}
static int loop(int n, const char *req, const char *rel)
{
	int reqfd, relfd;		 
	char buf[1];
	ssize_t nbytes;
	int i;
	

	for ( ; ; ) {
		if ((reqfd = open(req, O_RDONLY)) == -1) 
			return -1;
		
		for (i = 0; i < n; i++) {
			if ((nbytes = read(reqfd, buf, 1)) == -1)
				return -1;
			err_msg("i: %d read: %s pipe: %s\n", i, buf, req);
		}
		if (close(reqfd) == -1) 
			return -1;

		buf[0] = REL_CHAR;
		if ((relfd = open(rel, O_WRONLY)) == -1) 
			return -1;
		for (i = 0; i < n; i++) {
			if (write(relfd, buf, 1) != 1) 
				return -1;
		}
		if (close(relfd) == -1) 
			return -1;
	}      
	err_dump("loop: should not get here");
	return -1;
}
