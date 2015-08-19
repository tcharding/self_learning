#include "tch.h"
#include "barrier.h"
#include <errno.h>
#include <fcntl.h>

#define REQ_CHAR 'b'
#define REQ_PERMS (S_IRUSR | S_IWUSR | S_IWGRP | S_IWOTH)
#define REL_PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

/* waitbarrier: block at barrier */
int waitbarrier(char *name)
{
	char *rel, *req;
	int reqfd, relfd;
	char buf[1];
	int e;			/* errno save */

	if (name == NULL) {
		errno = EINVAL;
		return -1;
	}

	if (br_pipenames(name, &req, &rel) == -1) {
		e = errno, err_ret("br_pipnames error"), errno = e;
		return -1;
	}

	if (br_createpipes(rel, req) != 0) {
		e = errno, err_ret("br_creatpipes error"), errno = e;
		return -1;
	}

	buf[0] = REQ_CHAR;
	if ((reqfd = open(req, O_WRONLY)) == -1)
		return -1; /* errno set by open */
	if (write(reqfd, buf, 1) != 1) {
		e = errno, close(reqfd), errno = e;
		return -1;
	}
	if (close(reqfd) == -1) 
		return -1;

	if ((relfd = open(rel, O_RDONLY)) == -1) 
		return -1; 
	if (read(relfd, buf, 1) != 1) {
		e = errno, close(relfd), errno = e;
		return -1;
	}
	if (close(relfd) == -1) 
		return -1;

	return 0;
}
/* br_pipenames: fill request and release with pipe names, 
   free request and release with free() */
int br_pipenames(const char *name, char **request, char **release)
{
	FILE *fp;
	char *buf;
	size_t size;
	int e;
	
	if ((fp = open_memstream(&buf, &size)) == NULL)
		return -1;
	if ((fprintf(fp, "%s", name) < 0) || 
	    (fprintf(fp, ".request") < 0)) {
		e = errno, free(buf), errno = e;
		return -1;
	}
	if (fclose(fp) != 0) {
		e = errno, free(buf), errno = e;
		return -1;
	}
	*request = buf;
	
	if ((fp = open_memstream(&buf, &size)) == NULL)
		return -1;
	if ((fprintf(fp, "%s", name) < 0) || 
	    (fprintf(fp, ".release") < 0)) {
		e = errno, free(request), free(buf), errno = e;
		return -1;
	}
	if (fclose(fp) != 0)  {
		e = errno, free(request), free(buf), errno = e;
		return -1;
	}
	*release = buf;
	return 0;
}

/* br_createpipes: make FIFO's for barrier client/server IPC */
int br_createpipes(const char *request, const char *release)
{
	if (request == NULL || release == NULL) {
		errno = EINVAL;
		return -1;
	}
	if ((mkfifo(request, REQ_PERMS) == -1) && (errno != EEXIST)) 
		return -1;	/* mkfifo sets errno */

	if ((mkfifo(release, REL_PERMS) == -1) && (errno != EEXIST)) 
		return -1;	/* mkfifo sets errno */
	return 0;
}

/* br_rmpipes: remove pipes created with br_createpipes() 
   NULL arguments, or non-existent pipes, not error */
int br_rmpipes(const char *request, const char *release)
{
	if ((unlink(release) == -1) && (errno != ENOENT))
		return -1;
	if ((unlink(request) == -1) && (errno != ENOENT))
		return -1;
	return 0;
}
