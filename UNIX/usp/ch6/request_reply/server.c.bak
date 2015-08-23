#include "tch.h"
#include "rr.h"
#include <linux/limits.h>

/* request/reply server */
int main(void)
{
	char *rq_fifo = RQ_FIFO; /* request */
	int rq_fd;
	char rq_buf[PIPE_BUF];
	char *rs_fifo;		/* response */
	int rs_fd;
	char rs_buf[PIPE_BUF];	

				/* create and open rq_fifo */
	if ((mkfifo(rq_fifo, PIPE_PERMS) == -1) && (errno != EEXIST))
		err_sys("mkfifo error");
	rq_fd = Open(rq_fifo, O_RDONLY, O_CREAT);
	
	pprint_hdr();	
				/* main server loop */
	for ( ; ; ) {		 
		Read(rq_fd, rq_buf, 4); /* blocks waiting for request */
		rq_buf[4] = '\0';
			
		/* if (Fork() > 0)	       /\* parent *\/ */
		/* 	continue; */
		/* pid = atoi(rq_buf);	       /\* get pid from request message *\/ */
		/* fprintf(stderr, "DEBUG: pid: %d\n", pid); */
		if (rr_pipe_name(atoi(rq_buf), &rs_fifo))
			err_quit("rr_pipename error");
		fprintf(stderr, "attempting to open %s\n", rs_fifo);
		if ((rs_fd = open(rs_fifo, O_WRONLY)) == -1)
			err_sys("open error");
		snprintf(rs_buf, PIPE_BUF, "Got your request, go ahead");
		Write(rs_fd, rs_buf, PIPE_BUF);
		Close(rs_fd);
		/* exit(0); */	/* child terminates */
	 } 
	
	return 0;
}
