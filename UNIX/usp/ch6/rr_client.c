#include "tch.h"
#include "request_reply.h"
#include <errno.h>

int main(void)
{
	/* char *rp_fifo; */
	/* char *rq_fifo = RQ_FIFO;	 */
	/* int rq_fd, rp_fd; */
	/* char *rq_buf; */
	/* char *rp_buf[PIPE_BUF]; */
	
	/* if (rr_pipe_name(getpid(), &rp_fifo)) */
	/* 	err_quit("rr_pipename error"); */
	/* if ((mkfifo(rp_fifo, RP_PERMS) == -1) && (errno != EEXIST)) */
	/* 	err_sys("mkfifo error"); */
	

	/* if (rr_request_msg(getpid(), &rq_buf)) */
	/* 	err_quit("rr_request_msg"); */
	/* rq_fd = Open(rq, O_WRONLY); */
	/* Write(rq_fd, rq_msg, strlen(rq_buf)); */
	/* Close(rq_fd); */

	/* rp_fd = Open(rp_fifo, O_RDONLY); */
	/* Read(rp_fd, rp_buf, PIPE_BUF); */
	/* pprint_hdr(); */
	/* pprint_pid(0, "Got from server: %s\n", rp_buf); */
	
	return 1;
}
