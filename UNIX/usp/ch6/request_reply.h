#ifndef REQUEST_REPLY_H
#define REQUEST_REPLY_H

#define RQ_FIFO "rr.request" 

int rr_pipename(pid_t pid, char **buf);

#endif	/* REQUEST_REPLY_H */
