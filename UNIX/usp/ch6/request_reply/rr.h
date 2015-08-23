#ifndef REQUEST_REPLY_H
#define REQUEST_REPLY_H

#define RQ_FIFO "rr.request" 
#define PIPE_PERMS (S_IRUSR | S_IWUSR | S_IWGRP | S_IWOTH)

int rr_pipe_name(pid_t pid, char **buf);
void pprint_hdr(void);
void pprint_pid(pid_t child, char *fmt, ...);

#endif	/* REQUEST_REPLY_H */
