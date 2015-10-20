#ifndef READLINE_H
#define READLINE_H

struct readln { 
	int fd;		   /* caller's descriptor to read from */ 
	char *readp;	   /* caller's buffer to read into */ 
	size_t maxlen;	   /* max #bytes to read */ 
				/* next three are used internally by the function */ 
	int rl_cnt;		/* initialize to 0 */ 
	char *rl_bufp;		/* initialize to rl_buf */ 
	char rl_buf[MAXLINE]; 
}; 
 
void readline_rinit(int fd, void *buf, size_t maxlen, struct readln *rl); 
ssize_t readline_r(struct readln *rl); 

#endif	/* READLINE_H */
