/* orginal file: ../lib/readline.c */ 
/* include readline */ 
#include "unp.h" 
#include "readline.h"

static ssize_t my_read_r(struct readln *rl, char *c);

ssize_t readline_r(struct readln *rl)
{
	ssize_t n, rc; 
	char c, *ptr; 
 
	ptr = rl->readp; 
	for (n = 1; n < rl->maxlen; n++) { 
		if ( (rc = my_read_r(rl, &c)) == 1) { 
			*ptr++ = c; 
			if (c == '\n') 
				break; /* newline is stored, like fgets() */ 
		} else if (rc == 0) { 
			*ptr = 0; 
			return(n - 1); /* EOF, n - 1 bytes were read */ 
		} else 
			return(-1); /* error, errno set by read() */ 
	} 
 
	*ptr = 0; /* null terminate like fgets() */ 
	return(n); 
}
/* initialise rl */
void readline_rinit(int fd, void *buf, size_t len, struct readln *rl)
{

	rl->fd = fd;
	rl->readp = buf;
	rl->maxlen = len;

	rl->rl_cnt = 0;
	rl->rl_bufp = rl->rl_buf;
}

/* optionally fill buffer and return one character */
static ssize_t my_read_r(struct readln *rl, char *c)
{
	
	if (rl->rl_cnt <= 0) { 
	again: 
		if ( (rl->rl_cnt = read(rl->fd, rl->rl_buf, sizeof(rl->rl_bufp))) < 0) { 
			if (errno == EINTR) 
				goto again; 
			return(-1); 
		} else if (rl->rl_cnt == 0) 
			return(0);
		rl->readp = rl->rl_bufp;
	} 
 
	rl->rl_cnt--; 
	*c = *rl->readp++; 
	return(1); 
} 

ssize_t Readline_r(struct readln *rl)
{
	ssize_t n; 
 
	if ( (n = readline_r(rl)) < 0) 
		err_sys("readline_r error"); 
	return(n); 	
}
