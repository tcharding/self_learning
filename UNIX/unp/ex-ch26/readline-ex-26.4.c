/* orginal file: ../lib/readline.c */ 
/* include readline */ 
#include "unp.h" 
typedef struct { 
	int read_fd;		/* caller's descriptor to read from */ 
	char *read_ptr;		/* caller's buffer to read into */ 
	size_t read_maxlen;	/* max #bytes to read */ 
				/* next three are used internally by the function */ 
	int rl_cnt; /* initialize to 0 */ 
	char *rl_bufptr; /* initialize to rl_buf */ 
	char rl_buf[MAXLINE]; 
} Rline;

static ssize_t my_read_r(Rline *rl);

ssize_t readline_r(Rline *rl)
{
	ssize_t n, rc; 
	char c, *ptr; 
 
	ptr = rl->read_ptr; 
	for (n = 1; n < rl->read_maxlen; n++) { 
		if ( (rl->rl_cnt = my_read_r(rl, &c)) == 1) { 
			*ptr++ = c; 
			if (c == '\n') 
				break; /* newline is stored, like fgets() */ 
		} else if (rl->cl_cnt == 0) { 
			*ptr = 0; 
			return(n - 1); /* EOF, n - 1 bytes were read */ 
		} else 
			return(-1); /* error, errno set by read() */ 
	} 
 
	*ptr = 0; /* null terminate like fgets() */ 
	return(n); 
}

void readline_rinit(int fd, void *buf, size_t len, Rline *rl)
{
	rl->read_fd = fd;
	rl->read_ptr = buf;
	rl->read_maxlen = len;

	rl->rl_cnt = 0;
	rl->rl_bufptr = rl->rl_buf;
}

static ssize_t my_read_r(Rline *rl)
{ 
 
	if (rl->rl_cnt <= 0) { 
	again: 
		if ( (rl->rl_cnt = read(rl->readfd, rl->rl_buf,
					sizeof(rl->rl_buf))) < 0) { 
			if (errno == EINTR) 
				goto again; 
			return(-1); 
		} else if (rl->rl_cnt == 0) 
			return(0); 
		rl->rl_bufptr = rl->rl_buf; 
	} 
 
	rl->rl_cnt--; 
	*rl->read_ptr = *rl->rl_bufptr++; 
	return(1); 
} 

ssize_t Readline_r(Rline *rl)
{
	ssize_t n; 
 
	if ( (n = readline_r(rl)) < 0) 
		err_sys("readline_r error"); 
	return(n); 	
}
