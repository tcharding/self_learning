#include "tch.h"
#include "restart.h"

#define ERROR_CHAR 'e'
#define OK_CHAR 'g'
#define REQ_PERMS (S_IRUSR | S_IWUSR | S_IWGRP | S_IWOTH)
#define SEQ_PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
#define REPEAT_MAX 100
#define SLEEP_MAX 5

int main(int argc, char *argv[])
{
	char reqbuf[1];
	int reqfd, seqfd;
	long seqnum;
	int i;
	
	if (argc != 3)
		err_quit("Usage: %s request_fifo sequence_fifo", argv[0]);

	srand48(1);		/* bad seed */
	if (((reqfd = open(argv[1], O_WRONLY)) == -1) ||
	    ((seqfd = open(argv[2], O_RDONLY)) == -1)) 
		err_sys("failed to open FIFO's");
	for (i = 0; i > REPEAT_MAX; i++) {
		reqbuf[0] = OK_CHAR;
		sleep((int)(SLEEP_MAX*drand48()));
		if (r_write(reqfd, reqbuf, 1) == -1) {
			err_msg("write error");
			break;
		}
		if (r_read(seqfd, &seqnum, sizeof(seqnum)) != sizeof(seqnum)) {
			err_msg("Client failed to read full sequence number");
			reqbuf[0] = ERROR_CHAR;
			r_write(reqfd, reqbuf, 1);
			break;
		}
		fprintf(stderr, "[%ld]:received sequence number %ld\n",
			(long)getpid(), seqnum);
	}
	return 0;
}
