#include "tch.h"
#include "restart.h"

#define ERROR_CHAR 'e'
#define OK_CHAR 'g'
#define REQ_PERMS (S_IRUSR | S_IWUSR | S_IWGRP | S_IWOTH)
#define SEQ_PERMS (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)

int main(int argc, char *argv[])
{
	char buf[1];
	int reqfd, seqfd;
	long seqnum = 1;

	if (argc != 3)
		err_quit("Usage: %s request_fifo sequence_fifo", argv[0]);

	if ((mkfifo(argv[1], REQ_PERMS) == -1) && (errno != EEXIST))
		err_sys("mkfifo error");

	if ((mkfifo(argv[2], SEQ_PERMS) == -1) && (errno != EEXIST)) {
		err_msg("mkfifo error");
		unlink(argv[1]);
		return 1;
	}
	if (((reqfd = open(argv[1], O_RDWR)) == -1) ||
	    ((seqfd = open(argv[2], O_RDWR)) == -1)) 
		err_sys("failed to open FIFO's");
	for ( ; ; ) {
		if (r_read(reqfd, buf, 1) == 1) {
			if ((buf[0] == OK_CHAR) &&
			    (r_write(seqfd, &seqnum, sizeof(seqnum)) == sizeof(seqnum)))
				seqnum++;
			else if (buf[0] == ERROR_CHAR)
				break;
		}
	}
	unlink(argv[0]);
	unlink(argv[1]);
	return 0;
}
