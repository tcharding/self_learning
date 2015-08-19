#include "socket.h"
#include <netinet/sctp.h>
#include "sctp.h"
                                                                       
int main(int argc, char *argv[])
{
	int sock_fd, msg_flags;
	char readbuf[BUFFSIZE];
	struct sockaddr_in servaddr, cliaddr;
	struct sctp_sndrcvinfo sri;
	struct sctp_event_subscribe evnts;
	int stream_increment = 1;
	socklen_t len;
	size_t rd_sz;
	int retsz, nstrms;
	struct sctp_status status;

	if (argc == 2)
		stream_increment = atoi(argv[1]);
	sock_fd = Socket(AF_INET, SOCK_SEQPACKET, IPPROTO_SCTP);
	bzero(&servaddr, sizeof(servaddr));
	servaddr.sin_family = AF_INET;
	servaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	servaddr.sin_port = htons(SERV_PORT);

	Bind(sock_fd, (SA *) &servaddr, sizeof(servaddr));

	bzero(&evnts, sizeof(evnts));
	evnts.sctp_data_io_event = 1;
	Setsockopt(sock_fd, IPPROTO_SCTP, SCTP_EVENTS, &evnts, sizeof(evnts)); 
  
	Listen(sock_fd, LISTENQ);
	for ( ; ; ) {
		len = sizeof(struct sockaddr_in);
		rd_sz = Sctp_recvmsg(sock_fd, readbuf, sizeof(readbuf),
				     (SA *) &cliaddr, &len, &sri, &msg_flags);
		if (stream_increment) {
			sri.sinfo_stream++;
/*
linux doesn't support sctp_get_no_strms function - see link
http://stackoverflow.com/questions/23897781/getsockopt-invalid-argument-for-ipproto-sctp-sctp-status

http://sourceforge.net/p/lksctp/mailman/message/12160491/

			retsz = sizeof(status);
			bzero(&status, sizeof(status));
			status.sstat_assoc_id = sri.sinfo_assoc_id;
			Getsockopt(sock_fd, IPPROTO_SCTP, SCTP_STATUS,
				   &status, (socklen_t *)&retsz);
			nstrms = status.sstat_outstrms;
			if (sri.sinfo_stream >= nstrms)
				sri.sinfo_stream = 0;
*/
		}

		Sctp_sendmsg(sock_fd, readbuf, rd_sz, (SA *) &cliaddr, len, sri.sinfo_ppid,
			     sri.sinfo_flags, sri.sinfo_stream, 0, 0);
	}
}
