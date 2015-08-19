#include "socket.h"
#include <netinet/sctp.h>
#include "sctp.h"

int 
sctp_get_no_strms(int sock_fd, struct sockaddr *to, socklen_t tolen)
{
	int retsz;
	struct sctp_status status;
	retsz = sizeof(status);	
	bzero(&status,sizeof(status));

/*	status.sstat_assoc_id = sctp_address_to_associd(sock_fd, to, tolen); */
	status.sstat_assoc_id = sri.sinfo_assoc_id;
	Getsockopt(sock_fd, IPPROTO_SCTP, SCTP_STATUS,
		   &status, (socklen_t *)&retsz);
	return(status.sstat_outstrms);
}
