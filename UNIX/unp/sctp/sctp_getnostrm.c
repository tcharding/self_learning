/* Authors: W. R. Stevens, B. Fenner, A. M. Rudoff */

#include	"unp.h"

int sctp_get_no_strms(int sock_fd,struct sockaddr *to, socklen_t tolen)
{
	socklen_t sz;
	struct sctp_paddrparams sp;
	struct sctp_status status;
	
	bzero(&status, sizeof(status));
	status.sstat_assoc_id = sctp_address_to_associd(sock_fd, to, tolen);
	/* Getsockopt(sock_fd, IPPROTO_SCTP, SCTP_STATUS, &status, &retsz); */
	sz = sizeof(struct sctp_paddrparams);	
	bzero(&sp, sizeof(sp));
	sctp_opt_info(sock_fd, 0, SCTP_STATUS, &sp, &sz);
	return (status.sstat_outstrms);
}
