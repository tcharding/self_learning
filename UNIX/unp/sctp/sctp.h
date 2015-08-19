/*
 * Sctp Header
 * 
 * attribution: UNIX Network Programming 
 *   Volume 1 Third Edition, W. R. Stevens, B Fenner, A M. Rudoff
 */
#ifndef _SCTP_H
#define _SCTP_H

int sctp_get_no_strms(int sock_fd,struct sockaddr *to, socklen_t tolen);
sctp_assoc_t sctp_address_to_associd(int sock_fd, struct sockaddr *sa, socklen_t salen);
void sctpstr_cli(FILE *fp, int sock_fd, struct sockaddr *to, socklen_t tolen);

/* sctp wrapper functions */

int Sctp_recvmsg(int s, void *msg, size_t len,
		 struct sockaddr *from, socklen_t *fromlen,
		 struct sctp_sndrcvinfo *sinfo,
		 int *msg_flags);


int Sctp_sendmsg (int s, void *data, size_t len, struct sockaddr *to,
		  socklen_t tolen, uint32_t ppid, uint32_t flags,
		  uint16_t stream_no, uint32_t timetolive, uint32_t context);

int Sctp_bindx(int sock_fd,struct sockaddr_storage *at,int num,int op);

#endif /* _SCTP_H */
