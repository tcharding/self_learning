#include "tch.h"
#include "tokenring.h"
#include <stdarg.h>
#include <stdbool.h>

/*
 * Interface Message Processor, token ring implementation
 */

enum { FALSE, TRUE };

enum msg_type {
	TOKEN, HOST2HOST, IMP2HOST, HOST2IMP, IMP2IMP
};

enum msg_status {
	NONE, NEW, ACK, ERR,
};

struct msg {
	enum msg_type type;    
	enum msg_status status;
	int sid;		/* source IMP ID */
	int did;		/* destination IMP ID */
	int num;		/* message number */
};

/* static prototypes */
static int init_host(struct ring *r);
static int host(void);
static int imp(int nid, struct ring net);

static ssize_t read_msg(int fd, struct msg *mbuf);
static ssize_t write_msg(int fd, struct msg mbuf);
static int write_token(int fd);

static/*@null@*/char *format_msg(struct msg mbuf);
static/*@null@*/char *format_status(enum msg_status s);
static/*@null@*/char *format_type(enum msg_type t);

static void err_pid_sys(int nid, char *fmt, ...);
static void err_pid_msg(int nid, char *fmt, ...);
static void err_pid_doit(int nid, int errnoflag, int error,
			 const char *fmt, va_list ap);
/* create ring of n processes */
int main(int argc, char *argv[])
{
	int nproc;
	struct ring r;
	pid_t pid;
	int i, status;
	int nid;		/* node ID */

	status = nproc = pid = 0;
	bzero(&r, sizeof(r));
	if (argc != 2 || ((nproc = atoi(argv[1])) <= 0))
		err_quit("Uasge: %s processes", argv[0]);


	nid = 0;		/* initial node */
	if (tr_init(&r) == -1)	/* initial ring */
		err_sys("tr_init error");
				/* create ring */
	for (i = 1; i < nproc; i++) {
		Pipe(r.fd);
		if ((pid = Fork()) > 0) {
			if (tr_dup_out(&r) == -1)
				err_sys("tr_dup_out error");
		} else {	/* child */
			if (tr_dup_in(&r) == -1)
				err_sys("tr_dup_in error");
			nid = i;
		}
		if (tr_close_fd(&r) == -1)
			err_sys("tr_close error");
		if (pid > 0)
			break;
	}
	if (imp(nid, r) == -1)
		err_sys("imp %d error", nid);

	if (nid != nproc-1)
		if (wait(&status) == -1)
			err_sys("wait error");
	return 0;
}
/* host: do host work */
static int host(void)
{
	return -1;
}
/* imp: do IMP work */
static int imp(int nid, struct ring net)
{
	struct ring host;	/* communication ring to host */
	struct msg hostmsg, netmsg;
	int maxfd1;
	fd_set readset;
	int retval;
	int got_msg;

	got_msg = FALSE;
	bzero(&hostmsg, sizeof(struct msg));
	bzero(&netmsg, sizeof(struct msg));
	bzero(&host, sizeof(struct ring));
	
	if (init_host(&host) == -1) 
		err_pid_sys(nid, "imp: host_init error");
	if ((net.in < 0 || net.in >= FD_SETSIZE ) ||
	    (host.in < 0 || host.in >= FD_SETSIZE )) 
		err_pid_sys(nid, "imp: fd invalid");
	maxfd1 = max(host.in, net.in) + 1;

	for ( ; ; ) {		/* IMP loop: does not return unless error */
		FD_ZERO(&readset);
		FD_SET(host.in, &readset);
		FD_SET(net.in, &readset);
		if ((retval = select(maxfd1, &readset, NULL, NULL, NULL)) == -1) {
			if (errno == EINTR)
				continue;
			else
				err_pid_sys(nid, "select error");
		}
		if(FD_ISSET(host.in, &readset)) { /* host msg ready */
			if (read_msg(host.in, &hostmsg) == -1)
				err_pid_sys(nid, "imp: host read msg error");
			if (got_msg == TRUE) {
				hostmsg.status = ERR;
				if (write_msg(host.out, hostmsg) == -1)
					err_pid_sys(nid, "imp: host write error");
			} else {
				got_msg = TRUE;
			}
		}

		if (FD_ISSET(net.in, &readset)) { /* got msg from network */ 
			if (read_msg(net.in, &netmsg) == -1)
				err_pid_sys(nid, "imp: net read msg error");
			switch (netmsg.type) {
			case TOKEN:
				if (got_msg == TRUE) { /* write host msg */
					if (write_msg(net.out, hostmsg) == -1)
						err_pid_sys(nid, "imp: net write error");
				} else { /* pass on token */
					if (write_msg(net.out, netmsg) == -1)
						err_pid_sys(nid, "imp: net write error");
				}
				break;
			default: 
				if (netmsg.sid == nid) { /* We sent this */
					if (netmsg.status == ACK) {
						err_pid_msg(nid, "ack'ed %s\n",
							    format_msg(netmsg));
						got_msg = FALSE;
					} else {
						err_pid_msg(nid, "not ack'ed %s\n",
							    format_msg(netmsg));
					}
					if (write_token(net.out) == -1)
						err_quit("write_token failed");
				} else if (netmsg.did == nid) { /* it's for us */
					err_pid_msg(nid, "received msg %s\n",
						    format_msg(netmsg));
					if ((netmsg.type == IMP2HOST) ||
					    (netmsg.type == HOST2HOST))
						if (write_msg(host.out, netmsg) == -1)
							err_pid_sys(nid, "imp: host write error");
					
					netmsg.status = ACK;
					if (write_msg(net.out, netmsg) == -1)
						err_pid_sys(nid, "imp: net write error");
				} else { /* just pass it on */
					if (write_msg(net.out, netmsg) == -1)
						err_pid_sys(nid, "imp: net write error");
				}
			}
		}
	}	   /* end of loop: for ( ; ; ) */
	return -1;		/* should not get here */
}
/* init_host: fork host and create communication ring */
static int init_host(struct ring *r)
{
	pid_t pid;

	if (tr_init(r) == -1)
		err_sys("tr_init error");
				/* creat host process */
	Pipe(r->fd);
	if ((pid = Fork()) > 0) {
		if (tr_dup_out(r) == -1)
			err_sys("tr_dup_out error");
	} else {	/* child */
		if (tr_dup_in(r) == -1)
			err_sys("tr_dup_in error");
	}
	if (tr_close_fd(r) == -1)
		err_sys("tr_close error");

	if (pid == 0) {		/* host (child) */
		if (host() == -1) /* should not return */
			err_sys("host() termination error");
	}
	/* IMP (parent) returns */
	return 0;
}
/* read_msg: read fd and fill mbuf */
static ssize_t read_msg(int fd, struct msg *mbuf)
{
	/* unsure how to handle interrupted reads from pipe? */
	ssize_t retval;

	retval = read(fd, mbuf, sizeof(struct msg));
	if ((retval == -1) && (errno == EINTR))
		err_msg("read_msg: read was interrupted");
	return retval;
}
/* write_msg: write m to fd */
static ssize_t write_msg(int fd, struct msg m)
{
	/* unsure how to handle interrupted writes to pipe? */
	ssize_t retval;
	FILE *fp;
	char *buf;
	size_t size;

	bzero(&buf, sizeof(char **));
	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");
	fprintf(fp, "%d %d %d %d %d", m.type, m.status, m.sid, m.did, m.num);
	if (fclose(fp) == -1)
		err_sys("fclose error");

	retval = write(fd, buf, strlen(buf)+1); /* +1 for null terminator */
	if ((retval == -1) && (errno == EINTR))
		err_msg("write_msg: write was interrupted");
	free(buf);
	return retval;
	
}
/* write_token: create token an write to fd */
static int write_token(int fd)
{
	struct msg t;

	t.type = TOKEN;
	t.status = NEW;
	t.sid = 0;
	t.did = 0;
	t.num = 0;
	if (write_msg(fd, t) == -1) {
		err_msg("write_token failed");
		return -1;
	}
	return 0;
}
/* dump_msg: write m to stderr */
static char *format_msg(struct msg m)
{
	FILE *fp;
	char *buf;
	size_t size;
	char *status;
	char *type;

	if ((status = format_status(m.status)) == NULL) {
		err_msg("dump_msg: msg_status error");
		return NULL;
	}
	if ((type = format_type(m.type)) == NULL) {
		err_msg("dump_msg: msg_type error");
		free(status);
		return NULL;
	}
	bzero(&buf, sizeof(char **));
	if ((fp = open_memstream(&buf, &size)) == NULL) {
		err_ret("open_memstream error");
		free(status);
		free(type);
		return NULL;
	}
	
	fprintf(fp, "[num:%d did:%d sid:%d status:%s type:%s]",
		m.num, m.did, m.sid, status, type);

	free(status);
	free(type);
	return buf;
}
/* msg_status: return string containing message status, must be free'd */
static char *format_status(enum msg_status s)
{
	FILE *fp;
	char *buf;
	size_t size;

	bzero(&buf, sizeof(char **));
	if ((fp = open_memstream(&buf, &size)) == NULL) {
		err_ret("open_memstream error");
		return NULL;
	}
	
	switch (s) {
	case NONE:
		fprintf(fp, "NONE");
		break;
	case NEW:
		fprintf(fp, "NEW");
		break;
	case ACK:
		fprintf(fp, "ACK");
		break;
	case ERR:
		fprintf(fp, "ERR");
		break;
	default:
		fprintf(fp, "error: unknown status enum");
		break;
	}
	if (fclose(fp) == -1)
		err_msg("fclose memstream error");
	return buf;
}
/* msg_type: return string containing message type, must be free'd */
static char *format_type(enum msg_type t)
{
	FILE *fp;
	char *buf;
	size_t size;

	bzero(&buf, sizeof(char **));
	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");
	
	switch (t) {
	case TOKEN:
		fprintf(fp, "TOKEN");
		break;
	case HOST2HOST:
		fprintf(fp, "HOST2HOST");
		break;
	case IMP2HOST:
		fprintf(fp, "IMP2HOST");
		break;
	case HOST2IMP:
		fprintf(fp, "HOST2IMP");
		break;
	case IMP2IMP:
		fprintf(fp, "IMP2IMP");
		break;
	default:
		fprintf(fp, "error: unknown status enum");
		break;
	}
	if (fclose(fp) == -1)
		err_msg("fclose memstream error");
	return buf;
}

/* err_pid_sys: print pid info, fmt message and exit */
static void err_pid_sys(int nid, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	err_pid_doit(nid, 1, errno, fmt, ap);
	va_end(ap);
	exit(EXIT_FAILURE);
}
/* err_pid_msg: print pid info, fmt message and return */
static void err_pid_msg(int nid, char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	err_pid_doit(nid, 0, 0, fmt, ap);
	va_end(ap);
	return;
}
/* err_pid_doit: print process info and a message
   caller specifies 'errnoflag' */
static void err_pid_doit(int nid, int errnoflag, int error,
			 const char *fmt, va_list ap)
{
	FILE *fp;
	char *buf;
	size_t size;

	buf = NULL;
	if ((fp = open_memstream(&buf, &size)) == NULL)
		err_sys("open_memstream error");
	fprintf(fp, "[nid:%d pid:%ld ppid:%ld]",
		nid, (long)getpid(), (long)getppid());
	(void)vfprintf(fp, fmt, ap);
	if (errnoflag == 1)
		fprintf(fp, "%s", strerror(error));
	(void)fprintf(fp, "\n");
	(void)fclose(fp);
	(void)fflush(stdout);		/* in case stdout and stderr are the same */
	(void)fputs(buf, stderr);
	(void)fflush(NULL);		/* flushes all stdio output streams */
	free(buf);		/* free memstream resource */
}
