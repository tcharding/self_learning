/* Source: UNIX Network Programming http://unpbook.com/src.html
 * modifications: added main() and included socket.h
 */
#include	"socket.h"
#include	<netinet/tcp.h>		/* for TCP_xxx defines */

union val {
  int				i_val;
  long				l_val;
  struct linger		linger_val;
  struct timeval	timeval_val;
} val;

static char	*sock_str_flag(union val *, int);
static char	*sock_str_int(union val *, int);
static char	*sock_str_linger(union val *, int);
static char	*sock_str_timeval(union val *, int);

struct sock_opts {
  const char	   *opt_str;
  int		opt_level;
  int		opt_name;
  char   *(*opt_val_str)(union val *, int);
} sock_opts[] = {
	{ "SO_RCVBUF",			SOL_SOCKET,	SO_RCVBUF,		sock_str_int },
	{ "SO_SNDBUF",			SOL_SOCKET,	SO_SNDBUF,		sock_str_int },
	{ "SO_RCVBUF",			IPPROTO_IPV6,	SO_RCVBUF,		sock_str_int },
	{ "SO_SNDBUF",			IPPROTO_IPV6,	SO_SNDBUF,		sock_str_int },
	{ "SO_RCVBUF",			IPPROTO_SCTP,	SO_RCVBUF,		sock_str_int },
	{ "SO_SNDBUF",			IPPROTO_SCTP,	SO_SNDBUF,		sock_str_int },
	{ NULL,					0,			0,				NULL }
};

int
main(int argc, char **argv)
{
	int					fd;
	socklen_t			len;
	struct sock_opts	*ptr;

	for (ptr = sock_opts; ptr->opt_str != NULL; ptr++) {
		printf("%s: ", ptr->opt_str);
		if (ptr->opt_val_str == NULL)
			printf("(undefined)\n");
		else {
			switch(ptr->opt_level) {
			case SOL_SOCKET:
			case IPPROTO_IP:
			case IPPROTO_TCP:
				fd = Socket(AF_INET, SOCK_STREAM, 0);
				break;
#ifdef	IPV6
			case IPPROTO_IPV6:
				fd = Socket(AF_INET6, SOCK_STREAM, 0);
				break;
#endif
#ifdef	IPPROTO_SCTP
			case IPPROTO_SCTP:
				fd = Socket(AF_INET, SOCK_SEQPACKET, IPPROTO_SCTP);
				break;
#endif
			default:
				err_quit("Can't create fd for level %d\n", ptr->opt_level);
			}

			len = sizeof(val);
			if (getsockopt(fd, ptr->opt_level, ptr->opt_name,
						   &val, &len) == -1) {
				err_ret("getsockopt error");
			} else {
				printf("default = %s\n", (*ptr->opt_val_str)(&val, len));
			}
			close(fd);
		}
	}
	exit(0);
}
/* end checkopts2 */

/* include checkopts3 */
static char	strres[128];

static char	*
sock_str_flag(union val *ptr, int len)
{
/* *INDENT-OFF* */
	if (len != sizeof(int))
		snprintf(strres, sizeof(strres), "size (%d) not sizeof(int)", len);
	else
		snprintf(strres, sizeof(strres),
				 "%s", (ptr->i_val == 0) ? "off" : "on");
	return(strres);
/* *INDENT-ON* */
}
/* end checkopts3 */

static char	*
sock_str_int(union val *ptr, int len)
{
	if (len != sizeof(int))
		snprintf(strres, sizeof(strres), "size (%d) not sizeof(int)", len);
	else
		snprintf(strres, sizeof(strres), "%d", ptr->i_val);
	return(strres);
}

static char	*
sock_str_linger(union val *ptr, int len)
{
	struct linger	*lptr = &ptr->linger_val;

	if (len != sizeof(struct linger))
		snprintf(strres, sizeof(strres),
				 "size (%d) not sizeof(struct linger)", len);
	else
		snprintf(strres, sizeof(strres), "l_onoff = %d, l_linger = %d",
				 lptr->l_onoff, lptr->l_linger);
	return(strres);
}

static char	*
sock_str_timeval(union val *ptr, int len)
{
	struct timeval	*tvptr = &ptr->timeval_val;

	if (len != sizeof(struct timeval))
		snprintf(strres, sizeof(strres),
				 "size (%d) not sizeof(struct timeval)", len);
	else
		snprintf(strres, sizeof(strres), "%d sec, %d usec",
				 tvptr->tv_sec, tvptr->tv_usec);
	return(strres);
}
