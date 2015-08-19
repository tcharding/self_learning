/*
 * Standard Header - to be included before all standard system headers.
 * 
 * attribution: UNIX Network Programming 
 *   Volume 1 Third Edition, W. R. Stevens, B Fenner, A M. Rudoff
 */
#ifndef _SOCKET_H
#define _SOCKET_H

#define _POSIX_C_SOURCE 200809L

#include <stdio.h>		/* for convenience */
#include <stdlib.h>		/* for convenience */
#include <string.h>		/* for convenience */
#include <unistd.h>		/* for convenience */

#include <sys/types.h>		/* basic system data types */
#include <error.h>		/* for perror */
#include <errno.h>

#include <netdb.h>
#include <sys/socket.h>		/* basic socket definitions */
#include <netinet/in.h>		/* sockaddr_in and other Intrenet definitions */
#include <arpa/inet.h>
#include <sys/un.h>		/* for Unix domain sockets */
#include <fcntl.h>		/* for non-blocking */

#include <signal.h>
#include <sys/select.h>		/* for select / pselect */
#include <time.h>		/* timespec{} for pselect */
#include <sys/time.h>		/* timeval{} for select */
#include <arpa/inet.h>		/* inet {3} functions */
#include <sys/ioctl.h>
#include <sys/wait.h>


enum {
    LISTENQ = 1024,		/* second argument to listen() */
    SERV_PORT = 9877,		/* TCP and UDP */
    BUFFSIZE = 8192,		/* buffer size for reads and writes */
    MAXLINE = 4096		/* max text line length */
};

/* unsafe versions */
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

/* compiler errors on these ones */
#define max(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })

#define min(a,b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })

#define SERV_PORT_STR "9877"	/* TCP and UDP */

/* Define bzero() as macro */
#define bzero(ptr, n) memset(ptr, 0, (size_t)(n))

#define SA struct sockaddr	/* only used in casts (SA *) */
typedef void Sigfunc(int);	/* simplify signal prototype */

/* error funtion prototypes (error.c) */
void err_ret(const char *fmt, ...); 
void err_sys(const char *fmt, ...) __attribute__((noreturn));
void err_dump(const char *fmt, ...) __attribute__((noreturn));
void err_msg(const char *fmt, ...);
void err_quit(const char *fmt, ...) __attribute__((noreturn));

/* prototypes for our own library functions */
Sigfunc *signal(int signo, Sigfunc *func);
int sock_bind_wild(int sockfd, int family);
int sock_cmp_addr(const SA *, const SA *, socklen_t addrlen);
int sock_cmp_port(const SA *, const SA *, socklen_t addrlen);
int sock_get_port(const SA *sockaddr, socklen_t addrlen);
void sock_set_addr(SA *sockaddr, socklen_t addrlen, const void *ptr); 
void sock_set_port(SA *sockaddr, socklen_t addrlen, const int port);
void sock_set_wild(SA *sockaddr, socklen_t addrlen);
struct addrinfo *host_serv(const char *hostname, const char *service,
			   int family, int socktype);
char *sock_ntop(const struct sockaddr *sockaddr, socklen_t addrlen);
char *sock_ntop_host(const struct sockaddr *sockaddr, socklen_t addrlen);
int tcp_connect(const char *host, const char *serv);
int tcp_listen(const char *host, const char *serv, socklen_t *lenp); 
int udp_client(const char *host, const char *serv, SA **saptr, socklen_t *lenp);
int udp_connect(const char *host, const char *serv,
		struct sockaddr **saptr, socklen_t *lenp);
int udp_server(const char *host, const char *serv, socklen_t *lenp);

/* prototypes for our own library  wrapper functions */
Sigfunc *Signal(int, Sigfunc *);
int Sock_bind_wild(int, int);
char *Sock_ntop(const SA *sa, socklen_t salen);
char *Sock_ntop_host(const SA *sa, socklen_t salen);
struct addrinfo *Host_serv(const char *hostname, const char *service,
			   int family, int socktype);
int Tcp_connect(const char *host, const char *serv);
int Tcp_listen(const char *host, const char *serv, socklen_t *lenp); 
int Udp_client(const char *host, const char *serv, SA **saptr, socklen_t *lenp);
int Udp_connect(const char *host, const char *serv,
		struct sockaddr **saptr, socklen_t *lenp);
int Udp_server(const char *host, const char *serv, socklen_t *lenp);

/*
int	Sockfd_to_family(int);
void	 Connect_timeo(int, const SA *, socklen_t, int);
int		 Family_to_level(int);
char			*If_indextoname(unsigned int, char *);
unsigned int		 If_nametoindex(const char *);
struct if_nameindex	*If_nameindex(void);
char   **My_addrs(int *);
ssize_t	 Read_fd(int, void *, size_t, int *);
int		 Readable_timeo(int, int);
ssize_t	 Recvfrom_flags(int, void *, size_t, int *, SA *, socklen_t *,
		 struct unp_in_pktinfo *);
Sigfunc *Signal_intr(int, Sigfunc *);
ssize_t	 Write_fd(int, void *, size_t, int);
int		 Writable_timeo(int, int);
*/

/* prototypes for our Unix wrapper functions (lib/wrapunix.c) */
void	*Calloc(size_t, size_t);
void	 Close(int);
void	 Dup2(int, int);
int		 Fcntl(int, int, int);
void	 Gettimeofday(struct timeval *, void *);
int		 Ioctl(int, int, void *);
pid_t	 Fork(void);
void	*Malloc(size_t);
int	     Mkstemp(char *);
void	*Mmap(void *, size_t, int, int, int, off_t);
int		 Open(const char *, int, mode_t);
void	 Pipe(int *fds);
ssize_t	 Read(int, void *, size_t);
/*
void	 Sigaddset(sigset_t *, int);
void	 Sigdelset(sigset_t *, int);
void	 Sigemptyset(sigset_t *);
void	 Sigfillset(sigset_t *);
int		 Sigismember(const sigset_t *, int);
void	 Sigpending(sigset_t *);
void	 Sigprocmask(int, const sigset_t *, sigset_t *);
*/
char	*Strdup(const char *);
long	 Sysconf(int);
void	 Sysctl(int *, uint32_t, void *, size_t *, void *, size_t);
void	 Unlink(const char *);
pid_t	 Wait(int *);
pid_t	 Waitpid(pid_t, int *, int);
void	 Write(int, void *, size_t);

/* prototypes for our stdio wrapper functions (wrapstio.c) */
void	 Fclose(FILE *);
FILE	*Fdopen(int, const char *);
char	*Fgets(char *, int, FILE *);
FILE	*Fopen(const char *, const char *);
void	 Fputs(const char *, FILE *);

/* prototypes for our socket wrapper functions (wrapsock.c) */
int Accept(int, SA *, socklen_t *);
void Bind(int, const SA *, socklen_t);
void Connect(int, const SA *, socklen_t);
void Getpeername(int, SA *, socklen_t *);
void Getsockname(int, SA *, socklen_t *);
void Getsockopt(int, int, int, void *, socklen_t *);
const char *Inet_ntop(int, const void *, char *, size_t);
void Inet_pton(int, const char *, void *);
void Listen(int, int);
ssize_t Readline(int, void *, size_t);
ssize_t Readn(int, void *, size_t);
ssize_t Recv(int, void *, size_t, int);
ssize_t Recvfrom(int, void *, size_t, int, SA *, socklen_t *);
ssize_t Recvmsg(int, struct msghdr *, int);
int Select(int, fd_set *, fd_set *, fd_set *, struct timeval *);
void Send(int, const void *, size_t, int);
void Sendto(int, const void *, size_t, int, const SA *, socklen_t);
void Sendmsg(int, const struct msghdr *, int);
void Setsockopt(int, int, int, const void *, socklen_t);
void Shutdown(int, int);
int Sockatmark(int);
int Socket(int, int, int);
void Socketpair(int, int, int, int *);
void Writen(int, void *, size_t);

#endif	/* _SOCKET_H */
