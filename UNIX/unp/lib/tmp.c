 
/* prototypes for our own library functions */ 
int connect_nonb(int, const SA *, socklen_t, int); 
int connect_timeo(int, const SA *, socklen_t, int); 
int daemon_init(const char *, int); 
void daemon_inetd(const char *, int); 
void dg_cli(FILE *, int, const SA *, socklen_t); 
void dg_echo(int, SA *, socklen_t); 
int family_to_level(int); 
char *gf_time(void); 
void heartbeat_cli(int, int, int); 
void heartbeat_serv(int, int, int); 
struct addrinfo *host_serv(const char *, const char *, int, int); 
int inet_srcrt_add(char *); 
u_char *inet_srcrt_init(int); 
void inet_srcrt_print(u_char *, int); 
void inet6_srcrt_print(void *); 
char **my_addrs(int *); 
int readable_timeo(int, int); 
ssize_t readline(int, void *, size_t); 
ssize_t readn(int, void *, size_t); 
ssize_t read_fd(int, void *, size_t, int *); 
ssize_t recvfrom_flags(int, void *, size_t, int *, SA *, socklen_t *, 
		       struct unp_in_pktinfo *); 
Sigfunc *signal_intr(int, Sigfunc *); 
int sock_bind_wild(int, int); 
int sock_cmp_addr(const SA *, const SA *, socklen_t); 
int sock_cmp_port(const SA *, const SA *, socklen_t); 
int sock_get_port(const SA *, socklen_t); 
void sock_set_addr(SA *, socklen_t, const void *); 
void sock_set_port(SA *, socklen_t, int); 
void sock_set_wild(SA *, socklen_t); 
char *sock_ntop(const SA *, socklen_t); 
char *sock_ntop_host(const SA *, socklen_t); 
int sockfd_to_family(int); 
void str_echo(int); 
void str_cli(FILE *, int); 
int tcp_connect(const char *, const char *); 
int tcp_listen(const char *, const char *, socklen_t *); 
void tv_sub(struct timeval *, struct timeval *); 
int udp_client(const char *, const char *, SA **, socklen_t *); 
int udp_connect(const char *, const char *); 
int udp_server(const char *, const char *, socklen_t *); 
int writable_timeo(int, int); 
ssize_t writen(int, const void *, size_t); 
ssize_t write_fd(int, void *, size_t, int); 
 
#ifdef MCAST 
int mcast_leave(int, const SA *, socklen_t); 
int mcast_join(int, const SA *, socklen_t, const char *, u_int); 
int mcast_leave_source_group(int sockfd, const SA *src, socklen_t srclen, 
			     const SA *grp, socklen_t grplen); 
int mcast_join_source_group(int sockfd, const SA *src, socklen_t srclen, 
			    const SA *grp, socklen_t grplen, 
			    const char *ifname, u_int ifindex); 
int mcast_block_source(int sockfd, const SA *src, socklen_t srclen, 
		       const SA *grp, socklen_t grplen); 
int mcast_unblock_source(int sockfd, const SA *src, socklen_t srclen, 
			 const SA *grp, socklen_t grplen); 
int mcast_get_if(int); 
int mcast_get_loop(int); 
int mcast_get_ttl(int); 
int mcast_set_if(int, const char *, u_int); 
int mcast_set_loop(int, int); 
int mcast_set_ttl(int, int); 
 
void Mcast_leave(int, const SA *, socklen_t); 
void Mcast_join(int, const SA *, socklen_t, const char *, u_int); 
void Mcast_leave_source_group(int sockfd, const SA *src, socklen_t srclen, 
			      const SA *grp, socklen_t grplen); 
void Mcast_join_source_group(int sockfd, const SA *src, socklen_t srclen, 
			     const SA *grp, socklen_t grplen, 
			     const char *ifname, u_int ifindex); 
void Mcast_block_source(int sockfd, const SA *src, socklen_t srclen, 
			const SA *grp, socklen_t grplen); 
void Mcast_unblock_source(int sockfd, const SA *src, socklen_t srclen, 
			  const SA *grp, socklen_t grplen); 
int Mcast_get_if(int); 
int Mcast_get_loop(int); 
int Mcast_get_ttl(int); 
void Mcast_set_if(int, const char *, u_int); 
void Mcast_set_loop(int, int); 
void Mcast_set_ttl(int, int); 
#endif 
 
uint16_t in_cksum(uint16_t *, int); 
 
#ifndef HAVE_GETADDRINFO_PROTO 
int getaddrinfo(const char *, const char *, const struct addrinfo *, 
		struct addrinfo **); 
void freeaddrinfo(struct addrinfo *); 
char *gai_strerror(int); 
#endif 
 
#ifndef HAVE_GETNAMEINFO_PROTO 
int getnameinfo(const SA *, socklen_t, char *, size_t, char *, size_t, int); 
#endif 
 
#ifndef HAVE_GETHOSTNAME_PROTO 
int gethostname(char *, int); 
#endif 
 
#ifndef HAVE_HSTRERROR_PROTO 
const char *hstrerror(int); 
#endif 
 
#ifndef HAVE_IF_NAMETOINDEX_PROTO 
unsigned int if_nametoindex(const char *); 
char *if_indextoname(unsigned int, char *); 
void if_freenameindex(struct if_nameindex *); 
struct if_nameindex *if_nameindex(void); 
#endif 
 
#ifndef HAVE_INET_PTON_PROTO 
int inet_pton(int, const char *, void *); 
const char *inet_ntop(int, const void *, char *, size_t); 
#endif 
 
#ifndef HAVE_INET_ATON_PROTO 
int inet_aton(const char *, struct in_addr *); 
#endif 
 
#ifndef HAVE_PSELECT_PROTO 
int pselect(int, fd_set *, fd_set *, fd_set *, 
	    const struct timespec *, const sigset_t *); 
#endif 
 
#ifndef HAVE_SOCKATMARK_PROTO 
int sockatmark(int); 
#endif 
 
#ifndef HAVE_SNPRINTF_PROTO 
int snprintf(char *, size_t, const char *, ...); 
#endif 
 
/* prototypes for our own library wrapper functions */ 
void Connect_timeo(int, const SA *, socklen_t, int); 
int Family_to_level(int); 
struct addrinfo *Host_serv(const char *, const char *, int, int); 
const char *Inet_ntop(int, const void *, char *, size_t); 
void Inet_pton(int, const char *, void *); 
char *If_indextoname(unsigned int, char *); 
unsigned int If_nametoindex(const char *); 
struct if_nameindex *If_nameindex(void); 
char **My_addrs(int *); 
ssize_t Read_fd(int, void *, size_t, int *); 
int Readable_timeo(int, int); 
ssize_t Recvfrom_flags(int, void *, size_t, int *, SA *, socklen_t *, 
		       struct unp_in_pktinfo *); 
Sigfunc *Signal(int, Sigfunc *); 
Sigfunc *Signal_intr(int, Sigfunc *); 
int Sock_bind_wild(int, int); 
char *Sock_ntop(const SA *, socklen_t); 
char *Sock_ntop_host(const SA *, socklen_t); 
int Sockfd_to_family(int); 
int Tcp_connect(const char *, const char *); 
int Tcp_listen(const char *, const char *, socklen_t *); 
int Udp_client(const char *, const char *, SA **, socklen_t *); 
int Udp_connect(const char *, const char *); 
int Udp_server(const char *, const char *, socklen_t *); 
ssize_t Write_fd(int, void *, size_t, int); 
int Writable_timeo(int, int); 
 
/* prototypes for our Unix wrapper functions: see {Sec errors} */ 
void *Calloc(size_t, size_t); 
void Close(int); 
void Dup2(int, int); 
int Fcntl(int, int, int); 
void Gettimeofday(struct timeval *, void *); 
int Ioctl(int, int, void *); 
pid_t Fork(void); 
void *Malloc(size_t); 
int Mkstemp(char *); 
void *Mmap(void *, size_t, int, int, int, off_t); 
int Open(const char *, int, mode_t); 
void Pipe(int *fds); 
ssize_t Read(int, void *, size_t); 
void Sigaddset(sigset_t *, int); 
void Sigdelset(sigset_t *, int); 
void Sigemptyset(sigset_t *); 
void Sigfillset(sigset_t *); 
int Sigismember(const sigset_t *, int); 
void Sigpending(sigset_t *); 
void Sigprocmask(int, const sigset_t *, sigset_t *); 
char *Strdup(const char *); 
long Sysconf(int); 
void Sysctl(int *, u_int, void *, size_t *, void *, size_t); 
void Unlink(const char *); 
pid_t Wait(int *); 
pid_t Waitpid(pid_t, int *, int); 
void Write(int, void *, size_t); 
 
/* prototypes for our stdio wrapper functions: see {Sec errors} */ 
void Fclose(FILE *); 
FILE *Fdopen(int, const char *); 
char *Fgets(char *, int, FILE *); 
FILE *Fopen(const char *, const char *); 
void Fputs(const char *, FILE *); 
 
/* prototypes for our socket wrapper functions: see {Sec errors} */ 
int Accept(int, SA *, socklen_t *); 
void Bind(int, const SA *, socklen_t); 
void Connect(int, const SA *, socklen_t); 
void Getpeername(int, SA *, socklen_t *); 
void Getsockname(int, SA *, socklen_t *); 
void Getsockopt(int, int, int, void *, socklen_t *); 
#ifdef HAVE_INET6_RTH_INIT 
int Inet6_rth_space(int, int); 
void *Inet6_rth_init(void *, socklen_t, int, int); 
void Inet6_rth_add(void *, const struct in6_addr *); 
void Inet6_rth_reverse(const void *, void *); 
int Inet6_rth_segments(const void *); 
struct in6_addr *Inet6_rth_getaddr(const void *, int); 
#endif 
#ifdef HAVE_KQUEUE 
int Kqueue(void); 
int Kevent(int, const struct kevent *, int, 
	   struct kevent *, int, const struct timespec *); 
#endif 
void Listen(int, int); 
#ifdef HAVE_POLL 
int Poll(struct pollfd *, unsigned long, int); 
#endif 
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
 
void err_dump(const char *, ...); 
void err_msg(const char *, ...); 
void err_quit(const char *, ...); 
void err_ret(const char *, ...); 
void err_sys(const char *, ...); 
 
