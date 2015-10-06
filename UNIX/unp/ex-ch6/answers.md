UNIX Network Programming - Chapter 6
=====================================
Exercise answers - Tobin Harding.

1. The fdset is implemented as an array within a struct, this allows assignment
   in C.
2. Write operations always block if the whole write cannot be completed. If
   select indicates that a socket is ready for writing and we try to write more
   bytes that available in the socket send buffer the call to write will
   block. Therefore sockets must be set to non-blocking if we wish to use them
   in select write sets.
3. Both descriptors may be ready to read, if we use `else` in the check then we
   will miss one of the descriptors if both are ready. 
4. On this system (Arch Linux 4.2.1), FD_ SETSIZE is define as 1024. This is also
   the current soft system limit (RLIMIT_NOFILE). Hard limit is 4096 however we
   cannot increase the soft limit since calls to the FD* macros are undefined
   for values above FD _SETSIZE.  
5. If client calls `pause` server continues to send packets. If client calls
   `shutdown` then `sleep` server continues to send packets indefinitely. If the
   client closes the socket (explicitly with `close` or implicitly by exiting)
   then the server stops sending packets.
6. An application would call `shutdown` with SHUT_RDWR if it wanted to force the
   socket to be "closed". If another process may have the socket open then
   `close` would not achieve this.
7. When a client sends RST `select` returns. FD_ISSET returns true for the
   socket and then `read` returns 0 indicating end of file.
8. 

Key 
===  
