UNIX Network Programming - Chapter 4
=====================================
Exercise answers - Tobin Harding.

1. Since INADDR_* are defined as constants in the header netinet/in.h, they must
   be stored in host byte order. 
2. daytimetcpcli-ex-4.2.c: ephemeral ports are within the range 50000 - MAX (MAX
   is 2 to the power of 16).
3. Given the order of execution (child completes before fork returns to parent)
   the 'first' close (child) causes the file table entry count for the socket to
   be decremented by one, bringing it from 2 to 1. The second call to close
   (parent) decrements the count again from 1 to 0 causing the kernel to release
   the resources associated with the open socket.
4. daytimetcpsrv1-ex-4.4.c: If we remove the call to listen (i.e the socket is
   not converted to passive), the call to accept fails, error code 22 (EINVAL),
   Invalid argument. man accept(3) states EINVAL to indicate 'The socket is not
   accepting connections'.
5. daytimetcpsrv1-ex-4.5.c: Removing the call to bind causes the kernel to
   allocate an ephemeral port to the server, when the client tries to connect we
   get 'connection refused' error indicating that the host is running but no
   process is listening for connections on the specified port (as we would
   expect since the ephemeral port is unlikely to have the port number specified
   in the client).
  
