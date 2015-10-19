UNIX Network Programming - Chapter 26
======================================
Exercise answers - Tobin Harding.

1. In the forked version each process is handling one file descriptor (ignoring 
   stdin, stderr, and stdout). Totaling 101 descriptors. In the threaded version
   all 101 descriptors are within one process, each handled by a single thread.
2. If the thread does not close its file descriptor then eventually the server
   will reach the system limit for maximum open files.
3. tcpcli01-ex-26.3.c strclithread-ex-26.3.c tcpserv01-ex-26.3.c
4. 
5. 
6. 
7. 
8. 
9. 
