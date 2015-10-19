UNIX Network Programming - Chapter 2
======================================
Exercise answers - Tobin Harding.

1. There are two calls to `printf` because `sock_ntop` uses a static buffer.
2. Yes, dg_send_recv can return 0. This occurs if sender sends datagram
   containing no data, only sequence number and timestamp.
3. dg_send_recv-ex-22.3.c
4. One method is to create a socket for each interface. (from answers)
5. Did not complete.
6. Did not complete.
