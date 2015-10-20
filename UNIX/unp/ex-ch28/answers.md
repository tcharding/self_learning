UNIX Network Programming - Chapter 28
======================================
Exercise answers - Tobin Harding.

1. Non-available parts of the IPv6 header are; version number, next header
   field, jumbo payload option (to an application) and fragment header. From answers.
2. The call to `write` will block indefinitely once the socket receive buffer
   becomes full. This can be avoided by setting the socket to non blocking
   (`setsockopt`) and ignoring the error EWOULDBLOCK.
3. The kernel does not check socket option on a raw socket for
   broadcasting. (From answer: Berkley derived kernels allow broadcast on raw
   socket). 
4. Since we don't set multicast socket option the kernel selects the out going
   interface. From answers.
