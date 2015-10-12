UNIX Network Programming - Chapter 13
======================================
Exercise answers - Tobin Harding.

1. If `daemon_init` is called before checking the command line arguments any
   error is written to /dev/null since err_quit writes to stdout and
   `daemon_init` opens /dev/null on stdin, stout, and stderr.
2. Only `chargen` (TCP and UDP) would require a `fork`, all the other 8 services
   could be handled in an iterative manner (assuming most often connections will
   require only one request/response cycle).
3. `bind` will fail unless SO_REUSEADDR option is set. If it is set server
   (`chargen`) will respond. When the client exits if an echo server is running
   it will echo the generate characters back to `chargen`, what ensues is a well
   known Denial Of Service attack as `chargen` and echo server loop sending
   characters to each other. It is for this reason that `chargen` is almost
   always disabled.
4. IP address and port are obtained by calling `getpeername` on the socket. Also
   they are contained in the return from `accept`.
