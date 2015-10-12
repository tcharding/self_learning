UNIX Network Programming - Chapter 12
======================================
Exercise answers - Tobin Harding.

1. ipv[46]tcpsrv.c: Attempting to start IPv6 server (no socket option set) when
   IPv4 server is running results in 'Address already in use' error. Setting
   SO_REUSEADDR has no effect. Whichever server process is started first gets
   exclusive access to the interface.
