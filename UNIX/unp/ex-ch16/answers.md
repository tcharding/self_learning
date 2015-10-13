UNIX Network Programming - Chapter 16
======================================
Exercise answers - Tobin Harding.

1. We call `shutdown` instead of `close` to force a FIN to be sent. Since the
   reference count is 2, `close` will merely reduce it by one and not cause FIN
   to be sent. 
2. In the conditions described, `Writen` will return -1 (write returns error
   because socket was closed, server TCP stack sends FIN when server process
   exits). The return value of `Writen` is not checked so this error is missed,
   process then `fgets` more input. This event loop will continue, read input,
   write to socket (error on write) until no more input is available. Process
   will exit with no notification of error condition. Input will not be echoed
   though, obviously. WRONG, first write to closed socket causes RST from
   server, continued writes cause SIGPIPE to be sent to parent.
3. If the parent dies first and child gets eof on socket read then the child
   will signal (non-existan) parent and exit. FROM ANSWERS -> `getppid` returns
   1 once parent is dead (init process). This is not a problem if process is a
   user process but if it may run as root then this should be checked (killing
   init process is bad).
4. Program will still function, these lines simply avoid the unnecessary call to
   `selet` if `connect` returned immediately.
5. Data can become available before `connect` returns if the server sends data
   as soon as `accept` returns and the client is busy when the final packet of
   the three way handshake arrives.
