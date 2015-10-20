UNIX Network Programming - Chapter 30
======================================
Exercise answers - Tobin Harding.

1. The parent keeps the listening socket open in case it needs to `fork` more
   children. From answers.
2. Changing to Unix domain datagram sockets instead of Unix domain stream
   sockets involves changing the second argument of `socket` to SOCK_DGRAM
   (instead of SOCK_STREAM) and replacing calls to `read` with calls to
   `recvfrom`. Also call `sendto` instead of `write`. Alternatively the socket
   could be connected then read/write calls can remain unchanged. Also with
   datagram sockets the parent does not receive eof if the child terminates
   prematurely, however the parent can catch SIGCHLD signals to deal with this. 
3. No need to run these myself, I have complete faith in the authors ability to
   run and document the code examples.
