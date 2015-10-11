UNIX Network Programming - Chapter 10
======================================
Exercise answers - Tobin Harding.

1. If the client in figure 10.4 receives an error, the SCTP stack will notify it
   through the msg pointer argument. However this argument is not checked, the fix
   is clearly then to check the msg argument after the call to `sctp_recvmsg`
   returns. 
2. Turn on message event notifications or do a timed read.
3. 800 was chosen in an attempt to fit the message into one packet, better to
   get the SCTP_MAXSEG size via a call to `getsockopt`.
4. The Nagle algorithm affects SCTP in a similar manner to TCP except it tries
   to coalesce chunks instead of packets.
   sctpsrv01.c, sctpcli01.c: also modified library routines in order for build
   to succeed (and Makefile).
5. All socket options must be set before 'using' a socket (calling `listen` or
   `connect`). If not behaviour will not take effect.
6. Only one-to-many-style sockets can modify the number of streams because
   association is created implicitly when a message is first sent ancillary data
   must be sent at this time to effect socket options, only one-to-many sockets
   can piggyback this data so it arrives in time to create the association.
7. An SCTP server does not track the state of each association because a cookie
   is passed with each message. This has the danger that an attacker could
   misuse eavesdropped cookies however cryptographic signatures are used to
   combat this.
8. Weather or not this is a good design decision depends on the expected
   behavior of clients. If a client is expected to only need to send one message
   then this may be a good design, if however, multiple messages are likely then
   this design will be inefficient since the four-way-handshake will need to be
   carried out for each message.
