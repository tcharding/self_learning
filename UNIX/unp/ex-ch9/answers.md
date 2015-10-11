UNIX Network Programming - Chapter 9
=====================================
Exercise answers - Tobin Harding.

1. An application programmer would most likely use the `sctp_peeloff` function
   when porting existing TCP applications to SCTP.
2. Since SCTP does not support half close, then once all data is written an
   association that receives a shutdown msg (close) can automatically close.
3. A server must call `sctp_recvgmsg` (instead of `accept`) to enable
   piggybacking data on the third leg of the four-way handshake. This implicitly
   states that the association is one-to-many.
4. Data may be piggybacked on the third and fourth packet of the SCTP handshake
   in an application that did not need to send many packets (perhaps just one
   each way) but required some of the features SCTP offers over TCP.
5. If some of the addresses bound to a socket are not suitable for an
   association then you may get a subset i.e if some are IPv6 addresses but the
   peer only supports IPv4.
