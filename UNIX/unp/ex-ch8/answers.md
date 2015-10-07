UNIX Network Programming - Chapter 8
=====================================
Exercise answers - Tobin Harding.

1. Yes, UDP is message orientated while TCP is stream orientated. Therefore
   recvfrom returns maximum one UDP message on each call (`read` on connected
   socket, `recvfrom` on default socket)  whereas TCP returns all data in the
   receive buffer on each call to `read`. 
2. len may have been changed by the call to `recvfrom` since in is a
   value-result argument. Replacing len with clilen may cause an error if the
   lengths of the peer address is less than the length passed in clilen. 
3. term
4. An ICMP error is returned to UDP but it is not passed to the client.
5. Data is not accepted for a listenin TCP socket.
6. did not complete, `sock` example.
7. did not complete, multiple hosts required.
8. Amount of data that can be sent in an UDP datagram is limited by the length
   field in the header (16 bits), it is therefore 2^16. Length includes the IP
   header (20 bytes) and the UDP header (8 bytes). Payload is therefore 2^16 -
   28*8 or 65508 (see overflow-udp.c). In IPv6 the length field is again 16 bits
   however this is the payload length, giving UDP an extra 20 bytes of
   data. Also IPv6 has an option for large packets so UDP can take advantage of
   this to send packets much larger, however UDP is not typically used for bulk
   data transfer since it is an un-reliable transfer protocol.
9. IP_RECVDSTADDR is not defined on this system (Arch Linux). If it was one
   would call `setsockopt` with a non-zero value to turn it on.

Key 
===  
term = completed at terminal
