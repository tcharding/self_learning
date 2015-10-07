UNIX Network Programming - Chapter 7
=====================================
Exercise answers - Tobin Harding.

1. buff-opts.c
2. daytimetcpcli-ex-7.2.c
3. Yes after connect the receive buffer size and maximum segment size have
   changed. The server sends it's maximum segment size with the initial ACK
   during TCP three way handshake. The client has then updated its MSS and
   receive buffer size.
4. Race condition exists between first clients call to `bind` and `connect`, `bind`
   returns error to second client. After first client `connect` returns then
   `bind` succeeds in second client.
5. term: cannot have multiple servers bound to localhost and 0.0.0.0 even if
   SO_REUSEADDR option is passed (neither with re-use SO _REUSEPORT).
6. With UDP however I was able to have a server listening on 127.0.0.1:9999 and
   one listening on 0.0.0.0:9999
7. According to `man(8) ping` -d (SO_DEBUG) option is not used by the Linux kernel.
8. Nagle algorithm prevents TCP from writing data from second write to the
   network, delayed ACK's causes server to wait for the delay to expire before
   sending ACK. Client TCP then sends data from second write. Nagle combined
   with delayed ACK's adds RTT + delay  to the request/response.
9. When TCP_NODELAY option is set, server sends ACK immediately reducing total
   time by 100ms (ACK delay).
10. Using writev client sends only one TCP segment and the server responds
	immediately (after server response time). 
11. RCF states maximum delay to be 500ms.
12. The echo server spends most of its time blocked on `read` (excluding the
    process blocked on `accept`). If SO_KEEPALIVE option is set and a connected
    peer host crashes (and does no reboot) the TCP probe packet, sent after 2
    hours of idle time, will timeout i.e TCP will send probe packets repeatedly
    (useing back off algorithm) until the maximum time is reached. The servers
    socket will have its pending error set to ETIMEDOUT.
13. The echo client spends its time blocked on `fgets` i.e input from stdin, and
	blocked on `read`  of socket i.e input from server. If the connected server host
    crashes and does not reboot, assuming some data becomes available from
	stdin, `writen` will return in client. However TCP will not receive an ACK
	for this segment and will try to re-send eventually setting pending error on
	the socket to ETIMEDOUT. The client does not check for errors pending so
	will block on the next call to `read` indefinetly.
14. The echo client (using `select`) spends its time blocked on `select`. Given that
	the server host crashes as in the above example if data arrives from stdin
	(`fgets` returns, server blocks on `select`) TCP will eventually close the
	socket with pending error set to ETIMEDOUT (as above). This will cause `select`
	to return with the socket ready for reading. Client `read` will then return
	eof.
	If no data is available from stdin, with SO_KEEPALIVE option set, client
	will probe peer after 2 hours. This will lead to the same set of events as
	above, `select` returns after exponential backoff algorithm
	completes. Pending error is set to ETIMEDOUT and `read` returns end of file.
15. Give host/client both with SO_KEEPALIVE option set and no data flow. Two TCP
    segments will be sent every two hours, one probe and one ACK by each
    host. This will result in 4 segments only if both hosts try to probe each
    other within half the RTT. If the timeouts do not occur within this time
    then only one peer need probe the other because the probe will re-activate
    the connection for both.
16. Could not locate text file at given address.
