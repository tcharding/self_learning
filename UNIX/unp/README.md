UNIX Network Programming
========================
- W. Richard Stevens, Bill Fenner, Andrew M. Rudoff  
(The Sockets Networking API, Volume 1, Third edition)  

*Completed exercises from the text.*

Directory Listing
-----------------
daytime
* daytime client server implementations (tcp/udp)
* gethostbyname() gethostbyaddr() examples
* getaddrinfo() examples using function calls defined in lib
* protocol independent implementations
* getnameinfo()
* alarm() SIGALRM example usage

echo_IP  
* echo server and client
* manually fill sockaddr_in struct (i.e not using getaddrinfo())
* includes implementations (udp/tcp) using fd_set and select()
* signal() example

sockopts
* examples of getting and setting socket options  

sctp
* SCTP echo client/server implementation including use of multiple streams

lib
* library built mostly from examples straight out of text, or directly from
  downloaded source code. 

### Build Instructions #
*sctp/ depends on sctp (lkstcp-tools)*  
* Recursive Makefile included in root directory.
