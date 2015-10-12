UNIX Network Programming - Chapter 1
======================================
Exercise answers - Tobin Harding.

1. hostent-ex-11.1.c: On a host with more than one IP address the h_name
   returned by `gethostbyaddr` is the same for each address.
2. hostent-ex-11.2.c: Fix error in previous question, just call `gethostbynam`
   on the first address in the h_addr_list.
3. service chargen resolves to 19. No one runs this service now so request hangs
   waiting for TCP timeout.
4. No my resolver does not allow dotted decimal format, no error is
   returned. Fixing this code just means re-implementing the whole file using
   `Tcp_connect`. This is already done for us in names/daytimetcpcli.c
5. Implicitly completed during last question.
6. dgcliaddr-ex-11.6.c
7. One can always get the socket length by calling `getsockname`, the third
   argument is value-result.
8. daytimetcpcli-ex-11.8.c: flags needed are NI_NUMERICHOST | NI_NUMERICSERV
9. Error is 'Address already in use'. solution, daytimeudpsrv2-ex-11.9.c: Setting
   socket option to SO_REUSEADDR allows port stealing. Also once 'theif' server
   stops original server is able to continue serving requests. Current system
   allows use process to steal port from root process.
10. 'Trying x.x.x.x' indicates that `gethostbyname` returned that
    address. 'Connected to ...' indicates that `connect` has returned.
11. getnameinfo_timeo.c
