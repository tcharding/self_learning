UNIX Network Programming - Chapter 1
=====================================
Exercise answers - Tobin Harding.

1. term.  
2. term. `ip -s link` `ip route`  
3. daytimetcpcli-ex-1.3.c: Changing the address family causes error EAFNOSUPPROT
   to be  returned. Information on this error is contained within
   /usr/include/asm-generic/errno.h on this system (Arch Linux). 
4. daytimetcpcli-ex-1.4.c
5. daytimetcpsrv-ex-1.5.c: Counter is still 1, only one read(). This is because
   TCP is buffering the server writes and sending them all at once.

Key 
===  
term = completed at terminal
