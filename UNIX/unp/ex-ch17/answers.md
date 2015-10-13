UNIX Network Programming - Chapter 17
======================================
Exercise answers - Tobin Harding.

1. No it does not matter because it is a union and both are sockaddr structs.
2. get_ifi_info-ex-17.2.c: kernel returns three structs, address families are; 0
   ( unspecified ), 2 (AF_INET), and 54 (could not find definition). However
   `ioctl/prifinfo` only prints the loop back address ?
3. Did not complete, couldn't compare two addresses to see if on the same subnet.
4. get_ifi_info-ex-17.4.c
