UNIX Network Programming - Chapter 14
======================================
Exercise answers - Tobin Harding.

1. If a process has not yet established a signal handler then resetting the
   signal handler will restore the default disposition for the signal.
2. connect-timeo-ex-14.2.c
3. daytimetcpcli-ex-14.3.c 
4. If we 'fall off' main (do not call exit or return), main returns to the C
   startup routine (as if we had called return explicitly). This routine calls
   exit so standard IO buffers are flushed. [apue section 7.3]
5. str_echo_stdio02-ex-14.5.c
