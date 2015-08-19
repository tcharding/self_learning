Chapter 5
=========
1. ch5/setbuf.c
2. Setting buffer size to 4 causes more time to be used by the user process
   since it loops many more times. One loop every 4 characters instead of one loop
   every line (or MAXLINE) characters. System time remains the same since IO is
   buffered the same number of system calls occur.
3. Return value of 0 from printf(3) means successfully wrote zero length string.
4. If char is implemented as an unsigned type then comparing it to EOF
   (negative, usually -1) will not behave as expected. Exact behaviour will depend on
   the character set being used.
5. To use fsync() with a file pointer you would call fileno() to first get the
   file descriptor. 
6. Call to exit() flushes all streams.
7. Relates to BSD, did not complete.   

