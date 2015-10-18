UNIX Network Programming - Chapter 20
======================================
Exercise answers - Tobin Harding.

1. Zero replies received. This is as expected, one host on subnet not listening
   on port 13.
2. `select` returns -1 and EINTR, we call `continue` and `selecte` again and it
   returns 1 with the pipe readable.
3. Broadcast messages belong to UDP (17).
