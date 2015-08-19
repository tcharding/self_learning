Chapter 1 
=========
1. term 
2. 2 other processes started during between successive executions, hence the
   second pid is 854 after the first was 851.
3. In the function `char *strerror(int errnum)` errnum is passed by value
   therefore it is not declared with const. However, in `void perror(const char
   *msg)`, msg is passed by reference. The const qualifier hints states that the
   pointer referenced by msg is not changed by the call to perror.
4. Calendar time is stored as number of seconds since the epoch, 00:00:00
   January 1970. If this is implemented as a 32 bit signed integer then it will
   overflow 2e31 seconds after this time. This is x years after the epoch where
   > x = 2e31 / seconds in a year
   >   = 2e31 / 60 * 60 * 24 * 365  (ignoring leap years)
   >   = 68
   Overflow in the year 2038. This can be extended by using 64 bits to store the
   time. Yes this is compatible with current systems since major systems today
   are 64 bit.
5. If process time is stored as a 32 bit signed integer it will overflow 2e31
   seconds after it starts. This is x days where
   > x = 2e31 / seconds in a day
   >   = 2.31 / 60 * 60 * 24
   >   = 24855 days
   

key 
---
term - completed at terminal
