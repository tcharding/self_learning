UNIX Network Programming - Chapter 25
======================================
Exercise answers - Tobin Harding.

1. Yes there is a difference. The first sends one message and the OOB data byte
   points at the byte after 'b' (the end of the message). The second, two calls,
   sends two messages and the second one causes the receiving TCP to overwrite
   the OOB pointer from the first call.
2. Did not complete.
