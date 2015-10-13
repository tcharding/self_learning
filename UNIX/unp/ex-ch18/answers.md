UNIX Network Programming - Chapter 18
======================================
Exercise answers - Tobin Harding.

1. 64 bit address (8 bytes) plus 5 character name totals 13 bytes of data
   (sdl_data[]). We would therefore expect sdl_len member to be 21, 13 plus the size
   of the struct members (8 bytes), total 21 bytes. Name is not
   null-terminated. This is rounded up to 24 bytes (%4 bytes 32-bit
   architecture, %8 on 64-bit). 
2. Disabling loopback causes kernel no to respond. (from answers).
