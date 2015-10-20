UNIX Network Programming - Chapter 27
======================================
Exercise answers - Tobin Harding.

1. Calling with G or g selects loose source routing or strict source routing.
2. Add one byte of padding to the end. (use an EOL)
3. `ping` creates a raw socket therefore receiving the complete IP header
   (including options). From answers.
4. 0 because it is working on stdin, non-portable coding style.
5. This call is a bug, modern compilers would catch this.

