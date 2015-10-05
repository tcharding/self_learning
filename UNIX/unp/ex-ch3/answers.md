UNIX Network Programming - Chapter 3
=====================================
Exercise answers - Tobin Harding.

1. Value-result arguments must be passed by reference in order that a result may
   be assigned to them. You cannot assign a result to a pass by value argument.
2. readn and writen both cast the void * pointer to a char * pointer in order to
   do pointer arithmetic and move the pointer along the number of bytes read.
3. inet_pton_loose.c
