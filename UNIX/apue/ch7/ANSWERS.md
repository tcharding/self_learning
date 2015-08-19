Chapter 7
=========
1. On my system the return value is 0 (including a compiler warning). One guess
   as to why it returned 13 is that this is the value returned by the last
   statement (printf) before the program finishes.
2. The output from the printf's is output just prior to the process terminating
   at which time buffers are flushed.
3. If one knew how many arguments were passed into main then a function called
   from main could manipulate the address of one of its automatic arguments to
   go back up the stack (to a lower address) and access argv and argc. If one
   did not know the number of arguments passed in it would be very difficult to
   do since you would have to guess how far to move within the stack.
4. Making location 0 in the data segment not accessible allows the program to
   catch some memory errors, i.e dereferenceing a NULL pointer
5. * typedef void Exitfunc(void)
   * int atexit(Exitfunc *fp);
6. Yes, when allocating memory with calloc all values are set to zero. This
   includes pointers, i.e they are set to NULL pointers.   
7. There are no sizes for the heap and stack because the heap and stack are not
   part of an executable.
8. The disk usage of an executable is larger than the sums of the text segment,
   data segment and bss segment because the executable contains other data, i.e
   the startup routine run before main and optionally debug information.
9. Just because the program is trivial does not mean that the library calls it
   makes are trivial, the executable without shared linking must include all of these.
10. The code is incorrect because the pointer reference is not valid when it is
    de-referenced. However the code would likely still run since the memory
    location my not yet have been overwritten. 

