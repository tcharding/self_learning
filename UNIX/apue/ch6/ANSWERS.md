Chapter 6
=========
1. To access shadow password entries one uses the [get|set|end]spent()
   functions. These operate on struct spwd pointers.
2. spent.c
3. utsname.c
4. time_t is long int on my system. Latest time then that can be represented is
   2 to the power of 64 seconds apter the Epoch, 1970-01-01 00:00:00 +0000
   (UTC). When this wraps around we will all be dead so I don't suppose it
   matters. Those alive however will need to check their code.
5. time.c

key 
---
term - completed at terminal
