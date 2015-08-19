Chapter 8
=========
1. vfork.c
2. stack_smash.c
3. waitid.c
4. Output is intermingled because the shell waits for the parent then runs the
   process again, starting another 'parent'. To correct this the parent needs to
   wait for the child to complete before exiting.
5. exec.c echoarg.c: argv[2] is still the absolute path because execlp searches
   $PATH and creates it when the executable is found.
6. zombie.c
7. close-on-exec.c, could not complete. Where is definition of struct DIR? It is
   not in dirent.h, that only typedefs it to __dirstream?

