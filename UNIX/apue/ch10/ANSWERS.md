Chapter 10 
==========
1. Removing the infinite loop causes the program to exit after catching a
   signal, i.e after catching exactly one signal.
2. sig2string.c
3. paper
4. On a busy system the alarm may go off before set_jmp is called, the result
   would be hard to predict - it would depend on what value was in the jump buffer.
5. multi_alarm.c
6. sync.c
7. Second call to kill because SIGABRT terminates a process in a different
   manner to exit, i.e it dumps core. Also the disposition is reset to default
   to ensure that the signal is not blocked or ignored.
8. ? No idea why a process would need to know the uid or effective uid of the
   signals sending process?
9. prmask.c. Does not compile, implementation relies on sigset_t being of type
   long long but compiler complains that it is non-scalar. /usr/include/bits
   defines it as 'unsigned long'?
10. longsleep.c. Sleep creeps a bit over 2 seconds each 5 minutes. Correct this
    by using nanosleep with an absolute time.
11. readwrite.c: unexplained behaviour, setting rlimit in the program does not
	cause sigxfsz to be raised, however setting it in the shell (ulimit 2) does.
12. fwrite.c; fwrite does not get interrupted by SIGALRM. The kernel is
 restarting it.

key 
---
paper - done with pen and paper
