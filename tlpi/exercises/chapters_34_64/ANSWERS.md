Answers to Exercises 
====================

_chapters 34 onward_

ch34
----
1. Using killpg(getpgrp(), SIGUSR1) would cause problems if process is created
   within a pipeline since the signal would be sent to all members of the
   pipeline. Default action for SIGUSR1 is to terminate the receiving process.

   This could be avoided by creating a separate process group for all the child
   processes.

2. changepgrp.c, causes shell to terminate?

3. setsid.c

4. If the signal is unblocked at the start of the handler, i.e before restoring
   the disposition to default, then pending signals will be delivered
   immediately causing the signal handler to be called again. This would in turn
   cause the handler to raise sigstop a second time.

5. orphan.c

6. orphancatch.c

ch35
----
1. nice.c

2. rtsched.c

3. fifoloop.c

4. pipeaffinity.c

ch36
----
1. rusage.c

2. time.c

3. xsoftlimit.c, strangely calls to setrlimit succeed even with soft limit below
   current usage. Verified also that the soft limit is changed.

ch37
----
1. logger.c

ch38
----
1. Modification of the fie causes the set-UID-bit to be removed. This is a
   security feature to prevent malicious (or accidental) modification of set-UID programs.

2. sudo.c

ch39
----
1. sched_set.c

ch40
----
1. getlogin.c Writen to specification does not function correctly. getuxent()
   does not return a record matching the current device. Neither when using a
   terminal emulator or when at a second console (tty2).

2. utmpx_login.c

3. who

