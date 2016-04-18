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

ch41
----
1. No output

ch42
----
1. ../sharedlibs/*

2. dynload.c

ch43
----

TODO: come back to these exercises after chapter 61

ch44
----
1. twinpipe.c

2. popen.c

3. fifo_seqnum_server.c

4. fifo_seqnum_server.c

5. This approach would cause the following read to get EOF again if no client
   had accessed it, i.e it would achieve nothing.

6. Set open flags to NONBLOCK, if client misbehaves server will simply ignore
   and continue.

7. nonblockpipe.c. Result of test code; pipes behave as expected  O_NONBLOCK
   flag causes operations on pipe to that would block to return immediately with
   EAGAIN. 

ch45
----
1. ftok.c verify()

2. ftok.c my_ftok() verify()

3. getcall.c

ch46
----
1. No output

2. sysv_mqid_seqnum_srv/cli

3. 0 is a valid message queue identifier but 0 cannot be used as a message type
   (from answers).

4. svmsg_file_server.c

5. svmsg_file_client.c

6. D not complete.
