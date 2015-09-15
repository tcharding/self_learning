Chapter 14 Advanced I/O
=======================
1. starve.c: successive read locks cause process waiting on write lock to be
   starved on current system  *Linux eros 4.1.6-1-ARCH x86_64 GNU/Linux*.
2. term
3. You could use multiple threads each with subset of the fd's. Once select()
   returns a thread could set a condition variable and have a single thread
   process any I/O when the condition is set. This would however be quite an
   ugly solution.
4. term/book
5. sleepus.c
6. You can implement tellwait using record locking to an extent. You cannot,
   however, have the child process run first i.e the parent cannot call
   WAIT_CHILD straight after the fork. This is because the only way to block on
   the other process is to aquire a lock, however locks are not passed over the
   fork so the child cannot be guaranteed to get a lock first since we do not
   know which process, parent or child, will run first. Hence the parent cannot
   reliably wait (aquire exclusive lock) on the child.
7. pipesize.c: PIPE_BUF: 4096 but we could write 65536 (2^16) bytes before blocking.
8. did not complete: skipped section on asyncronous I/O.
9. ?
10. memmap_copy.c: access time is not update, however, it is also not updated
	cp(1) or by opening/closing the file in emacs without an edit?
11. memmap_copy.c

key 
---
term: completed at terminal
