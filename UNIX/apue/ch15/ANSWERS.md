Chapter 
=========
1. Removing the close causes the child to block on read. i.e never receives EOF
2. Removing the wait causes the shell to return immediately, outputting the
   prompt before the child has completed its output. 
3. If popen(3) is passed a non existent command it passes this to the shell. The
   shell outputs an error msg (command not found). However popen does not
   return an error, a later read from the stream returns NULL i.e end of file or
   error. ferror() does not report an error.
4. The shell returns an error code (141).
5. coprocess.c
6. If waitpid is not available and popen must use wait then the signal raised by
   termination of the child process created by a call to system will interfere with
   the original call to popen.
7.
* select-close-write.c:
   Once the write end of the pipe is close calls to select return that the pipe
   is ready to read, and reads get 0 bytes signifying end of file.

* poll-close-write.c:
   Poll behaves in a similar manner, successive calls to poll return fd ready
   and reads return 0.
* select-close-read.c:
   select returns fd ready, write to pipe (closed already) causes signal
   SIGPIPE.
* poll-close-write.c:
   poll returns fd ready, however the revents member of the pollfd struct does
   not register an event. BUG: Did not manage to terminate loop since poll keeps
   returning 1.
8. Writes to stderr are displayed on stderr (the terminal) even though stdout is
   piped into the calling process. This behaviour is the exact reason for having
   stderr and stdout seperate.
9. When cmdstring argument terminates it returns its exit code to the shell
   (invoked by popen) via wait(). The shell process then terminates returning as
   it's exit code the code received. This value is returned by pclose via the
   wait() call on the shell child process, pclose returns -1 with errno set to
   ECHILD if it cannot get the shell's exit code.
10. To open a FIFO as full duplex you could use two threads, one for reading one
    for writing.
11. A process need only know the key and call getmsg() to access a message
    que. And have access permissions.
12. msgque.c: Each message que ID is 2^15 bigger than the previous (signed 16 bit?).
13. To build a linked list in shared memory you could define a node structure
    then shmget (or mmap /dev/zero) a chunk of memory of size 'node'. The link
    pointer would be of type void *.
14. paper   
15. counter-XSIshmem.c
16. did not complete: skipped XSI semaphores
17. counter-reclock.c, as noted previously, you cannot guarantee which process
	will run first with advisory record locks, hence return value of update is
	unknown. 
18. counter-POSIXsem.c: 


key 
---
paper - completed on paper
