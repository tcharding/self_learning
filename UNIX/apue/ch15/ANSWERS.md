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
   
key 
---
term - completed at terminal
