Chapter 12 Thread Control
=========================

1. If redirected output is duplicated, this is because output (stdout) is
   buffered and the child process gets a copy of this buffer which is then
   output as well as the parent process outputting the buffer.
2. env_r.c
3. No you cannot make getenv_r async safe because it calls malloc, malloc is not
   async safe.   
4. Did not complete: no FreeBSD system available.
5. You still would use fork() to if you needed to exec(). You may wish to fork
   before creating threads i.e a client/server where server forks to handle each
   connection may have use for multiple threads.
6. nap.[ch] tst-nap.c, did not complete. This question is basically the same as
   the 'UNIX Systems Programming' chapter 10 project already completed. 
7. No, you cannot call pthread_cond_destroy in a child process because this call
   is undefined if there are threads waiting on the condition. The child has no
   way of knowing if this is the case or not.
8. One way of simplifying the timeout function would be to pass in the timespec
   as a relative time instead of absolute.   

