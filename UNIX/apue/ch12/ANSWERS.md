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
6. nap.[ch] tst-nap.c


