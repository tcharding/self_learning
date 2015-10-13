UNIX Network Programming - Chapter 15
======================================
Exercise answers - Tobin Harding.

1. Calling `unlink` with the pathname of a bound Unix domain socket will remove
   the filename from the file system. Future reads/writes to the socket will
   fail. 
2. Calls to `connect` fail with errno : 'No such file or directory' if
   the socket pathname does not exist (i.e has not been created yet). If the
   pathname exists but has not been bound by another process then errno is:
   'Connection refused'. This is the case if a server does not unlink the socket
   pathname before exit.
3. Using TCP 26 bytes are read, also TCP buffers all writes so only one data
   data segment is sent. Converting to Unix domain sockets, read is still 26
   bytes. Using `send` instead of write (with MSG_EOR flag) has no obvious
   effect, output is as it was using previous methods.
4. backlog-unix.c, backlog-tcp.c: tcp version does not function as hoped. see
   email to text authors (unp.emial.txt).
5. Completed at terminal, yes removing `bind` does cause a server error.
