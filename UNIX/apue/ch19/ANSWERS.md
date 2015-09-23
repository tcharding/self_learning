Chapter 19 Pseudo Terminals 
===========================
1. After the pseudo terminal device is opened (posix_openpt()) a call is made to
   grantpt() to set the permissions on the slave device.
2. termios-winsz.c: Then run it with `pty -n ./termios-winsz`
   Alternatively run `pty -n stty -a`
3. loop.c
4. Yes you can set access modes in the child process with a call to fcntl using
   command F_GETFL, turn on mode bits as required, call fcntl again with
   F.SETFL. (underscore missing, to inhibit markdown italics). WRONG, fcntl does
   not allow the read/write access to be changed.
5. No output for answer.
6. On control-D from user process termination order is as follows pty process
   exits (if loop is implemented with fork then loop returns in parent causing
   exit and child calls exit directly). Next pty master gets eof and exits then
   pty slave does the same, lastly cat gets eof and exits.
7. script.sh
8. Out put comes from the pty slave.
9. pty-fork-fun.c prog.c: could not get ioctl to send signal to process group of
   slave. Did get slave and master running and read/write msg between prog and
   main process through master and slave.
