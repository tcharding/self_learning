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
5.    

key 
---
term - completed at terminal
