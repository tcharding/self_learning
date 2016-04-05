Exercises from Text
===================
_The Linux Programming Environment_

Completed by Tobin Harding, March/April 2016

Some exercises required modifying code from the text book source tree. As such
not all files under this directory are wholly written by Tobin. Files copied
contain original copyright notice and licence.

Ego stroking line count can be obtained using the following command;

   $ wc $(find . -name *.[ch] -exec grep -L 'Kerrisk' {} \;)
