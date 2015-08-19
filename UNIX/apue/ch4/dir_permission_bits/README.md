Directory Permissions 
=====================
Directory access is controlled in the following manner.

1. To pass through a directory (e.g to `cd` or chdir() into, or below, it) you must have
exectute permission.

2. To read the contents of a directory (e.g `ls` or opendir()) you must have
read permission.

3. To create or remove (link/unlink) files within the directory you must have
write permission.

_Example directories have permissions set as their name indicates_
