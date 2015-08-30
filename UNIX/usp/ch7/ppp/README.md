7.9 Pipelined Pre-processor
==========================
Use pipeline of processes to transform infile to outfile. Transformations
supplied in config file. One transformation done by one process.

Main 
----
ppp.c  
processchar.c  
parse-config.c  

_build with `make dev`_

Tests 
-----
tst-string.c  
tst-data.c  
tst-vector.c  
tst-processchar.c  
tst-parse-config.c  

_build with `make tests`_
