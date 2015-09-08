Chapter 11 - Cracking Shells
============================

All sections completed except 11.7 (job control). Reasoning: why have job
control in a shell when modern OS's are capable of having multiple terminal
windows running at once.

File list
---------
BNF.md - Backus-Naur Form shell grammar  
ush.h - project header  
main.c - main shell loop  
exec.c - command execution functions  
input.c - user input and parsing functions  
tst-parse.c - executable to run unit tests (implemented in input.c)

#### Usage: #
_build with_ `make` _then run_ `$ ush`
