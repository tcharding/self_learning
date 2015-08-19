Chapter 9
=========
1. The init process is responsible for writing logout information to wtmp
   because it is the parent process of 'login' (via getty on some systems). Once
   a user is logged out the login process terminates, this can be monitored by
   it's parent so init is the obvious choice for writing to wtmp.
2. session.c: running ./session then hitting Control C terminates the parent
   process but the child process continues. This confirms that the child does
   not have a controlling terminal.

