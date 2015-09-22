Chapter 17 - UXIX Domain Sockets
================================
1. If one were to use regular pipes (or a connection-based UNIX domain socket)
   message boundaries would not be distinguishable. To overcome this one could
   implement an application protocol to break the stream into messages i.e write
   an extra null byte in between messages.
2. passfd.c
3. extern declarations in the header do not allocate memory for the
   variables. The variables are defined in the source file.   
4. buf_args.c
5. loop.c: did not come up with any optimisations.
6. Calling stat to confirm 'name' is a socket does not help since if 'name'
   exists and is not a socket it will still cause the call to bind to fail. Also
   S_ISSOCK macro is only available if certain feature test macros are difined,
   hence this method reduces portability.
7. To send multiple fd in one call to sendmsg you could add both to the cmsghdr,
   one after the other in the data segment. Alternatively you could use sendmmsg.
   send. Did not implement.

