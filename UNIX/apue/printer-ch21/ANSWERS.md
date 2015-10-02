Chapter 21 - Communicating with a Network Printer
=================================================
1. Modified ipp.h and added log_error() to print.c
2. Added flags to print.h, modified print.c to use request flags. Modified
   printd.c to add IPP options for request flags.
3. Implemented printd request to printer but did not read printer response.
4. printd.c: get_status()
5. Added cancel_job() to printd.c and print.c: A user can cancel another users
   job if they guess the username, the server has no way of knowing which jobs
   belong to whom except by username.
6. Did not complete: add support for multiple printers.
7. We don't need to prod the main printer thread because it only wakes up when
   there is a job, and before processing each job it checks if a reread is needed.
8. Did not complete: add support for chunked transfer encoding (RFC 2616)
9. Added call to ftruncate() before writing nextjob.
