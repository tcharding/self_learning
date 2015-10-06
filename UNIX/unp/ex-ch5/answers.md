UNIX Network Programming - Chapter 5
=====================================
Exercise answers - Tobin Harding.

1. MSL is somewhere less that 2 minutes on this system (Linux 4.2.1)
2. Passing a binary file causes the client to hang. Tracing the code we get,
   client: fgets() returns some amount of data (binary, fgets() returns on eof or
   '\n'), writen() writes this data to socket. Server: read() returns after
   reading data from socket correctly then writes data correctly via
   writen(). Client: readline() calls myread() calls read(2) which reads all data
   correctly but then is blocks since myread() calls read() in a loop
   terminating on error or '\n'. Original fgets() (binary data, must have read
   up to eof (-1)) leaving read() blocked waiting on more data from the server
   and the server blocked waiting for data from the client.
3. The only noticeable difference is that telnet appends the character '\r'
   before the newline, this does not affect the server however since it echos
   every character irrespective of the character.
4. The final two segments are not sent, the client never closes the socket since
   it is blocked in read(). However when the client process is terminated the
   kernel will send the final FIN, and the server will respond with the final
   ACK (assuming it is still running).
5. The server will respond with a RST since the client/server connection is no
   longer valid. i.e the socket was lost on reboot.
