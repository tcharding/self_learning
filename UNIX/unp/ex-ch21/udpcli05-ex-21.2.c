/* orginal file: ../bcast/udpcli05.c */ 
#include "unp.h" 
 
int 
main(int argc, char **argv) 
{ 
 int sockfd; 
 struct sockaddr_in servaddr, cliaddr; 
 
 if (argc != 2) 
 err_quit("usage: udpcli05 <IPaddress>"); 
 
 bzero(&servaddr, sizeof(servaddr)); 
 servaddr.sin_family = AF_INET; 
 servaddr.sin_port = htons(13); /* standard daytime server */ 
 Inet_pton(AF_INET, argv[1], &servaddr.sin_addr); 
 
 bzero(&cliaddr, sizeof(cliaddr)); 
 servaddr.sin_family = AF_INET; 
 servaddr.sin_port = htons(0); /* standard daytime server */ 
 Inet_pton(AF_INET, "224.0.0.1", &cliaddr.sin_addr); 
 
 sockfd = Socket(AF_INET, SOCK_DGRAM, 0); 
 Bind(sockfd, (SA *) &cliaddr, sizeof(cliaddr)); 
 dg_cli(stdin, sockfd, (SA *) &servaddr, sizeof(servaddr)); 
 
 exit(0); 
} 
