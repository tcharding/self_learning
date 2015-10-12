/* orginal file: ../advio/str_echo_stdio02.c */ 
#include "unp.h" 
 
void str_echo_stdlib(int sockfd) 
{ 
	char line[MAXLINE]; 
	FILE *fpin, *fpout; 
 
	fpin = Fdopen(sockfd, "r"); 
	fpout = Fdopen(sockfd, "w"); 
				/* option 1 */
	/* if (setvbuf(fpout, NULL, _IOLBF, 0) < 0) */
	/* 	err_sys("setvbuf error"); */
	
	while (Fgets(line, MAXLINE, fpin) != NULL) {
		Fputs(line, fpout);
				/* option 2 */
		/* fflush(fpout); */
	}
}
