#ifndef USP_AMS
#define USP_AMS

int getbytes(void);
int getdone(void);
int geterror(void);
int initread(int fdread, int fdwrite, int signo, char *buf, int bufsize);
int initsignal(int signo);
int supenduntilmaybeready(void);

#endif	/* USP_AMS */
