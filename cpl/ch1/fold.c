#include <stdio.h>

/* 
 * NOTE: complete without pointers and using only putchar() and getchar() 
 */

enum {
	BUFSIZE = 8192,
	MAXLINE = 16
};

int getword(char buf[], int lim);
void putn(char buf[], int nchars);
void putstr(char buf[]);

int main(void)
{
	char buf[BUFSIZE];
	int len, avail;

	avail = MAXLINE;
	while ( (len = getword(buf, BUFSIZE)) != EOF) {
		if (len == 0)
			continue;
		if (len > avail && len < MAXLINE) {
			putchar('\n');
			avail = MAXLINE;
		}
		if (len < avail) {
			putstr(buf);
			avail -= len;
		} else {
			/* handle word longer than MAXLINE */
		}
	}
}

/* getword: get next word from stream or EOF on end of file */
int getword(char buf[], int lim)
{
	int cnt, c;

	cnt = 0;
	while ((c = getchar()) != EOF && c != ' ' && c != '\n' && c != '\t') {
		buf[cnt] = c;
		cnt++;
	}
	buf[cnt] = '\0';
	return ( (c == EOF && cnt == 0) ? EOF : cnt);
}

/* putstr: write buf to stdout */
void putstr(char buf[])
{
	int i, c;

	i = 0;
	for (i = 0; (c = buf[i]) != '\0'; i++) 
		putchar(c);
		
}

/* putn: output nchars of buf to stdout */
void put(char buf[], int nchars)
{
	int i;

	for (i = 0; i < nchars; i++) 
		putchar(buf[i]);
}
