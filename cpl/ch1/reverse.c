#include <stdio.h>

enum { MAXLINE = 4096 };

void copy(char to[], char from[]); 
void reverse(char line[]);
int mygetline(char s[], int maxline);

/* reverse input line by line  */
int main(void)
{
	char line[MAXLINE];
	int len;
	
	while ( (len = mygetline(line, MAXLINE)) != EOF) {
		if (len != 0) {
			if (line[len-1] == '\n')
				line[len-1] = '\0';
			reverse(line);
		}
		printf("%s", line);
	}
	return 0;
}

/* reverse: reverse s in place */
void reverse(char s[])
{
	char tmp[MAXLINE];
	int i, cnt;

	cnt = 0;
	copy(tmp, s);
	while ( s[cnt] != '\0') 
		++cnt;

	for (i = 0 ; i < cnt; i++) 
		s[i] = tmp[cnt-1-i];
}
			
			
/* copy: copy 'from' into 'to'; assume to is big enough */
void copy(char to[], char from[])
{
	int i;

	i = 0;
	while ( (to[i] = from[i]) != '\0')
		++i;
}

/* mygetline: read a line into s, strip trailing whitespace, return length */
int mygetline(char s[], int lim)
{
	int c, i;

	for (i = 0; i < lim-1 && (c = getchar()) != EOF && c != '\n'; ++i)
		s[i] = c;

	if (c == EOF && i == 0)
		return EOF;

	if (c == '\n') {
		s[i] = c;
		++i;
	}

	s[i] = '\0';
	return i;
}
