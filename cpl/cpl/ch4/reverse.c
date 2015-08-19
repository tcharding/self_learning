#include <stdio.h>
#include <string.h>

enum { MAXLINE = 1024 };

int mygetline(char s[], int lim);
void reverse(char s[]);

int main(void)
{
	char readln[MAXLINE];
	
	while (mygetline(readln, MAXLINE) > 0) {
		reverse(readln);
		fprintf(stdout, "%s\n", readln);
	}
	return 0;
}

/* r_reverse: recursive helper */
void r_reverse(char s[], int left, int right)
{
	int tmp;
	
	if (left >= right)
		return;
	tmp = s[left];
	s[left] = s[right];
	s[right] = tmp;
	r_reverse(s, left+1, right-1);
}
/* reverse: reverse s in place */
void reverse(char s[])
{
	r_reverse(s, 0, strlen(s)-1);
}
/* mygetline: get line into s, return length */
int mygetline(char s[], int lim)
{
	int c, i;

	i = 0;
	while (--lim > 0 && (c = getchar()) != EOF && c != '\n')
		s[i++] = c;
	if (c == '\n')
		s[i++] = c;
	s[i] = '\0';
	return i;
}
