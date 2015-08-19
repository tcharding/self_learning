#include <stdio.h>

enum { BUFSIZE = 1024 };

void th_strcat(char *s, char *t);

int main(void){
	char buf[BUFSIZE];
	char *s = "start ";
	char *t = "| end ";

	buf[0] = '\0';
	th_strcat(buf, s);
	printf("after s: %s\n", buf);
	th_strcat(buf, t);
	printf("after t: %s\n", buf);
	return 0;
}

/* th_strcat: concatenate t to end of s */
void th_strcat(char *s, char *t)
{
	while (*s)
		s++;
	while ((*s++ = *t++))
		;
}

