#include <stdio.h>
#include <string.h>

enum { NO, YES };

int strend(char *s, char *t);

int main(void){
	char *s = "this string ends in the word string";
	char *yes = "string";
	char *no = "not this";

	if ((strend(s, yes)) != YES)
		fprintf(stderr, "strend(s, yes) failed\n");
	if ((strend(s, no)) != NO)
		fprintf(stderr, "strend(s, no) failed\n");

	return 0;
}

/* th_strcat: concatenate t to end of s */
int strend(char *s, char *t)
{
	
	s += (strlen(s) - strlen(t));
	while (*s++ == *t++) 
		if (*t == '\0')
			return YES;

	return NO;
}

