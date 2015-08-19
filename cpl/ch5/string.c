#include <stdio.h>

enum { BUFSIZE = 1024 };

char *th_strncpy(char *dst, char *src, int n);
char *th_strncat(char *dst, char *src, int n);
int th_strncmp(char *s1, char *s2, int n);

int main(void)
{
	char buf[BUFSIZE];
	char *s = "string-s";
	char *t = "string-t";
	int nstring = 6;
	int nall = 8;
	int res;

	buf[0] = '\0';
        th_strncat(buf, t, nall);
	printf("buf after th_strncat(buf, t): %s\n", buf);
        th_strncat(buf, s, nall);
	printf("buf after th_strncat(buf, s): %s\n", buf);

	th_strncpy(buf, s, nall);
	printf("buf after th_strncpy(buf, s): %s\n", buf);

	if ( (res = th_strncmp(s, t, nall)) >= 0)
		fprintf(stderr, "error: th_strncmp(s, t, nall) failed: %d\n", res);
	if ( (res = th_strncmp(s, t, nstring)) != 0)
		fprintf(stderr, "error: th_strncmp(s, t, nstring) failed: %d\n", res);
	if ( (res = th_strncmp(t, s, nall)) <= 0)
		fprintf(stderr, "error: th_strncmp(t, s, nall) failed: %d\n", res);

	return 0;
}
/* th_strncpy: write n bytes from src to dst */
char *th_strncpy(char *dst, char *src, int n)
{
	char *frontp = dst;

	for( ; n > 0; --n) {
		if ( (*dst++ = *src++) == '\0')
			break;	/* we are done */
	}
	*dst = '\0';		/* cat'd n bytes, add terminator */
	return frontp;
}

/* th_strncat: concatinate up to n bytes from src to dst */
char *th_strncat(char *dst, char *src, int n)
{
	char *frontp = dst;

	while(*dst)
		dst++;		/* seek to end of dst */
	for( ; n > 0; --n) {
		if ( (*dst++ = *src++) == '\0')
			break;	/* we are done */
	}
	*dst = '\0';		/* cat'd n bytes, add terminator */
	return frontp;
}

/* th_strncmp: return <0 if s<t, 0 if s==t, >0 if s>t, comparing n characters */
int th_strncmp(char *s1, char *s2, int n)
{

	for( ; n > 0; --n) {
		if ( (*s1 != *s2) || *s2 == '\0')
			break;	/* first non matching character */
		s1++, s2++;
	}
	if (n == 0)
		return 0;	/* they match */
	return *s1 - *s2;
}


