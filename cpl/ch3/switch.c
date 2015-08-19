#include <stdio.h>

enum { MAXLINE = 2048 };

void re_scape(char dst[], char src[]);
void escape(char dst[], char src[]);

int main(void)
{
	char readline[MAXLINE], buf[MAXLINE*2];
	char re[MAXLINE];
	
	while ((fgets(readline, MAXLINE, stdin)) != NULL) {
		escape(buf, readline);
		printf("escaped: %s\n", buf);
		re_scape(re, buf);
		printf("re_scaped: %s\n", re);
	}
	return 0;
}
/* escape: convert non-printable characters into escape sequences */
void escape(char dst[], char src[])
{
	int i;			/* src */
	int j;			/* dst */
	
	for (i = 0, j = 0; src[i] != '\0'; i++, j++) {
		switch (src[i]) {
		case '\n':
			dst[j++] = '\\';
			dst[j] = 'n';
			break;
		case '\t':
			dst[j++] = '\\';
			dst[j] = 't';
			break;
		case '\\':
			dst[j++] = '\\';
			dst[j] = '\\';
			break;
		default:
			dst[j] = src[i];
			break;
		}
	}
	dst[j]= '\0';
}

/* re_scape: convert escape sequences to ASCII */
void re_scape(char dst[], char src[])
{
	int i;			/* src */
	int j;			/* dst */

	for (i = 0, j = 0; src[i] != '\0'; i++, j++) {
		if (src[i] == '\\') {
			i++;
			switch (src[i]) {
			case 'n':
				dst[j] = '\n';
				break;
			case 't':
				dst[j] = '\t';
				break;
			case '\\':
				dst[j] = '\\';
				break;
			default:
				fprintf(stderr, "Error: ill formatted input: %s\n", src);
				return;
			}
		} else
			dst[j] = src[i];
	}
	dst[j]= '\0';
}
