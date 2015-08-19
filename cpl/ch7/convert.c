#include <stdio.h>
#include <ctype.h>
#include <string.h>

/* convert to uppercase or lowercase */
int main(int argc, char *argv[])
{
	int c;

	while ((c = getchar()) != EOF) {
		if (strstr(argv[0], "toupper") != NULL)
			putchar(toupper(c));
		else
			putchar(tolower(c));
	}
	return 0;
}
