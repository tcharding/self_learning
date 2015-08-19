#include <stdio.h>

int any(char *s, char *t)
{
	int i;

	for (i = 0; s[i] != '\0'; i++)
		if (contains(t, s[i]))
			return i;
	return -1;
}
