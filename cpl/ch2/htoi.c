#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/*
 * convert hex string into int val
 */



int dtoi(char c);

int main(int argc, char *argv[])
{
	char *hex;
	unsigned long val;
	
	if (argc == 1) {
		printf("Usage: %s <hex_string>\n", argv[0]);
		return 0;
	}
	hex = argv[1];
	if (*hex == '0')
		hex += 2;	/* +2 for '0X' */
/*
	if (strlen(hex) > 16) {
		printf("Error: hex string too long, max 16 digits\n");
		return 1;
	}
*/
	val = 0;
	while (*hex) {
		val = val * 16 + dtoi(*hex);
		++hex;
	}
	printf("%ld\n", val);
	return 0;
}

/* dtoi: convert hex digit to int val */
int dtoi(char c)
{
	const char hex[] = "0123456789ABCDEF";
	char d;
	int i;

	d = toupper(c);
	for (i = 0; i < 16; i++)
		if (d == hex[i])
			return i;
	return -1;		/* not hex digit */

}
