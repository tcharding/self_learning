#include "apue.h"

/* determine system endianess (big or little) */
int main(void)
{
	int val;
	char byte;
	
	val = 0x04030201;
	byte = (char)val;
	if (byte == (char)0x01)
		printf("System is little-endian\n");
	else
		printf("System is big-endian\n");
	return 0;
}
