#include "apue.h"

/* put terminal into raw mode and exit */
int main(void)
{
	tty_raw(STDIN_FILENO);
	tty_raw(STDOUT_FILENO);
	exit(0);
}
