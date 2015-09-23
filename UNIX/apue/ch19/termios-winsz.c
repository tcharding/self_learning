#include "apue.h"

/* print termios and winsize struct info */
int main(void)
{
	struct termios trm;
	
	if (tcgetattr(STDIN_FILENO, &trm) < 0)
		err_sys("tcgetattr error");

	msg("Printing termios structure\n");
	msg("c_iflag: %d, c_oflag: %d, c_cflag: %d, c_lflag: %d",
	    (int)trm.c_iflag, (int)trm.c_oflag, (int)trm.c_cflag, (int)trm.c_lflag);
	exit(0);
}
