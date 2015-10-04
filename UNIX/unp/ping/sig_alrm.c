/* Authors: W. R. Stevens, B. Fenner, A. M. Rudoff */

#include	"ping.h"

void
sig_alrm(int signo)
{
	(*pr->fsend)();

	alarm(1);
	return;
}
