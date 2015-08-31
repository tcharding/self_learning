#include "apue.h"

/* test multi_alarm */
int main(void)
{
	
}

static unsigned int next;

/* multi_alarm: add multiple alarm capability to alarm(3P) */
unsigned int multi_alarm(unsigned int new)
{
	unsigned int set;

	set = alarm(new);
	if (set == 0 || set == new)
		return new;
	if (set > new) {
		next = set - new;
		return new;
	} else if (set < new) {
		next = new - set;
		alarm(set);
		return set;
	}
	return 0;		/* shouldn't get here */
}
