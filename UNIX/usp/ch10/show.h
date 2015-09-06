#ifndef SHOW_H
#define SHOW_H

void show_init(int maxt);
void show(int traceflag, const char *msg,
	  long val1, long val2, int blockedflag);

#endif	/* SHOW_H */
