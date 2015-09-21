#ifndef BUFFER_H
#define BUFFER_H

#define BUFSIZE 8
#define PATH_MAX 1024

enum { FALSE, TRUE };

typedef struct {
	int infd;
	int outfd;
	char file[PATH_MAX];
} buffer_t;

int getitem(buffer_t *itemp);
int putitem(buffer_t item);
int getdone(int *flag);
int setdone(void);

#endif	/* BUFFER_H */
