#ifndef TOKENRING_H
#define TOKENRING_H

enum { IN, OUT };

struct ring {
	int in;			/* read file descriptor */
	int out;		/* write file descriptor */
	int fd[2];
};

int tr_init(struct ring *);
int tr_dup_in(struct ring *);
int tr_dup_out(struct ring *);
int tr_close_fd(struct ring *);

#endif	/* TOKENRING_H */
