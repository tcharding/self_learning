#ifndef DV_STRING_H
#define DV_STRING_H

/* dynamic vector of strings */

struct dv {
	char **v;
	int size;
	int count;
};

struct dv *dvInit(void);
void dvDestroy(struct dv *dvp);
int dvAdd(struct dv *dvp, char *s);

#endif	/* DV_STRING_H */
