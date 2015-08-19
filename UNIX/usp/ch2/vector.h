#ifndef _DV_H
#define _DV_H

/* 
 * Abstract Data Type (dynamic string storage)
 */

struct dv {
	int size;
	int cnt;
	char **v;
};

struct dv *creatdv(void);
void freedv(struct dv *p);
int adds(struct dv *p, const char *s);
int foreach(struct dv *p, int (*fnc)(char *));
char **dupv(struct dv *p);
void freev(char **v);

#endif	/* _DV */
