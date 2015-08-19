#ifndef _DIA_H
#define _DIA_H

struct dia {			/* dynamic integer array */
	int size;
	int num;
	int *data;
};

struct dia *dia_creat(void);	/* create new data structure */
void dia_free(struct dia *p);	/* free data structure */
int dia_add(struct dia *, int); /* add value to data structure */
void dia_print(struct dia *);	/* print data structure */

#endif	/* _DIA_H */
