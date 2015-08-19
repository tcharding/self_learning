#ifndef _ADT_H
#define _ADT_H

struct wc {			/* word/count pair */
	char *word;
	int count;
};

/* Abstract Data Type */
struct adt {			/* dynamic wc array */
	int size;
	int num;
	struct wc **data;	/* array of pointers */
};

struct adt *adt_creat(void);		/* create new data structure */
void adt_free(struct adt *);		/* free data structure */
int adt_add(struct adt *, char *word, int cnt); 
void adt_print(struct adt *);		/* print data structure */
void adt_sort(struct adt *);		/* sort the data structer */

#endif	/* _ADT_H */

