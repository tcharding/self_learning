#include "adt.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define _XOPEN_SOURCE 700
char *strdup(const char *s);	/* string.h */

#define ALLOC 1			/* set to 1 to test */

static struct wc *wc_creat(char *word, int count);
static void wc_free(struct wc *);
static void quicksort(struct wc *v[], int left, int right);
	
struct adt *adt_creat(void)
{
	struct adt *p;

	if ((p = malloc(sizeof(struct adt))) == NULL) {
		fprintf(stderr, "adt_creat: malloc error\n");
		return NULL;
	}
	p->size = 0;
	p->num = 0;
	p->data = NULL;
	return p;
}

void adt_free(struct adt *p)
{
	int i;

	for (i = 0; i < p->num; i++)
		wc_free(p->data[i]);
	free(p->data);
	free(p);
}

int adt_add(struct adt *ap, char *word, int cnt)
{
	int memsize;
	
	if (ap == NULL)
		return -1;
	if (ap->size == 0) {	/* first time through */
		memsize = ALLOC;
		if ((ap->data = malloc(sizeof(struct wc *) * memsize)) == NULL) {
			fprintf(stderr, "adt_add: malloc error\n");
			return NULL;
		}
		ap->size = memsize;
	} else if (ap->num >= ap->size) { /* resize */
		struct wc **temp;
		memsize = ap->size * 2;
		temp = realloc(ap->data, sizeof(struct wc *) * memsize);
		if (temp == NULL) {
			fprintf(stderr, "adt_add: realloc error\n");
			return NULL;
		} else {
			ap->data = temp;
			ap->size = memsize;
		}
	}
	ap->data[ap->num++] = wc_creat(word, cnt);
	return ap->num;
}
void adt_print(struct adt *p)
{
	int i;

	for (i = 0; i < p->num; i++)
		printf("%4d %s\n", p->data[i]->count, p->data[i]->word);
			
}

void adt_sort(struct adt *p)
{
	quicksort(p->data, 0, p->num-1);
}

/* qsort: sort v[left] .. v[right] into decreasing order */
static void quicksort(struct wc *v[], int left, int right)
{
	int i, last;
	void swap(struct wc *v[], int i, int j);

	if (left >= right)	/* nothing to do */
		return;		/* fewer than two elements */
	swap(v, left, (left + right)/2);
	last = left;
	for (i = left+1; i <= right; i++)
		if (v[i]->count > v[left]->count)
			swap(v, ++last, i);
	swap(v, left, last);
	quicksort(v, left, last-1);
	quicksort(v, last+1, right);
}

/* swap: interchange v[i] and v[j] */
void swap(struct wc *v[], int i, int j)
{
	struct wc *tmp;

	tmp = v[i];
	v[i] = v[j];
	v[j] = tmp;
}
static struct wc *wc_creat(char *word, int count)
{
	struct wc *p;

	if ((p = malloc(sizeof(struct wc *))) == NULL) {
		fprintf(stderr, "wc_creat: malloc error\n");
		return NULL;
	}
	p->word = strdup(word);
	p->count = count;
	return p;
}

static void wc_free(struct wc *p)
{
	free(p->word);
	free(p);
}
