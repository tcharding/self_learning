/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 2.7 */
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "listlib.h"
#define TRAV_INIT_SIZE 8

struct list {
	data_t item;
	struct list *next;
};
typedef struct list list_t;

static struct list endlist;
static struct list *headp = NULL;
static struct list *tailp = NULL;
static struct list **travptrs = NULL;
static int travptrs_size = 0;

/* accessdata: return non-negative traversal key if successful */
int accessdata(void)
{
	int i;
	struct list **newptrs;
	
	if (headp == NULL) {	/* can't access an empty list */
		errno = EINVAL;
		return -1;
	}
	if (travptrs_size == 0) { /* first traversal */
		travptrs = (struct list **)calloc(TRAV_INIT_SIZE,
						  sizeof(struct list *));
		if (travptrs == NULL) 
			return -1;
		travptrs[0] = headp;
		travptrs_size = TRAV_INIT_SIZE;
		return 0;
	}
	for (i = 0; i < travptrs_size; i++) { /* look for an empty slot for key */
		if (travptrs[i] == NULL) {
			travptrs[i] = headp;
			return i;
		}
	}
	newptrs = realloc(travptrs, 2*travptrs_size*sizeof(struct list *));
	if (newptrs == NULL)
		return -1;
	travptrs = newptrs;
	travptrs[travptrs_size] = headp;
	travptrs_size *= 2;
	return travptrs_size / 2;
}

/* adddata: allocate node for data and add to end of list */
int adddata(data_t data)
{
	struct list *newnode;
	int nodesize;

	nodesize = sizeof(struct list) + strlen(data.s) + 1;
	if ((newnode = (struct list *)(malloc(nodesize))) == NULL)
		return -1;	/* couldn't add node */
	newnode->item.time = data.time;
	newnode->item.s = (char *)newnode + sizeof(struct list);
	strcpy(newnode->item.s, data.s);
	newnode->next = NULL;
	if (headp == NULL)
		headp = newnode;
	else
		tailp->next = newnode;
	tailp = newnode;
	return 0;
}

/* getdata: copy next item and set dp->s */
int getdata(int key, data_t *dp)
{
	struct list *p;

	if ( (key < 0) || (key >= travptrs_size) || (travptrs[key] == NULL)) {
		errno = EINVAL;
		return -1;
	}
	if (travptrs[key] == &endlist) { /* end of list */
		dp->s = NULL;
		travptrs[key] = NULL;
		return 0;	/* end of list is natural condition */
	}
	p = travptrs[key];
	dp->s = (char *)malloc(strlen(p->item.s) + 1);
	if (dp->s == NULL)
		return -1;
	dp->time = p->item.time;
	strcpy(dp->s, p->item.s);
	if (p->next == NULL)
		travptrs[key] = &endlist;
	else
		travptrs[key] = p->next;
	return 0;
}

/* freekey: free list entry corresponding to key */
int freekey(int key)
{
	if ( (key < 0) || (key >= travptrs_size)) { /* out of range */
		errno = EINVAL;
		return -1;
	}
	travptrs[key] = NULL;
	return 0;
}
