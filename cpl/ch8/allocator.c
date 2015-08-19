#include <stdio.h>
#include <sys/resource.h>

typedef long Align;		/* for alignment to long boundry */

union header {			/* block header: */
	struct {
		union header *ptr; /* next block if on free list */
		unsigned size;	   /* size of this block */
	} s;
	Align x;		/* force alignment of blocks */
};
typedef union header Header;

static Header base;		/* empty list to get started */
static Header *freep = NULL;	/* start of free list */
static long totmem = 0;		/* total memory used */

void free(void *ap);
static Header *morecore(unsigned);

/* malloc: general-purpose storage allocator */
void *malloc(unsigned nbytes)
{
	Header *p, *prevp;
	unsigned nunits;

	nunits = (nbytes+sizeof(Header)-1)/sizeof(Header) + 1;
	if ((prevp = freep) == NULL) { /* no free list yet */
		base.s.ptr = freep = prevp = &base;
		base.s.size = 0;
	}
	for (p = prevp->s.ptr; ; prevp = p, p = p->s.ptr) {
		if (p->s.size >= nunits) { /* big enough */
			if (p->s.size == nunits) /* exactly */
				prevp->s.ptr = p->s.ptr;
			else {
				p->s.size -= nunits;
				p += p->s.size;
				p->s.size = nunits;
			}
			freep = prevp;
			return (void *)(p+1);
		}
		if (p == freep)		/* wrapped around free list */
			if ((p = morecore(nunits)) == NULL)
				return NULL; /* none left */
	}
}

/* calloc: allocate memory for 'n' units of size 's' */
void *calloc(int n, unsigned s)
{
	return malloc(n * s);
}

#define NALLOC 1024		/* minimum #units to request */

/* morecore: ask system for more memory */
static Header *morecore(unsigned nu)
{
	char *cp, *sbrk(int);
	Header *up;
	unsigned long size;
	struct rlimit rl;

	if (nu < NALLOC)
		nu = NALLOC;
	size = nu * sizeof(Header);
	/*
	 * check system resource limit
	 */
	if (getrlimit(RLIMIT_AS, &rl) < 0)
		return NULL;
	if (totmem + size > rl.rlim_cur)
		return NULL;	/* unnecessary; sbrk fails if this is exceeded */
				/* implemented to satisfy question 8.7 */
	cp = sbrk(size);
	if (cp == (char *) -1)	/* no space at all */
		return NULL;
	totmem += size;		/* store total memory allocated */
	up = (Header *) cp;
	up->s.size = nu;
	free((void *)(up+1));
	return freep;
}

/* free: put block ap in free list */
void free(void *ap)
{
	Header *bp, *p;

	if (ap == NULL)		/* safe to free null block */
		return;
	bp = (Header *)ap - 1;	/* point to block header */
	if (bp->s.size == 0) /* add checks (question 8.7) */
		return;
	for (p = freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
		if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
			break;	/* freed block at start or end of arena */
	if (bp + bp->s.size == p->s.ptr) { /* join to upper nbr */
		bp->s.size += p->s.ptr->s.size;
		bp->s.ptr = p->s.ptr->s.ptr;
	} else
		bp->s.ptr = p->s.ptr;
	if (p + p->s.size == bp) { /* join to lower nbr */
		p->s.size += bp->s.size;
		p->s.ptr = bp->s.ptr;
	} else
		p->s.ptr = bp;
	freep = p;
}

/* bfree: add arbitary block  of memory to free list */
int bfree(void *p, int nbytes)
{
	Header *bp;
	int nunits;

	if (nbytes < (sizeof(Header) * 2))
		return -1;	/* too small */
	nunits = (nbytes - sizeof(Header)) / sizeof(Header);
	bp = (Header *)p;
	bp->s.size = nunits;
	free(bp);
	return 0;
}
