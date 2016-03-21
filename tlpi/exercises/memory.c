/* Exercise 7.2 */
#include "tlpi_hdr.h"


struct node {
	long len;		/* length of this free block (including struct members) */
	struct node *next;
	struct node *previous;
};


void * my_malloc(size_t size);
void my_free(void *ptr);

static struct node *get_big_enough(size_t size);
static void *allocate_chunk(struct node *ptr, size_t size);

static struct node *find_big_enough(size_t size);
static void coalesce(void);
static int get_more_memory(void);
static Boolean is_splitable(struct node *ptr, size_t size);
static void *split_block(struct node *ptr, size_t size);

static Boolean is_big_enough(struct node *ptr, size_t size);
static void join_adjacent(struct node *a);
static Boolean has_adjacent(struct node *ptr);
static size_t memsize_of_block(struct node *ptr);


/* test memory allocator and my_free */ 
int
main()
{
/*	stress_test_allocator(); */
	char *buf = my_malloc(100);
	my_free(buf);

	exit(EXIT_SUCCESS);
}

/* static void */
/* stress_test_allocator() */
/* { */
/* 		int small = 10; */
/* 	int medium = 1000; */
/* 	int large = 1000000; */

/* 	int i; */
/* 	int loop = 1000; */

/* 	void *mem, *smem, *mmem, *lmem; */

/* 	mem = smem = mmem = lmem = NULL; */
	
/* 	for (i = 0; i < loop; i++) { */
/* 		mem = my_malloc(small); */
/* 		if (mem == NULL) */
/* 			fatal("small"); */

/* 		if (i % 3 == 0) { */
/* 			my_free(smem); */
/* 			smem = mem; */

/* 			mem = my_malloc(medium); */
/* 			if (mem == NULL) */
/* 				fatal("medium"); */
/* 		} */

/* 		if (i % 10 == 0) { */
/* 			my_free(mmem); */
/* 			mmem = mem; */
			
/* 			mem = my_malloc(large); */
/* 			if (mem == NULL) */
/* 				fatal("large"); /\*  *\/ */
/* 		} */

/* 		if (i % 30 == 0) { */
/* 			my_free(lmem); */
/* 			lmem = mem; */
/* 		} */

/* 		if (i % 50 == 0) { */
/* 			printf("program break: %10p\n", sbrk(0)); */
/* 		} */
/* 	} */
/* } */

void
my_free(void *memory)
{

}

struct node *pool;			/* available memory pool */

void *
my_malloc(size_t size)
{
	struct node *ptr = get_big_enough(size);
	if (ptr == NULL)
		return NULL;	/* no more memory */

	printf("early exit from my_malloc\n");
	exit(0);
	return allocate_chunk(ptr, size);
}

/* get_big_enough: allocate memory until block big enough is found */
static struct node *
get_big_enough(size_t size)
{
	struct node *ptr = NULL;

	for ( ; ; ) {
		if ((ptr = find_big_enough(size)))
			break;
		else
			coalesce();

		if ((ptr  = find_big_enough(size)))
			break;
		else
			if (get_more_memory() == -1)
				return NULL;
	}

	return ptr;
}

/* allocate_chunk: allocate chunk of block (ptr) */
static void *
allocate_chunk(struct node *ptr, size_t size)
{
	void *mem;

	if (is_splitable(ptr, size)) {
		mem = split_block(ptr, size);
	} else {
		ptr->previous->next = ptr->next;
		ptr->next->previous = ptr->previous;

		mem =  (void *) (ptr + sizeof(ptr->len));
	}

	return mem;
}
/* 
 * find_big_enough
 *
 * find an available block big enough to hold size bytes (first fit)
 */
static struct node *
find_big_enough(size_t size)
{
	struct node *ptr;

	for (ptr = pool; ptr != NULL; ptr = ptr->next) {
		if (is_big_enough(ptr, size))
			return ptr;
	}
	return NULL;
}

/* coalesce: coalesce available memory */
static void
coalesce(void)
{
	struct node *ptr;
	
	if (pool == NULL)
		return;
	
	for (ptr = pool; ptr->next != NULL; ptr = ptr->next) {
		if (has_adjacent(ptr))
			join_adjacent(ptr);
	}
}

#define BLOCK_SIZE 4096 * 10  /* 10 pages */

/* get_more_memory: increase the heap size */
static int
get_more_memory(void)
{
	struct node *ptr, *newNode;

	for (ptr = pool; ptr->next != NULL; ptr = ptr->next)
		;

	newNode = sbrk(BLOCK_SIZE);
	if (newNode == (void *)-1)
		return -1;
	
	newNode->len = BLOCK_SIZE;
	newNode->next = NULL;
	newNode->previous = ptr;
	ptr->next = newNode;

	return 0;
}

/* is_splitable: is block large enough to hold size bytes and a free block */
static Boolean
is_splitable(struct node *ptr, size_t size)
{
	size_t required = 0;

	required += sizeof(ptr->len); /* memory segment */
	required += size;
	
	required += sizeof(ptr->len); /* free block */
	required += sizeof(ptr->next);
	required += sizeof(ptr->previous);

	return (required <= ptr->len);
}

/* split_block: split free block into memory segment and new free block. */
static void *
split_block(struct node *ptr, size_t size)
{
	struct node *freeBlock;
	void *memSegment;
	size_t initialLengh, memSegmentLength, freeBlockLength;

	initialLengh = ptr->len;
	memSegmentLength = sizeof(ptr->len) + size;
	freeBlockLength = initialLengh - memSegmentLength;

				/* setup free block */
	freeBlock = ptr + sizeof(ptr->len) + size;
	freeBlock->len = freeBlockLength;
	freeBlock->next = ptr->next;
	freeBlock->previous = ptr->previous;


				/* setup memSegment */
	ptr->len = memSegmentLength;
	memSegment = ptr + sizeof(ptr->len);

	return memSegment;
}

/* is_big_enough: true if block is big enough to hold size bytes */
static Boolean
is_big_enough(struct node *ptr, size_t size)
{
	return (memsize_of_block(ptr) >= size);
}

/* has_adjacent: true if node has an adjacent free node */
static Boolean
has_adjacent(struct node *ptr)
{
	return (ptr+ptr->len == ptr->next);
}

/* join_adjacent: join two adjacent memory blocks a and b */
static void
join_adjacent(struct node *a)
{
	struct node *b = a->next;

	a->len = a->len + b->len;
	a->next = b->next;
}

/* memsize_of_block: return usable size of memory block */
static size_t
memsize_of_block(struct node *ptr)
{
	return (ptr->len - sizeof(ptr->len));
}
