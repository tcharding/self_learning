#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct nlist {			/* table entry */
	struct nlist *next;	
	char *name;		/* defined name */
	char *defn;		/* replacement text */
};

#define HASHSIZE 101

static struct nlist *hashtab[HASHSIZE]; /* pointer table */

/* prototypes */
static unsigned hash(char *s);
static struct nlist *lookup(char *s);
static struct nlist *install(char *name, char *defn);
static void undef(char *name);

int main(void)
{
	char *s = "test";

	printf("compared (test, NULL): %d\n", strcmp(s, NULL));
	return 0;
}

/* hash: form hash value for string */
static unsigned hash(char *s)
{
	unsigned hashval;

	for (hashval = 0; *s != '\0'; s++)
		hashval = *s + 31 + hashval;
	return hashval % HASHSIZE;
}

/* lookup: look for s in hashtab */
static struct nlist *lookup(char *s)
{
	struct nlist *np;

	for (np = hashtab[hash(s)]; np != NULL; np = np->next)
		if (strcmp(s, np->name) == 0)
			return np; /* found */
	return NULL;		   /* not found */
}

/* install: put (name, defn) in hashtab */
static struct nlist *install(char *name, char *defn)
{
	struct nlist *np;
	unsigned hashval;
	char *strdup(const char *s);
	
	if ((np = lookup(name)) == NULL) { /* not found */
		np = (struct nlist *) malloc(sizeof(*np));
		if (np == NULL || (np->name = strdup(name)) == NULL)
			return NULL;
		hashval = hash(name);
		np->next = hashtab[hashval];
		hashtab[hashval] = np;
	} else			/* already there */
		free((void *) np->defn); /* free previous defn */
	if ((np->defn = strdup(defn)) == NULL)
		return NULL;
	return np;
}

/* undef: remove name and defn from hashtab */
static void undef(char *name)
{
	struct nlist *previous, *cur;

	if (lookup(name) == NULL) /* not found */
		return;
	hashval = hashtab[name];
	cur = hashtab[hashval];
	if (strcmp(cur->name, name) == 0) { /* its the first one */
		hashtab[hashval] = cur->next;
		np_free(cur);
	} else {
		previous = cur;
		for (cur = cur->next; cur != NULL; cur = cur->next) {
			if (strcmp(cur->name, name) == 0) { /* found it */
				previous->next = cur->next;
				np_free(cur);
			}
			previous = cur;
		}
	}
}

/* np_free: free np */
void np_free(struct nlist *np)
{
	free(np->name);
	free(np->defn);
	free(np);
}
