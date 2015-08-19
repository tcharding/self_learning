#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#define _XOPEN_SOURCE 700
char *strdup(const char *s);	/* string.h */

struct tnode {			/* tree node */
	char *word;
	int count;
	struct tnode *left;
	struct tnode *right;
};

char *keywords[] = { 
	"auto",	"break", "case", "char", "const","continue", "default", "do",
	"default", "else", "enum", "extern", "float", "for", "goto", "if", "int",
	"long", "register", "return", "short", "signed", "sizeof", "static",
	"struct", "switch", "typedef", "union", "unsigned", "void", "volatile",
	"while",
};

#define NKEYS (sizeof keywords / sizeof(char *))
#define MAXWORD 100
#define SIGNF 6			/* default to 6 characters (like compiler does) */

/* static prototypes */
static int binsearch(char *word, char *tab[], int n);
static struct tnode *addtree(struct tnode *, char *);
static void treeprint(struct tnode *);
static struct tnode *talloc(void);
static int identifier(const char *word);
static int collision(struct tnode *p, struct tnode *q, int signf);
static void treeprint_signf(struct tnode *p, int signf, int pflag);

/* C identifier frequency count */
int main(int argc, char *argv[])
{
	struct tnode *root;
	char word[MAXWORD];
	char *getword(char *, int);
	int flag;		/* 1 if inside comment block */
	int signf;		/* number of significant characters */

	flag = 0;
	root = NULL;
	signf = (argc > 1) ? atoi(argv[1]) : SIGNF;
	while (getword(word, MAXWORD) != NULL) {
		if (strcmp(word, "/*") == 0)
			flag = 1;
		if (flag == 1) {
			if (strcmp(word, "*/") == 0)
				flag = 0;
			continue;
		}
		if (identifier(word))
			root = addtree(root, word);
	}
	treeprint_signf(root, signf, 0);
/*	treeprint(root); */
	return 0;
}

/* identifier: return non-zero if word is a legal C identifier */
static int identifier(const char *word)
{
	
	if (!(*word == '_' || isalpha(*word)))
		return 0;
	return 1;
}

/* binsearch: find word in tab[0] .. tab[n-1], return -1 if not found */
static int binsearch(char *word, char *tab[], int n)
{
	int cond;
	int low, high, mid;

	low = 0;
	high = n - 1;

	while (low <= high) {
		mid = (high+low) / 2;
		if ((cond = strcmp(word, tab[mid])) < 0)
			high = mid - 1;
		else if (cond > 0)
			low = mid + 1;
		else
			return mid;
	}
	return -1;
}

/* addtree: add a node with w, at or below p */
static struct tnode *addtree(struct tnode *p, char *w)
{
	int cond;

	if (p == NULL) {	/* a new word has arrived */
		p = talloc();
		p->word = strdup(w);
		p->count = 1;
		p->left = p->right = NULL;
	} else if ((cond = strcmp(w, p->word)) == 0)
		p->count++;	/* repeated word */
	else if (cond < 0)	/* less than into left subtree */
		p->left = addtree(p->left, w);
	else			/* greater than into right subtree */
		p->right = addtree(p->right, w);
	return p;
}

/* treeprint_signf: in-order print of tree (signf collisions only) */
static void treeprint_signf(struct tnode *p, int signf, int pflag)
{

	if (p != NULL) {
		int cflag = 0;		/* 1 if child causes print */
		if (collision(p, p->left, signf) || collision(p, p->right, signf))
			cflag = 1;

		treeprint_signf(p->left, signf, cflag);
		if (cflag || pflag)
			printf("%s\n", p->word); 
		treeprint_signf(p->right, signf, cflag);
	}
}

/* collision: return non-zero if signf characters of p and q are the same */
static int collision(struct tnode *p, struct tnode *q, int signf)
{

	if (p == NULL || q == NULL)
		return 0;
        return ((strncmp(p->word, q->word, signf) == 0) ? 1 : 0);
}

/* treeprint: in-order print of tree p */
static void treeprint(struct tnode *p)
{
	if (p != NULL) {
		treeprint(p->left);
		if (binsearch(p->word, keywords, NKEYS) < 0) 
			printf("%4d %s\n", p->count, p->word); 
		treeprint(p->right);
	}
}

/* talloc: make a tnode */
static struct tnode *talloc(void)
{
	return (struct tnode *)malloc(sizeof(struct tnode));
}

/* treesize: get number of non-NULL nodes */
static int treesize(struct tnode *p)
{
	if (p == NULL)
		return 0;
	return (1 + treesize(p->left) + treesize(p->right));
}
