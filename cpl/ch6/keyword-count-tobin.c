/* based on text with added attempt to handle comment blocks */
/* not functioning because I rolled back getword.c */
#include <stdio.h>
#include <ctype.h>
#include <string.h>

struct key {
	char *word;
	int count;
} keytab[] = { 
	{"auto", 0},
	{"break", 0},
	{"case", 0},
	{"char", 0,},
	{"const", 0},
	{"continue", 0},
	{"default", 0},
	{"do", 0},
	{"default", 0},
	{"else", 0},
	{"enum", 0},
	{"extern", 0},
	{"float", 0},
	{"for", 0},
	{"goto", 0},
	{"if", 0},
	{"int", 0},
	{"long", 0},
	{"register", 0},
	{"return", 0},
	{"short", 0},
	{"signed", 0},
	{"sizeof", 0},
	{"static", 0},
	{"struct", 0},
	{"switch", 0},
	{"typedef", 0},
	{"union", 0},
	{"unsigned", 0},
	{"void", 0},
	{"volatile", 0},
	{"while", 0},
};

#define MAXWORD 100
#define NKEYS (sizeof keytab / sizeof(struct key)) 

/* prototypes */
struct key *binsearch(char *word, struct key *tab, int n);

/* count C keywords; tobin's version */
int main(void)
{
	int getword(char *, int);
	char word[MAXWORD];
	struct key *p;
	int flag;		/* 1 if inside comment block */

	flag = 0;
	while (getword(word, MAXWORD) != EOF) {
		fprintf(stderr, "%s\n", word);
		if ((strcmp(word, "/*")) == 0) 
			flag = 1;
		if (flag == 1) { /* skip comment block */
			if ((strcmp(word, "*/")) == 0)
				flag = 0;
			continue; 
		}		
		if (isalpha(word[0])) 
			if ((p = binsearch(word, keytab, NKEYS)) != NULL) 
				p->count++;
	}
	for (p = keytab; p < keytab + NKEYS; p++)
		/*	if (p->count > 0) */
		printf("%4d %s\n", p->count, p->word);
	return 0;
}

/* binsearch: find word in tab[0] .. tab[n-1] */
struct key *binsearch(char *word, struct key *tab, int n)
{
	int cond;
	struct key *low = &tab[0];
	struct key *high = &tab[n];	s
truct key *mid;

	while (low < high) {
		mid = low + (high-low) / 2;
		if ((cond = strcmp(word, mid->word)) < 0)
			high = mid;
		else if (cond > 0)
			low = mid + 1;
		else
			return mid;
	}
	return NULL;
}


/* Define 'word' as 1 or more characters that may 
 * appear surrounded by whitespace in leagl C code */

int stab[] = {EOF, ';', '(', ')', '{', '}', '[', ']', '+', '-', '*', '/'};
#define NS (sizeof stab / sizeof(stab[0]))
/* issingle: true for any character that may stand alone in legal C code */
int issingle(int c)
{
	int i;
	
	for (i = 0; i < NS; i++)
		if (c == stab[i])
			return 1;
	return 0;
}

/* getword: get next word from input, return first character or EOF */
int getword(char *word, int lim)
{
	int c, getch();
	void ungetch(int c);
	char *w = word;

	while (isspace(c = getch()))
		;
	*w++ = c;
	if (issingle(c) ) {
		*w = '\0';
		return c;
	}
	for ( ; --lim > 0; w++) {
		if (isspace(*w = getch())) {
			ungetch(*w);
			break;
		}
	}
	*w = '\0';
	return word[0];
}
