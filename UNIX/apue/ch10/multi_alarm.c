#include "apue.h"
#include <setjmp.h>
#define SIG_MSG "Recieved signal ALARM\n"
#define MAX_ALRM 255		/* maximum number of alarms */

/* export this if multi_alarm needed for another project */
static unsigned int multi_alarm(unsigned int new);

/* linked list implementation */
typedef struct linked_list_node {
	sig_atomic_t t;
	struct linked_list_node *next;
} ll_t;
static ll_t *ll_creat(unsigned int t);
static void ll_free(ll_t *np);
static void ll_rmhead();

/* static prototypes */
static void sig_alrm(int signo);
static void add_alrm(unsigned int t);
static unsigned int next_alrm();

/* internal linkage data types */
static ll_t *head;
static jmp_buf env_alrm;

/* test multi_alarm */
int main(void)
{
	unsigned int set;
	unsigned int first, second, third, fourth;

	first = 2;
	second = 4;
	third = 6;
	fourth = 1;

	
	if (signal(SIGALRM, sig_alrm) == SIG_ERR)
		err_sys("signal error");

	if ((set = multi_alarm(first)) != first)
		err_msg("multi_alarm error: set:%u exp:%u", set, first);
	if ((set = multi_alarm(second)) != second-first)
		err_msg("multi_alarm error: set:%u exp:%u", set, second-first);

	set = multi_alarm(third);
	set = multi_alarm(fourth);

	/* if (setjmp(env_alrm) != 0) { */
	/* 	if (signal(SIGALRM, sig_alrm) == SIG_ERR) */
	/* 		err_sys("signal error"); */
	/* } */
	if (sigsetjmp(env_alrm, 1)) {
		if (signal(SIGALRM, sig_alrm) == SIG_ERR)
			err_sys("signal error");
	}
	for ( ; ; ) {
		(void)pause();
	}
}

/* sig_alrm: sig catcher */
static void sig_alrm(int signo)
{
	int err = errno;
	
	(void)write(STDOUT_FILENO, SIG_MSG, sizeof(SIG_MSG) - 1);
	(void)write(STDOUT_FILENO, &signo, sizeof(int));
	(void)alarm(next_alrm());
	ll_rmhead();
	errno = err;
	siglongjmp(env_alrm, 1);
}

/* multi_alarm: add multiple alarm capability to alarm(3P)
    returns time until next alarm */
static unsigned int multi_alarm(unsigned int new)
{
	unsigned int cur;	/* time until current alarm */

	cur = alarm(new);
	if (cur == 0 || cur == new)
		return new;
	if (cur > new) {	/* new alarm expires first */
		add_alrm(cur - new);
		return new;
	} else if (cur < new) {	/* current alarm expires first */
		add_alrm(new - cur);
		(void)alarm(cur);
		return cur;
	}
	return 0;		/* shouldn't get here */
}

/* add_alrm: add t to list of alarms */
static void add_alrm(unsigned int t)
{
	ll_t *new, *before, *after;

	new = ll_creat(t);	/* call does not return if error */

	if (head == NULL) {	/* empty list */
		head = new;
	} else if (new->t < head->t) {	/* insert at head of list */
		new->next = head;
		head = new;
	} else {		/* search for spot to add */
		before = head;
		after = head->next;
		for ( ; ; ) {
			if ((after == NULL) ||
			    (new->t < after->t)) {
				before->next = new;
				new->next = after;
				break;
			} else {
				before = after;
				after = after->next;
			}
		}
	}
}

/* next_alrm: get next alarm from list, 0 if list empty */
static unsigned int next_alrm()
{
	if (head == NULL)
		return 0;
	return head->t;
}

/*
 *  ------------ linked list implementation --------------- 
*/

/* ll_rmhead: remove list_node from head of list */
static void ll_rmhead()
{
	ll_t *tmp;

	if (head == NULL)
		return;
	tmp = head->next;
	ll_free(head);
	head = tmp;
}

/* ll_creat: create list node, free wit ll_free */
static ll_t *ll_creat(unsigned int t)
{
	ll_t *new;

	new = Malloc(sizeof(ll_t));
	new->t = t;
	return new;
}
/* ll_free: free list node */
static void ll_free(ll_t *np)
{
	if (np != NULL)
		free(np);
}
