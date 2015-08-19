/* attribution: UNIX Systems Programming - Robbins and Robbins */
/* Program 2.5 */
static int cnt = 0;

/* onepass: return true if interchanges are made */
static int onepass(int a[], int n)
{
	int i, interchanges, tmp;

	for (i = 0; i < n-1; i++)
		if (a[i] > a[i+1]) {
			tmp = a[i];
			a[i] = a[i+1];
			a[i+1] = tmp;
			interchanges = 1;
			cnt++;
		}
	return interchanges;
}

void clearcount(void)
{
	cnt = 0;
}

int getcount(void)
{
	return cnt;
}

/* bubblesort: a[0] < a[1] .. < a[n-1] */
void bubblesort(int a[], int n)
{
	int i;

	for (i = 0; i < n-1; i++)
		if (!onepass(a, n-i))
			break;
}
