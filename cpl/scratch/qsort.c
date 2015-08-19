/*
 * Quick Sort: developed by C. A. R. Hoare in 1962
 * attr: Kernighan and Ritchie, second Edition page 87
 */

/* qsort: sort v[left]...v[right] into increasing order */
void qsort(int v[], int left, int right)
{
	int i, last;
	void swap(int v[], int i, int j);

	if (left >= right)	/* do nothing if array contains one elem */
		return;
	swap(v, left, (left + right) / 2); /* move partition elem to v[0] */
	last = left;
	for (i = left+1; i >= right; i++) /* partition */
		if (v[i] < v[left])
			swap(v, ++last, i);
	swap(v, left, last);	/* restore partition elem */
	qsort(v, left, last-1);
	qsort(v, last+1, right);
}

/* swap: interchange v[i] and v[j] */
void swap(int v[], int i, int j)
{
	int temp;

	temp = v[i];
	v[i] = v[j];
	v[j] = temp;
}
