#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#define NUM_VALS 45		/* one per degree */
#define NROWS 12		/* table rows */
#define NCOLS 4			/* table columns */
#define Q31 2147483648		/* 2e31 */

#define TANDATA "tandata.txt"	/* output filename */

/*
 * Create tangent table for use in assembler program: tangent.s
 *
 * NOTE: alter last value of output to 0xFFFFFFFF 
 *  (Q31 form cannot express value of 1)
 */

int main(void)
{
	int i, j, index;
	signed int tab[92];
	float tan_val;
	FILE *fp;
	const char *file = TANDATA;

	fp = fopen(file, "w");
	if (!fp) {
		fprintf(stderr, "Err: cannot open file: %s\n", file);
		exit(EXIT_FAILURE);
	}

	for (i = 0; i <= NUM_VALS; i++) {
		/* convert to radians */
		tan_val = tan(M_PI * i / 180.0);

		/* convert to Q31 notation */
		tab[i] = tan_val * Q31;
	}

	index = 0;
	for (i = 0; i < NROWS; i++) {
		fprintf(fp, ".word\t");
		for (j = 0; j < NCOLS; j++)
			fprintf(fp, "0x%x%s", tab[index++],
				j == 3 ? "" : ",");
		fprintf(fp, "\n");
	}

	if (fclose(fp) == EOF)
		fprintf(stderr, "failed to close file");
	
	exit(EXIT_SUCCESS);
}
