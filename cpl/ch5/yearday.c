#include <stdio.h>

static char daytab[2][13] = {
	{0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
	{0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31},
};

int day_of_year(int year, int month, int day);
void month_day(int year, int yearday, int *pmonth, int *pday);

int main(void)
{
	int leap = 2000;
	int nleap = 2001;
	int yearday = 105;
	int month, day;

	/* test month_day */
	month_day(nleap, yearday, &month, &day);
	if (month != 4 || day != 15)
		printf("Error: month_day, nleap\n");
	month_day(leap, yearday, &month, &day);
	if (month != 4 || day != 14)
		printf("Error: month_day, leap\n");

	/* test day_of_year */
	if (day_of_year(nleap, 4, 15) != yearday)
		printf("Error: day_of_year, nleap\n");
	if (day_of_year(leap, 4, 14) != yearday)
		printf("Error: day_of_year, leap\n");
	return 0;
}

/* day_of_year: set day of year from month and day */
int day_of_year(int year, int month, int day)
{
	int i, leap;

	if (year < 0 || month < 0 || day < 0 || month > 12)
		return -1;

	leap = (year%4 == 0 && year%100 != 0) || year%400 == 0;
	if (day > daytab[leap][month]) 
		return -1;

	for (i = 0; i < month; i++)
		day += *((*(daytab+leap))+i);
	return day;
}

/* month_day: set month, day from day of year */
void month_day(int year, int yearday, int *pmonth, int *pday)
{
	int i, leap;

	leap = (year%4 == 0 && year%100 != 0) || year%400 == 0;
	for (i = 0; yearday > daytab[leap][i]; i++) 
		yearday -= daytab[leap][i];
	*pmonth = i;
	*pday = yearday;
}
