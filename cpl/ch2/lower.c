/* lower: convert c to lower case; ASCII only */
int lower(int c)
{
	(c >= 'A' && c <= 'Z') ? return c + 'a' - 'A' : return c;
	/*
	if (c >= 'A' && c <= 'Z')
		return c + 'a' - 'A';
	else
		return c;
	*/
}
