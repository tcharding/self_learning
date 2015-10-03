#!/usr/bin/perl -w
use strict;

my $num = 1;

while (1) {
    if (($num % 3 == 0) && ($num % 5 == 0) && ($num % 7 == 0)) {
	last;
    }
    $num++;
}
printf "First number divisible by 3, 5, and 7 is: %d\n", $num;
