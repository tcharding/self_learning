#!/usr/bin/perl -w
use strict;
use 5.01001;

die "Usage $0 num\n"
    if (@ARGV != 1);

my ($num) = @ARGV;

given ($num) {
    when ($_ % 3 == 0) {print "Fizz "; continue }
    when ($_ % 5 == 0) {print "Bin "; continue }
    when ($_ % 7 == 0) {print "Sausage " }
}
print "\n";

my $flag = 0;
my $num = 1;

while (1) {
    if (($num % 3) && ($num % 5) && ($num % 7))
	last;
    $num++;
}
printf "First number divisible by 3, 5, and 7 is: %d\n", $num;
