#!/usr/bin/perl -w
use strict;

my $quiet = undef;
my $upper = 100;
my $lower = 1;
my $num = int($lower + rand $upper);
$quiet // print "num: $num\n";

print "Enter a number between $lower and $upper: ";
while (chomp (my $guess = <STDIN>)) {
    if ($guess == $num) {
	print "Correct!\n";
	last;
    } elsif ($guess < $num && $guess > $lower) {
	print "Nice guess but higher: ";
    } elsif ($guess > $num && $guess < $upper) {
	print "Nice guess but lower: ";
    } else {
	print "Between $lower and a $upper goose: ";
    }
}

    
