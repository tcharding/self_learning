#!/usr/bin/perl -w
use strict;
use 5.01001;

my $quiet = undef;
my $upper = 100;
my $lower = 1;
my $num = int($lower + rand $upper);
$quiet // print "num: $num\n";
my $guess;

print "Enter a number between $lower and $upper: ";
while (chomp (my $guess = <STDIN>)) {
    given ( $guess ) {
	when ($_ == $num) { print "Correct!\n"; }
	when ($_ < $num && $_ > $lower) { print "Nice guess but higher: "; }
	when ($guess > $num && $guess < $upper) { print "Nice guess but lower: "; }
	default { print "Between $lower and a $upper goose: "; }
    }
}

    
