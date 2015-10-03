#!/usr/bin/perl -w
use strict;
use 5.01001;
    
sub divisors {
    my $number = shift;

    my @divisors = ();
    foreach my $divisor (2 .. ($number/2) ) {
	push @divisors, $divisor unless $number % $divisor;
    }
    return @divisors;
}

die "Usage: $0 num\n"
    unless @ARGV == 1;

given (chomp (@ARGV[0])) {
    when (! /\A\d+\Z/ ) { print "Not a number, wombat!\n" }
    when ( $_ % 2 == 0) { print "Even, "; continue }
    when ( $_ % 2 != 0) { print "Odd, "; continue }
    when ( $_ % 1 == 0) { print "Divisible by my favourite number, "; continue }
    my @empty;
    my @divisors = divisors($ARGV[0]);
    when ( @divisors ~~ @empty ) { print "Prime!\n" }
    default { print "@divisors\n" }
}


