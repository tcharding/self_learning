#!/usr/bin/perl -w
use strict;

sub average {
    my $sum;
    my $n;
    foreach (@_) {
	$sum += $_;
	$n++;
    }
    $sum / $n;
}

sub above_average {
    my $average = &average(@_);
    my @res;
    foreach (@_) {
	if ($_ > $average) {
	    push(@res, $_);
	}
    }
    @res;
}

my @fred = &above_average(1..10);
print "\@fred is @fred\n";
print "(Should be  6 7 8 9 10)\n";
my @barney = &above_average(100, 1..10);
print "\@barney is @barney\n";
print "(Should be just 100)\n";
