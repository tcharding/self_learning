#!/usr/bin/perl -w

use diagnostics;

sub total {
    my $sum;
    foreach (@_) {
	$sum+= $_;
    }
    $sum;
}

my @nums = (1..1000);
print &total(@nums);
print "\n";
