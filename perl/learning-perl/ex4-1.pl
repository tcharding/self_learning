#!/usr/bin/perl -w

use diagnostics;

sub total {
    my $sum;
    foreach (@_) {
	$sum+= $_;
    }
    $sum;
}

my @nums = qw( 3 2 0 1 );
print &total(@nums);
print "\n";
