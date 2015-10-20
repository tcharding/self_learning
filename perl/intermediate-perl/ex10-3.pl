#!/usr/bin/perl
use strict;
use warnings;

my @words = qw( one and Arrow Two tonne worlds world's end);
print "@words\n";


my @sorted =
    map $_->[0],
    sort {$a->[1] cmp $b->[1] }
    map {
	my $string = $_;
	$string =~ tr/A-Z/a-z/;
	$string =~ tr/a-z//cd;
#	print "$string\n";
	[$_, $string];
    } @words;


print "@sorted\n";
