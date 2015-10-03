#!/usr/bin/perl -w
use strict;

my %last_name = qw{
		     fred flinstone Wilma Flinstone Barney Rubble
		     betty rubble Bamm-Bamm Rubble PEBBLES FLINSTONE
	     };

sub by_last_first_name {
    "\L$last_name{$a}" cmp "\L$last_name{$b}"
	or
	"\L$a" cmp "\L$b"
}

for (sort by_last_first_name keys %last_name) {
    print "$last_name{$_} $_\n";
}
