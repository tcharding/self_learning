#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

my @quick = qw( chal-1.pl chal-2.pl chal-3.pl chal-5.pl skip-6 skip-7 chal-8.pl);

for (@quick) {
    if ( /(skip-.)/ ) {
	print "$1: output or run time too long\n";
    } else {
	system "perl -Ilib script/$_";	
    }
}
