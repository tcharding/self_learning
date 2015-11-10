#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

my @set1 = qw( chal-1.pl chal-2.pl chal-3.pl chal-5.pl skip-6 skip-7 chal-8.pl);
my @set2 = qw( chal-9.pl chal-10.pl skip-11 skip-12 skip-13);

run( "Set 1", @set1 );
run( "Set 2", @set2 );


sub run {
    my( $name, @set ) = @_;
    print "*** Running solutions to $name ***\n";

    for (@set) {
	if ( /(skip-.+)/ ) {
	    print "$1: output or run time too long\n";
	} else {
	    system "perl -Ilib script/$_";	
	}
    }
    say "";
}
