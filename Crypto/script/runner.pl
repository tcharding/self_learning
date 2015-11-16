#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

my @set1 = qw( 1 2 3 slow-4 5 slow-6 quiet-7 8);
my @set2 = qw( 9 10 quiet-11 quiet-12 quiet-13 lib-15 16 fail-17 
	       quiet-18 fail-19 fail-20 quiet-21 22 );

run( "Set 1", @set1 );
run( "Set 2", @set2 );


sub run {
    my( $name, @set ) = @_;
    print "Running Solutions to $name\n";
    print "==========================\n";
    for (@set) {
	if ( /slow-(.+)/ ) {
	    print "$name Challenge $1: skipped, too slow\n"
	} elsif ( /quiet-(.+)/ ) {
	    system "perl -Ilib script/chal-${1}.pl 1>/dev/null";	
	    print "$name Challenge $1: ran with output suppressed\n";
	} elsif ( /lib-(.+)/ ) {
	    print "$name Challenge $1: Completed in modules\n";
	} elsif ( /fail-(.+)/ ) {
	    print "$name Challenge $1: *** Has bugs ***\n";
	} else {
	    system "perl -Ilib script/chal-${_}.pl";	
	}
    }
    say "";
}
