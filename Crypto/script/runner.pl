#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

my @set1 = qw( chal-1.pl chal-2.pl chal-3.pl skip-4 chal-5.pl skip-6 quiet-7 chal-8.pl);
my @set2 = qw( chal-9.pl chal-10.pl quiet-11 quiet-12 quiet-13 lib-15 chal-16.pl);

run( "Set 1", @set1 );
run( "Set 2", @set2 );


sub run {
    my( $name, @set ) = @_;
    print "Running Solutions to $name\n";
    print "==========================\n";
    for (@set) {
	if ( /skip-(.+)/ ) {
	    print "Set   Challenge $1: skipped, too slow\n"
	} elsif ( /quiet-(.+)/ ) {
	    system "perl -Ilib script/chal-${1}.pl > /dev/null";	
	    print "Set   Challenge $1: ran with output suppressed\n";
	} elsif ( /lib-(.+)/ ) {
	    print "Set   Challenge $1: Completed in modules\n";
	} else {
	    system "perl -Ilib script/$_";	
	}
    }
    say "";
}
