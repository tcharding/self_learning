#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

my @set1 = qw( 1 2 3 slow-4 5 slow-6 quiet-7 8);
my @set2 = qw( 9 10 quiet-11 quiet-12 quiet-13 noop-14 lib-15 16  );
my @set3 = qw( noop-17 quiet-18 noop-19 noop-20 lib-21 slow-22 23 noop-24 );

run( "Set 1", @set1 );
run( "Set 2", @set2 );
run( "Set 3", @set3 );


sub run {
    my( $name, @set ) = @_;
    print "Running Solutions to $name\n";
    print "==========================\n";
    for (@set) {
	if ( /slow-(.+)/ ) {
	    print "$name Challenge $1: Completed (not run)\n"
	} elsif ( /quiet-(.+)/ ) {
	    system "perl -Ilib script/chal-${1}.pl 1>/dev/null";	
	    print "$name Challenge $1: Completed ( > /dev/null )\n";
	} elsif ( /noop-(.+)/ ) {
	    print "$name Challenge $1: *** Incomplete ***\n";
	} elsif ( /lib-(.+)/ ) {
	    print "$name Challenge $1: Completed, see lib/Crypto/\n";
	} elsif ( /fail-(.+)/ ) {
	    print "$name Challenge $1: *** Has bugs ***\n";
	} else {
	    system "perl -Ilib script/chal-${_}.pl";	
	}
    }
    say "";
}
