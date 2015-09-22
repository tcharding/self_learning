#!/usr/bin/perl -w

use diagnostics;

&greet( 'Fred' );
&greet( 'Tobin' );

sub greet {
    state $last_person;
    my $who = shift;

    print "Hello $who! ";
    if ( defined $last_person ) {
	print "$last_person is here also\n";
    }
    else {
	print "You are the first one here";
    }
    $last_person = $who;
}

