#!/usr/bin/perl -w
#use diagnostics;

my $last_seen;

&greet( 'Fred' );
&greet( 'Tobin' );

sub greet {
    my $who = shift;

    print "Hello $who! ";
    if ( defined $last_seen ) {
	print "$last_seen is here also\n";
    }
    else {
	print "You are the first one here\n";
    }
    $last_seen = $who;
}

