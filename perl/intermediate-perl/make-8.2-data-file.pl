#!/usr/bin/perl
use v5.10.1;
use utf8;
use strict;
use warnings;

my @castaways = ( qw(Skipper Professor Gilligan Mary-Ann Ginger), 
	'Mr. Howell', 'Mrs. Howell' );
my @things = qw(coconut banana papaya flower crab);

foreach ( 0 .. 100 ) {
	my $castaway = $castaways[ rand @castaways ];
	my $thing = $things[ rand @things ];
	my $number = 1 + int rand 5;
	$thing .= 's' if $number > 1;
	say "$castaway: $number $thing";
	}


