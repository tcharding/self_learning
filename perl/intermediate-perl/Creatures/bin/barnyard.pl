#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw( ../lib );

#use LivingCreature ':all';
require 'Animal.pm';
require 'Cow.pm';
require 'Horse.pm';
require 'Mouse.pm';
require 'Sheep.pm';

my @barnyard;

say "Pick an animal followed by a name to add it to your barnyard.\n (Cow, Horse, Sheep, Mouse) followed by a name";
while (<>) {
    my( $animal, $name ) = split / /, $_;
    chomp $name;
    say "Adding a $animal named $name to your barnyard";
    #say "Pick another (or just hit enter)";
    push @barnyard, [ $animal, $name ];
}

say "";
&print_barnyard;

sub print_barnyard {
    say "Your barnyard contains: ";
    for (@barnyard) {
	my $pair = $_;
	my( $animal, $name ) = @$pair;
	print "A $animal called $name\n";
	#	$animal->speak;
	$animal->speak;
    }
}


