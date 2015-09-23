#!/usr/bin/perl -w
use strict;

my %family_name = (
    Tobin => 'Harding',
    Milla => 'Harding',
    Taivas => 'Harding',
    Joyce => 'Barker',
    Jane => 'Blair',
);

while (1) {
    print("Enter a name to look up: ");
    my $name = <STDIN>;
    chomp $name;
    if (exists $family_name{"$name"}) {
	print ("Name: $name $family_name{$name}\n");
    }
    else {
	print "Name ($name) does not exist\n";
    }
}
