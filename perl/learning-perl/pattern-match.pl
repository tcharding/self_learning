#!/usr/bin/perl -w
use strict;
use autodie;

my $file = "input.txt";
open my $fh, '<', $file;
chomp (my @lines = <$fh>);

while (1) {
    print "Enter a regular expression to run it against $file\n";
    print "> ";
    chomp (my $regex = <STDIN>);
    last if $regex =~ /\A\s*\Z/;
    my @matches = eval {
	grep /$regex/, @lines;
    };
    if ($@) {
	print "Error: invalid regular expression ($@), please try again.\n";
    } else {
	my $count = @matches;
	print ": $_\n";
	print "There were $count matching strings:\n",
	    map "$_\n", @matches;
    }
    print "\n";
}
