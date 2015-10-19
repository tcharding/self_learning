#!/usr/bin/perl
use strict;
use warnings;
use v5.10;

while (<>) {
    state $fhs;
    chomp $_;
    my ($person, $what) = split /:/;
    unless ($fhs->{$person} ) {
	open my $fh, '>>', $person
	    or die "Failed to open fh for $person: $!";
	$fhs->{$person} = $fh;
    }
    say { $fhs->{$person} } "$_";
}
