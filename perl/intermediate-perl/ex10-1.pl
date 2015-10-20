#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

chdir;
my @files = glob '*';
#my @sorted = sort { -s $a <=> -s $b } glob '*';
print "files: ", Dumper(\@files);


print "---------\n";

my @sorted = reverse
    map $_->[0],
    sort { $a->[1] <=> $b->[1] }
    map [$_, (stat $_)[7]],
    @files;

print "sorted: ", Dumper(\@sorted);
