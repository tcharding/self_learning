#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;
use Benchmark qw(:all);

chdir;
my @files = glob '*';
my @sorted;

cmpthese(my $count, {
    short => sub { @sorted = sort { -s $a <=> -s $b } glob '*' },
    long => sub {
	@sorted = 
	    map $_->[0],
	    sort { $b->[1] <=> $a->[1] }
	    map [$_, (stat $_)[7]],
	    @files
    }
});



# my @sorted = reverse
#     map $_->[0],
#     sort { $a->[1] <=> $b->[1] }
#     map [$_, (stat $_)[7]],
#     @files;

#print "sorted: ", Dumper(\@sorted);
