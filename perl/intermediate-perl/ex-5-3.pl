#!/usr/bin/perl
use strict;
use warnings;

# Parse and print network usage report
#
# log format: from.where to.where num


# Parse
my %total;
while (<>) {
    my ($src, $dst, $bytes) = split;
    $total{$src}{$dst} += $bytes;
}

my @entries;
for my $source (sort keys %total) {
    my $sum;
    my @entry;
    push @entry, $source;
    my %transfers;
    for my $destination (sort keys %{ $total{$source} }) {
	$transfers{$destination} = $total{$source}{$destination};
	$sum += $total{$source}{$destination};
    }
    push @entry, $sum;
    push @entry, \%transfers;
    push @entries, \@entry;
}

@entries = sort { $a->[1] cmp $b->[1] } @entries;

# Print
for my $entry_ref  (@entries) {
    my ($source, $sum, $transfers) = @{$entry_ref};

    printf "%s\n", $source;
    for (sort keys %{$transfers}) {
	printf "\t%-25s %15s\n", $_, ${$transfers}{$_};
    }
    print "\n";
    print "\n";
}
