#!/usr/bin/perl
use strict;
use warnings;

# ex-3-1.pl - print files smaller than MAX
#
# Tobin Harding

my $MAX  = 400;

die "Usage: $0 file [file ...]\n"
    if (@ARGV < 1 );

my @files = grep {
    my $size = (stat $_)[7];
    if ($size < $MAX) {
	$_;
    } else {
	();
    }
} @ARGV;

@files = map { '    ' . $_ . "\n" } @files;

for (@files) {
    printf "%s", $_;
}


