#!/usr/bin/perl -w
use strict;

die "Usage: $0 missing file operand\n"
    if (@ARGV == 0);
die "Usage: $0 missing file operand after '$ARGV[0]'\n"
    if (@ARGV == 1);

my ($src, $dst) = @ARGV;

if (-d $dst) {
    rename $src => "$dst/$src";
} else {
    rename $src => $dst;
}
