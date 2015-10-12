#!/usr/bin/perl
use strict;
use warnings;

# add-file-comment.pl - set the file name in this comment line
#
# Tobin Harding

$^I = "";

my $arg = $ARGV[0];
while (<>) {
    s/name\.pl/$arg/g;
    print;
}

