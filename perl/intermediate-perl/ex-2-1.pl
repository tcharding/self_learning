#!/usr/bin/perl
use strict;
use warnings;
use File::Spec;
use Cwd;

# ex-2-1.pl - get full path listing of current working directory
#
# Tobin Harding

#my $cwd = File::Spec->curdir();
#my $cwd = `pwd`;
my $cwd = getcwd();
my @files = `ls *`;
for (@files) {
    chomp $_;
    printf "    %s\n", File::Spec->catfile($cwd, $_);
}


