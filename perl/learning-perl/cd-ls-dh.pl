#!/usr/bin/perl -w
use strict;

print "Enter directory name: ";
chomp (my $dir = <STDIN>);

chdir $dir or die "$0 error: directory '$dir' does not appear to exist\n";
my @files = <* .*>;
for (@files) {
    print "$_\n";
}
