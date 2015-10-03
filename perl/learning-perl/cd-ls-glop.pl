#!/usr/bin/perl -w
use strict;

print "Enter directory name: ";
chomp (my $dir = <STDIN>);

#chdir $dir or die "$0 error: directory '$dir' does not appear to exist\n";
opendir my $dh, $dir or die "Failed to open $dir $!";

my @files;
while (my $file = readdir $dh) {
    push (@files, $file);
}

for (sort @files) {
    print "$_\n";
}
