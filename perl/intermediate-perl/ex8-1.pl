#!/usr/bin/perl
use strict;
use warnings;
use IO::Tee;

if (@ARGV < 1) {
    die "Usage: $0 OPTION\n options:\n\t -s\n\t -f file\n";
}

my $sflag = 0;
my $file = "";

while (@ARGV > 0) {
    my $opt = shift @ARGV;
    if ($opt eq "-s") {
	$sflag = 1;
    } elsif ($opt eq "-f") {
	$file = shift @ARGV;
    } else {
	die "Unknown option: $opt";
    }
}
my $fh;
my $string = "";
if ( ($file ne "") && ($sflag == 1) ) {
    open my $s_fh, '>', \ $string;
    open my $f_fh, '>', $file;
    $fh = IO::Tee->new ($s_fh, $f_fh);
} elsif ($sflag == 1) {
    open $fh, '>', \ my $string;
}
if ($file ne "") {
    open $fh, '>', $file;
}

print {$fh} `date`;
print "string: $string\n";
