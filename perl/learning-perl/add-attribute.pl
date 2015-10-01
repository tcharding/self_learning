#!/usr/bin/perl -w
use strict;
use autodie;

my $attrib = "/* attr: Advanced Programming in the UNIX Environment - Stevens and Rago */\n";

foreach (@ARGV) {
    my $infile = $_;
    my $outfile = $infile;
    $outfile =~ s/(.*)/$1\.new/;
    open IN, '<', $infile;
    open OUT, '>', $outfile;
    
    $_ = <IN>;
    if (/attr/) {
	print OUT $_;
    } else {
	print OUT $attrib;
	print OUT $_;
    }

    while (<IN>) {
	print OUT $_;
    }
}


    
    

    
