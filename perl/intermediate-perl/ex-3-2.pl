#!/usr/bin/perl
use strict;
use warnings;

# ex-3-2.pl - regex from stdin
#
# Tobin Harding

my $dir = "/";			# apply regex to this directory
my @files = `ls $dir`;

printf "Enter regular expression to run agianst: %s\n", $dir;
while (1) {
    print "regex> ";
    chomp (my $regex = <STDIN>);
    last unless ( defined $regex && length $regex );

    print map {	"    $_\n" }
	grep { eval{ /$regex/ } }
	@files;
}

    
#     my @res = map {
# 	if (/$regex/) {
# 	    $_;
# 	} else {
# 	    ();
# 	}
#     } @files;
#     for (@res) {
# 	print "$_";
#     }
# }

