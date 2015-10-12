#!/usr/bin/perl
use strict;
use warnings;
use autodie;

# new-question.pl - start a new question
#
# Tobin Harding

die "Usage: $0 <ch.q>\n" 
    if (@ARGV < 1);

my $qnum = $ARGV[0];
my ($ch, $ex) = split /\./, $qnum;
#printf "ch: %d ex: %d", $ch, $ex;
my $file = sprintf "ex-%s-%s.pl", $ch, $ex;
#print "$file\n";
if (-e 'cur.pl') {
    system 'rm cur.pl';   
}
system "touch $file";

chmod 0744, "$file";
system "ln -s $file cur.pl";
