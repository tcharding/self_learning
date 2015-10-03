#!/usr/bin/perl -w
use strict;
use DateTime;

(my $day, my $month, my $year) = @ARGV;

my $now = DateTime->now;
my $past = DateTime->new(
    day => $day,
    month => $month,
    year => $year,
);
my $duration = $now - $past;
my @units = $duration->in_units( qw(years months days) );

printf "%d years, %d months, %d days\n", @units;
   
