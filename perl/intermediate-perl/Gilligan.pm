#!/usr/bin/perl
use strict;
use warnings;
package Gilligan;

our @day = qw(ark dip wap sen pop sep kir);
our @month = qw(diz pod bod rod sip wax lin sen kun fiz nap dep);

sub day {
    my $num = shift @_;
    if ($num < 0 || $num >= $#day) {
	warn "$num out of bounds\n";
    }
    $day[$num];
}

sub month {
    my $num = shift @_;
    die  "$num out of bounds\n"
	if ($num < 0 || $num >= $#month);
    $month[$num];
}
1
