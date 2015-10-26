#!/usr/bin/perl
use strict;
use warnings;

require 'Gilligan.pm';

my( $sec, $min, $hour, $mday, $mon, $year, $wday ) = localtime;

printf "Today is %s %s %s %s\n", &Gilligan::day($wday),
    &Gilligan::month($mon), $mday, $year+1900;
