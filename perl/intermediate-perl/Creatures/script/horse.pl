#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Horse;

my $horse = Horse->new('Mr. Ed');
printf "%s is colored %s\n", $horse->name, $horse->color;
$horse->speak;
$horse->set_color('Black');
printf "%s is now colored %s\n", $horse->name, $horse->color;

$horse->set_name('Retired');
printf "Horse '%s' has left the building\n", $horse->name;

