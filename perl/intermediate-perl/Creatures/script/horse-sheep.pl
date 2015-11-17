#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Horse;
use Sheep;

my $tv_horse = Horse->new('Mr. Ed');
$tv_horse->eat('hay');
Sheep->eat('grass');
