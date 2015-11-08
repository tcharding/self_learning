package Lifeform::Horse;
use parent qw(Lifeform::Animal);

use 5.022000;
use strict;
use warnings;

sub test {
    print "It works!\n";
}

sub sound {
    'neigh';
}

1;
