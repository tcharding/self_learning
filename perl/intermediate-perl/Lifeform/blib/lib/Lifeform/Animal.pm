package Lifeform::Animal;

use 5.022000;
use strict;
use warnings;

our $VERSION = '0.01';

sub speak {
    my $class = shift;
    print "a $class goes ", $class->sound, "!\n";
}

sub sound {
    die 'You must define sound in subclass';
}

1;
