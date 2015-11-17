package Mouse;
use parent qw(Animal);

use 5.022000;
use strict;
use warnings;

sub sound {
    'squeek';
}

sub speak {
    my $class = shift;
    Animal::speak($class);	# MESSY
    print "[but you can barely hear it!]\n";
}


1;
