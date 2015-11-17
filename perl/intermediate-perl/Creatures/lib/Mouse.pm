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
    $class->SUPER::speak(@_);
    print "[but you can barely hear it!]\n";
}


1;
