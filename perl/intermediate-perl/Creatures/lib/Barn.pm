package Barn;

use 5.022000;
use strict;
use warnings;

sub new {
    bless [ ], shift
}
sub add {
    push @{shift()}, shift;
}
sub contents {
    @{shift()};
}
sub DESTROY {
    my $self = shift;
    print "$self is being destroyed ...\n";
    for ($self->contents) {
	print ' ', $_->name, " goes homeloss.\n";
    }
}

1;
