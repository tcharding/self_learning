package Animal;
use parent qw(LivingCreature);

use 5.022000;
use strict;
use warnings;

our $VERSION = '0.01';

sub new {
    my( $class, $name ) = @_;
    my $self = {Name => $name, Color => $class->default_color };
    bless $self, $class;
}

sub default_color { 'brown' };

sub color {
    my $self = shift;
    $self->{Color};
}

sub set_color {
    my $self = shift;
    $self->{Color} = shift;
}

sub set_name { $_[0]->{Name} = $_[1] };

sub name {
    my $either = shift;
    ref $either
	? $either->{Name}		# it's an instance 
	: "An unnamed $either";	# it's a class
}

sub eat {
    my( $either, $food ) = @_;
    print $either->name, " eats $food.\n";
}


sub sound {
    die 'You must define sound in subclass';
}

sub DESTROY {
    my $self = shift;
    print '[', $self->name, " has died.]\n";
}

1;
