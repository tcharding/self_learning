package RaceHorse;
use parent qw(Horse);

use 5.022000;
use strict;
use warnings;

#
# Uses binary format to for permanent storage; SUB OPTIMAL
#

dbmopen (our %STANDINGS, "standings", 0666)
    or die "Connot access standings dbm: $!";

sub new {
    my $self = shift->SUPER::new( @_ );
    my $name = $self->name;
    my @standings = split ' ', $STANDINGS{$name} || "0 0 0 0";
    @$self{ qw(wins places shows losses)} = @standings;
    $self;
}

sub won { shift->{wins}++; };
sub placed { shift->{places}++; };
sub showed { shift->{shows}++; };
sub lost { shift->{losses}++; };

sub standings {
    my $self = shift;
    join ', ', map "$self->{$_} $_", qw(wins places shows losses);
}

sub DESTROY {
    my $self = shift;
    $STANDINGS{$self->name} = "@$self{qw(wins places shows losses)}";
    $self->SUPER::DESTROY if $self->can( 'SUPER::DESTROY' );
}
   

1;
