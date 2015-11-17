package Crypto::Prng;

use 5.022000;
use strict;
use warnings;

use Carp;

#
# Pseudo Random Number Generator
#
# OO implementation of the Mersenne Twister (MT19937)
#

our $VERSION = '0.01';

my( $w, $n, $m, $r ) = (32, 624, 397, 31);

my( $u, $d ) = (11, 0xFFFFFFFF);
my( $s, $b ) = (7, 0x9D2C5680);
my( $t, $c ) = (15, 0xEFC60000);
my $a = 0x9908B0DF;
my $l = 18;
my $f = 1812433253;

my $lower_mask = 0x80000000;
my $upper_mask = 0x7fffffff;

# constructor
sub new {
    my( $class ) = @_;
    my $self = {State => [], Index => 0};
    bless $self, $class;
}

# dev only 
sub state {
    #    $_[0]->{State};
    my $self = shift;
    $self->{State};
}

sub seed {
    ref(my $self = shift) or croak "instance variable needed";
    my $seed = shift;
				# clear state
    $self->{State} = [];
    $self->{Index} = 0;

    my $state = $self->{State};	# for convenience 
    push @$state, $seed;
    for ( 1 .. $n-1 ) {
	$$state[$_] = int32(
	    $f * ($$state[$_ - 1] ^ ($$state[$_ - 1] >> ($w-2))) + $_
	);
    }
}

# Extract a tempered value based on $state[index], calling twist() every n numbers
sub extract_number {
    ref(my $self = shift) or croak "instance variable needed";
    my $seed = shift;
    my $state = $self->{State};	# for convenience
    
    if ($self->{Index} >= $n) {
	if ($self->{Index} > $n) {
	    die "Generator was not seeded\n";
	}
	$self->twist();
    }

    my $y = $$state[$self->{Index}];
    $y = $y ^ (($y >> $u));
    $y = $y ^ (($y << $s) & $b);
    $y = $y ^ (($y << $t) & $c);
    $y = $y ^ ($y >> $l);

    $self->{Index}++;		
    return int32( $y );
}

# Generate the next n values from the series x_i 
sub twist {
    ref(my $self = shift) or croak "instance variable needed";
    my $state = $self->{State};	# for convenience
    
    for (0 .. $n-1) {
	my $x = ($$state[$_] & $upper_mask) + ($$state[($_ + 1) % $n] & $lower_mask);
	my $xA = $x >> 1;
	if (($x % 2) != 0) {
	    $xA = $xA ^ $a;
	}
	$$state[$_] = $$state[($_ + $m) % $n] ^ $xA;
    }
    $self->{Index} = 0;
}
# Get the 32 least significant bits.
sub int32 {
    my $n = shift;
    return int(0xFFFFFFFF & $n)
}


1;
__END__

=head1 NAME

Crypto:: -

=head1 SYNOPSIS

  use Crypto::;

=head1 DESCRIPTION

=head2 EXPORT

=head1 SEE ALSO

=head1 AUTHOR

Tobin Harding, E<lt>me@tobin.ccE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2015 by Tobin Harding

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.22.0 or,
at your option, any later version of Perl 5 you may have available.


=cut
