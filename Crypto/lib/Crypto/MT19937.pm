package Crypto::MT19937;

use 5.022000;
use strict;
#use warnings;
use Math::BigInt;

#
# Implement the MT19937 Mersenne Twister RNG
#

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				      seed
				      extract_number
				      get_state
			      )
			    ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';


my @state;
my $index = 0;

my( $w, $n, $m, $r ) = (32, 624, 397, 31);

my( $u, $d ) = (11, 0xFFFFFFFF);
my( $s, $b ) = (7, 0x9D2C5680);
my( $t, $c ) = (15, 0xEFC60000);
my $a = 0x9908B0DF;
my $l = 18;
my $f = 1812433253;

my $lower_mask = 0x80000000;
my $upper_mask = 0x7fffffff;

# for dev
sub get_state {
    return \@state;
}

sub seed {
    my $seed = shift;
				# clear state
    @state = ();
    $index = 0;

    push @state, $seed;
    for ( 1 .. $n-1 ) {
	$state[$_] = int32( $f * ($state[$_ - 1] ^ ($state[$_ - 1] >> ($w-2))) + $_);
    }
}

# Extract a tempered value based on $state[index], calling twist() every n numbers
sub extract_number {
    if ($index >= $n) {
	if ($index > $n) {
	    die "Generator was not seeded\n";
	}
	&twist();
    }
    my $y = $state[$index];
    $y = $y ^ (($y >> $u));
    $y = $y ^ (($y << $s) & $b);
    $y = $y ^ (($y << $t) & $c);
    $y = $y ^ ($y >> $l);

    $index++;		
    return int32( $y );
}

# Generate the next n values from the series x_i 
sub twist {
    for (0 .. $n-1) {
	my $x = ($state[$_] & $upper_mask) + ($state[($_ + 1) % $n] & $lower_mask);
	my $xA = $x >> 1;
	if (($x % 2) != 0) {
	    $xA = $xA ^ $a;
	}
	$state[$_] = $state[($_ + $m) % $n] ^ $xA;
    }
    $index = 0;
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
