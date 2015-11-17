#!/usr/bin/perl
use strict;
#use warnings;
use feature qw/say/;

#
# Crack an MT19937 seed using brute force
#

use Crypto::MT19937 qw/:all/;

my( $u, $d ) = (11, 0xFFFFFFFF16);
my( $s, $b ) = (7, 0x9D2C568016);
my( $t, $c ) = (15, 0xEFC6000016);

my $seed = &gen_seed;  # long run time
print "seed: $seed\n";
&seed( $seed );
my $v = extract_number();

my $guessed_seed = &attack_mt19937( $v );

print "Set 2 Challenge 22: ";
if ($seed == $guessed_seed) {
    print "Completed!\n";
} else {
    print "Failed\n";
}

sub attack_mt19937 {
    my $val = shift;
    my $max = 2**32;
    for (0 .. $max) {
	my $seed = mt19937_value( $_ );
	if ($seed == $val) {
	    return $_;
	}
    }
    return -1;

}

# simulate MT19937 first value of sequence
sub mt19937_value {
    my $y = shift;

    $y = $y ^ (($y >> $u) & $d);
    $y = $y ^ (($y << $s) & $b);
    $y = $y ^ (($y << $t) & $c);
    $y = $y ^ ($y >> 1);

    return int32( $y );
}


# Get the 32 least significant bits.
sub int32 {
    my $n = shift;
    return int(0xFFFFFFFF & $n)
}

sub gen_seed {
#    sleep(rand(5));
    my $date = `date --rfc-3339=ns`;
    chomp $date;
    my @s = split /\./, $date;
    @s = split /\+/, $s[1];
    return $s[0];
}
