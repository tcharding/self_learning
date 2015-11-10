#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Convert qw( :all );

use Data::Dumper;

my $s = "Burning 'em, if you ain't quick and nimble\n";
$s  .=  "I go crazy when I hear a cymbal";

my $exp;
$exp = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272";
$exp .= "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f";

my $key = ascii_to_hex("ICE");
my $hex = ascii_to_hex( $s );

my $c = &repeating_xor( $hex, $key );
				# check result
print "Set 1 Challenge 5: ";
if ($c eq $exp) {
    print "Completed!\n";
} else {
    print "Failed\n";
    print "$exp\n";
    print "$c\n";
}

# multi character key (hex) repeating_xor
sub repeating_xor {
    my( $hex, $key ) = @_;
    my $klen = length( $key );
    my $out;

    my $ki = 0;
    while (length( $hex ) > 0) {
    	$ki = $ki % $klen;
    	my $kbyte = substr($key, $ki, 2);
    	my $hbyte = substr($hex, 0, 2);
    	$hex = substr($hex, 2);
    	$out .= unpack('h*',pack('h*', $hbyte) ^ pack('h*', $kbyte));
    	$ki += 2;
    }
    return $out;
}
