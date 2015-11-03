#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use MIME::Base64;

my( $bin, $bits, $hex, $base64, $ascii);

# base64 -> bin -> base64
$bin = decode_base64( $base64 );
$base64 = encode_base64( $bin );

# hex -> bin -> hex
$bin = pack( 'H*', $hex );
$hex = unpack( 'H*', $bin );

# hex -> decimal
my $decimal = hex( $hex );

# decimal -> hex
my $hex = sprintf "%x", $decimal;

# hex -> ascii
sub hex_to_ascii {
    my $hex = shift;
    $hex =~ s/(([0-9a-f][0-9a-f])+)/pack('H*', $1)/ie;
    return $hex;
}

# ascii -> hex
sub ascii_to_hex {
    my $s = shift;
#    $s =~ s/((.)+)/unpack('h*', $1)/ie;
    return sprintf(%x, $s);
}

# num -> char -> num
$num  = ord( $char );
$char = chr( $num );
