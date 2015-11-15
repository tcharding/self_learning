package Crypto::Stream;

use 5.022000;
use strict;
use warnings;

use Carp;
require Exporter;

use Crypt::Rijndael;
use Crypto::Base qw/ :all /;
use Crypto::Block qw/ :all /;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				      encrypt_aes_ctr
				      decrypt_aes_ctr
			      ) ],
		 );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

# uses 64 bit nonce and 64 bit counter 
sub encrypt_aes_ctr {
    my( $p, $key, $nonce, $counter ) = @_;
    my $ks = "";		# 16 bytes of key stream
    my $c;

    while (length($p) > 0) {
	if (length( $ks ) == 0) {
	    $ks = get_stream( $key, $nonce, $counter );
	    $counter++;
	}

	$c .= pack('H*', substr($ks, 0, 2)) ^ pack('A*', substr($p, 0, 1));

				# move along
	$ks = substr($ks, 2);	# hex
	$p = substr($p, 1);	# ascii
    }
    return $c;
}

# uses 64 bit nonce and 64 bit counter
sub decrypt_aes_ctr {
    my( $c, $key, $nonce, $counter ) = @_;
    my $ks = "";
    my $hex = unpack('H*', $c);
    my $p = "";
    
    while (length($hex) > 0) {
	if (length( $ks ) == 0) {
	    $ks = get_stream( $key, $nonce, $counter );
	    $counter++;
	}

	my $hchar = substr($p, 0, 2);	      # hex
	my $kchar = substr($ks, 0, 2); # hex
	my $xor = pack('H*', $kchar) ^ pack('H*', $hchar);
	$p .= pack('H*', substr($ks, 0, 2)) ^ pack('H*', substr($hex, 0, 2));

				# move along
	$ks = substr($ks, 2);	# hex
	$hex = substr($hex , 2); # hex
    }
    return hex_to_ascii(unpack('H*', $p));
}

# used by ctr mode to get keystream
sub get_stream {
    my( $key ,$nonce, $counter ) = @_;
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    
    my $hex = dec_to_64bit_hex( $nonce ) . dec_to_64bit_hex( $counter );
    my $c = $cipher->encrypt( hex_to_ascii( $hex ) );

    my $stream = unpack('H*', $c);
    return $stream;
}

# convert integer to 64 bit unsigned little endian hex string
sub dec_to_64bit_hex {
    my $dec = shift;
    my $hex = unpack('H*', pack('C*', $dec));
    while (length( $hex ) < 16 ) {
	$hex .= "00";
    }
    return $hex;
}
