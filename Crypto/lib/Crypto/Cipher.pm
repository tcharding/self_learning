package Crypto::Cipher;

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
				      encrypt_aes_cbc decrypt_aes_cbc
			      ) ],
		 );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

# encrypt using AES in CBC mode
sub encrypt_aes_cbc {
    my( $plaintext, $key, $iv ) = @_;
    # sanity checks

    $iv = &pseudo_random_string( 16 ) unless $iv;
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    if( length( $plaintext) % 16 != 0) {
	die "encrypt_aes_cbc block not multiple of block size (16 bytes)\n";
    }
    				# hexify inputs
    my $blocks = split_string_into_blocks( $plaintext, 16 );	# 16 byte blocks
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    				# now chain block cipher 
    my( $bin, $ciphertext );
    my $feedback = pack( 'A*', $iv );
    for (@$blocks) {
	my $input = $feedback ^ pack('A*', $_);
	my $bin = $cipher->encrypt( $input ); # encrypt as hex string (As 128)
	$ciphertext .= $bin;
	$feedback = $bin;
    }

    return $ciphertext;
}

# decrypt using AES in CBC mode
sub decrypt_aes_cbc {

    my( $ciphertext, $key, $iv ) = @_;
    $iv = &pseudo_random_string( 16 ) unless $iv;
				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    				# hexify input
    my $blocks = split_bin_into_blocks( $ciphertext, 16 );
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );

    		# now chain block cipher mode
    my $feedback = pack( 'A*', $iv );
    my $plaintext;
    for (@$blocks) {
	$plaintext .=  $cipher->decrypt( $_ ) ^ $feedback;
	$feedback = $_;
    }
    #    $plaintext = strip_padding( $plaintext, 16 );
    return $plaintext;
}
