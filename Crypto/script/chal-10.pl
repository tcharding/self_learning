#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Implement CBC mode
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;
use Crypto::Block qw/:all/;

my $key = "YELLOW SUBMARINE";
my $iv = "THIS IS 16 BYTES";

my $plaintext = "this is the top secret message";
my $ciphertext = encrypt( $plaintext, $key, $iv );
my $washed = decrypt( $ciphertext, $key, $iv );

print "Set 2 Challenge 10: ";
if ( $plaintext eq $washed ) {
    print "Completed!\n";
} else {
    print "Failed\n";
    print "expected: $plaintext\n";
    print "We got:   $washed\n";
}

sub encrypt {
    my( $plaintext, $key, $iv ) = @_;
				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
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
sub decrypt {

    my( $ciphertext, $key, $iv ) = @_;
				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    				# hexify inputs
    my $blocks = split_bin_into_blocks( $ciphertext, 16 );
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );

    		# now chain block cipher mode
    my $feedback = pack( 'A*', $iv );
    my $plaintext;
    for (@$blocks) {
	$plaintext .=  $cipher->decrypt( $_ ) ^ $feedback;
	$feedback = $_;
    }
    return &strip_padding( $plaintext );
}

