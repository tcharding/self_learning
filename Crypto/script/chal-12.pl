#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Byte-at-a-time ECB decryption (Simple)
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;
use Crypto::Block qw/:all/;
use Crypto::Base qw/:all/;

# global data for encryption oracle
my $key = &pseudo_random_string( 16 ); # fixed unknown key
my $append = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg" .
    "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq" .
    "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK";
$append = decode_base64( $append );

# get block size
my $block_size = &guess_block_size;
print "Cipher block size: $block_size\n";

# decect ECB mode
my $input = "this is 16 bytes";
$input = $input . $input;
my $c = encryption_oracle( $input );
if (has_repeating_blocks( unpack('H*', $c)) != -1) {
    print "Cipher appears to be running in ECB mode\n";
} else {
    die "Cipher does not appear to be running in ECB mode, aborting ...\n";
}

my $p = &cryptanalize;
print "\n$p\n";

sub cryptanalize {
    my $p = "";
    my $bufsz = length( &encryption_oracle("") ); # how much data are we recovering
    
    for (1 .. $bufsz) {
	my $dict = &build_dictionary( "A" x ($bufsz - $_) . $p );
	my $c = &encryption_oracle( "A" x ($bufsz - $_) );
				# get first bufsz bytes (x2 because it's hex)
	my $hex = substr( unpack('H*', $c), 0, ($bufsz * 2)); 

	for (keys %$dict) {
	    if ($$dict{$_} eq $hex) {
		$p .= $_;
	    }
	}
    }
    return $p;
}

#    my $nrepeats = ($blockn * $block_size) + ($block_size - 1);
sub build_dictionary {
    my $s = shift;
    my %d;
    my $match = (length( $s ) + 1) * 2;

    my @chars = (32 .. 126);
    push @chars, 10;		# we need a newline also

    for (@chars) {
	my $char = chr($_);
	my $input = $s . $char;
	my $c = encryption_oracle( $input );
	my $hex = unpack('H*', $c);
	$d{$char} = substr($hex, 0, $match);
    }
    return \%d;
}

# guess block size from ciphertext encrypted by oracle 
sub guess_block_size {
    my $next = 0;
    my($s, $cnt) = ("", 0);
				# get base length, encrypt empty string
    my $len = length ( encryption_oracle( $s ) ); 

    while ($next <= $len) {
	$s .= "A";		# add one byte to input
	$next = length( encryption_oracle( $s ) );
    }
    return $next - $len;	# block size
}

#
# Encryption Oracle
#
sub encryption_oracle {
    my $m = shift;
    $m .= $append;
    $m = &pad( $m, 16 );
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    
    return $cipher->encrypt( $m );
}
