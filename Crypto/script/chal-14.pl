#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Byte-at-a-time ECB decryption (Harder)
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;
use Crypto::Block qw/:all/;
use Crypto::Base qw/:all/;

use constant TWINBLOCKS => "yellow submarineyellow submarine";

my $key = &pseudo_random_string;
my $append = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg" .
    "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq" .
    "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK";

&cryptanalize;

sub cryptanalize {
    my $blocks = 3;		# make this bigger as needed
    my $bufsz = $blocks * 16;
    my $p = "";
    
    my $cnt;
    for (1 .. $bufsz) {
	my $dict = &build_dictionary( "A" x ($bufsz - $_) . $p );
	my $input = TWINBLOCKS . "A" x ($bufsz - $_);

	my( $c, $hex, $start_of_twin );
				# keep encrypting until block aligned
	{
	    $c = encryption_oracle( $input );
	    $hex = unpack('H*', $c);
	    $start_of_twin = has_repeating_blocks( $hex );
	    redo if $start_of_twin == -1;
	}

	$start_of_twin += 64;		# skip TWINBLOCKS
	my $block = substr( $hex, $start_of_twin, ($bufsz * 2) );
	for (keys %$dict) {
	    if ($$dict{$_} eq $block) {
		$p .= $_;
		print "$_\n";
	    }
	}
    }
    print "$p";
}

sub build_dictionary {
    my $s = shift;
    my $match = (length( $s ) + 1) * 2;
    my $long = TWINBLOCKS . $s; # force repeated blocks
    my %d;

    my @chars = (32 .. 126);
    push @chars, 10;		# we need a newline also

    my $cnt;
    for (@chars) {
	my $char = chr($_);
	my $c = encryption_oracle( $long . $char );
	my $hex = unpack('H*', $c);
	my $index = (&has_repeating_blocks( $hex ));
	redo if $index == -1;	# try again, not block aligned

	$index += 64;		# skip TWIN BLOCKS
	$d{$char} = substr($hex, $index, $match);
    }
    return \%d;
}
 
sub encryption_oracle {
    my $input = shift;
    my( $min, $max ) = ( 8, 32 ); 

    my $r = pseudo_random_string( &pseudo_random_int( $min, $max ));
    my $message = $r . $input . decode_base64($append);

    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    $input = &pad( $message, 16 );
    $cipher->encrypt( $input );
}
