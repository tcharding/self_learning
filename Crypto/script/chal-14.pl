#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Crypto::Block qw/:all/;
use MIME::Base64 qw(encode_base64 decode_base64);

my $TWINBLOCKS = "yellow submarineyellow submarine";
my $key = &random_16_chars;
my $append = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg" .
    "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq" .
    "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK";

print &cryptanalize;

sub cryptanalize {
    my $blocks = 3;		# make this bigger as needed
    my $bufsz = $blocks * 16;
    my $p = "";
    
    my $cnt;
    for (1 .. $bufsz) {
	my $allas = "A" x ($bufsz - $_);
	my $dict = &build_dictionary( $allas . $p );
	my( $c, $hex, $start_of_twin );
	my $enc_input = $TWINBLOCKS . "A" x ($bufsz - $_);
	{
	    $c = encryption_oracle( $enc_input );
	    $hex = unpack('H*', $c);
	    $start_of_twin = &has_repeats( $hex );
	    redo if $start_of_twin eq 0;
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
    return $p;
}

sub build_dictionary {
    my $s = shift;
    my $match = (length( $s ) + 1) * 2;
    my $long = $TWINBLOCKS . $s; # force repeated blocks
    my %d;

    my @chars = (32 .. 126);
    push @chars, 10;		# we need a newline also

    my $cnt;
    for (@chars) {
	my $char = chr($_);
	my $c = encryption_oracle( $long . $char );
	my $hex = unpack('H*', $c);
	my $index = (&has_repeats( $hex ));
	redo if $index == 0;	# try again, not block aligned

	$index += 64;		# skip TWIN BLOCKS
	$d{$char} = substr($hex, $index, $match);
    }
    return \%d;
}
 
sub encryption_oracle {
    my $input = shift;
				# random amount of random data
    my( $min, $max ) = ( 97, 122 );

    my $amount = int(rand(($max - $min))) + $min;
    my $random;
    
    until ($random) {
    	for (1..$amount) {
    	    $random .= chr($_);
    	}
    }
#    my $random = random_16_chars; # just for dev
    
    my $message = $random . $input . decode_base64($append);

    $input = &pad( $message, 16 );
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    
    return $cipher->encrypt( $input );
}
