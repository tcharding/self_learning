#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# The CBC padding oracle
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypto::Base qw/:all/;
use Crypto::Block qw/:all/;
use Crypto::Cipher qw/:all/;

use constant VERBOSE => 1;

my @a;				# supplied input strings
@a = qw( MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=
	 MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=
	 MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==
	 MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==
	 MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl
	 MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==
	 MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==
	 MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=
	 MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=
	 MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93
   );

my $key = pseudo_random_string;

#&test_oracles;
#&test_build_ciphertext;
&test_build_known;
&attack_padding_oracle;
#my $pad = chr(16) x 16;
#print "pad16: $pad\n";

sub attack_padding_oracle {
    my( $iv, $c ) = &encryption_oracle;
    my $hex = unpack('H*', $c);

    print "iv: $iv\n";
    
    my @blocks;
    my $size = 32;		# 16 bytes hex
    for (my $i = 0; $i < length( $hex ); $i += $size) {
	push @blocks, substr( $hex, $i, $size );
    }
    my $nblocks = @blocks;
    print "nblocks: $nblocks\n";
    say "";
    my $p;
    for (0 .. $nblocks-1) {
	$p .= "(block $_): ";
	$p .= &attack_block( $_, $iv, \@blocks, VERBOSE );
	$p .= "\n";
    }
    print $p;
}

# attack blockn (i.e modify block blockn-1)
sub attack_block {
    my( $blockn, $iv, $blocks, $verbose ) = @_;
    my $nblocks = @$blocks;

    my $p = "";
    for my $byte (1..16) {
	my $found = 0;
	for (1 .. 255) {
#	for (1..16, 32..126) {	# padding plus printable characters
	    my $char = chr($_);
	    my $mod_block;
	    if ($blockn == 0) {
		$iv = mod_iv( $iv, $char, $p);
		if (length($iv) != 16) { die "iv error"; }
	    } else {
		$mod_block = build_modified_block( $$blocks[$blockn - 1], $char, $p);
	    }
	    my $c = build_ciphertext( $blocks, $mod_block, $blockn );
	    if (&padding_oracle( $iv, $c ) ) {
		$p = $char . $p;
		$found = 1;
		if ($verbose == VERBOSE) {
		    print "$byte: we got char: $char p: *$p*\n";
		}
		last;
	    }
	}
	if ($found == 0) {
	    $p .= "*";
#	    return "Failed to find char, p: *$p*";
	}
    }
    return $p;
}

# modify IV to attack first block
sub mod_iv {
    my( $iv, $char, $p ) = @_;
    my $hex = ascii_to_hex( $iv );
    my $mod = build_modified_block( $hex, $char, $p );
    
    return hex_to_ascii( $mod );
}
sub build_modified_block {
    my( $block, $char, $p ) = @_;

    
    my $pad = length($p) + 1;	# pad length and padding byte value in decimal
    my $i = 32 - ($pad * 2);

				# get xor bytes
    my( $pt, $ct, $pd );	
    $pt = pack('A*', $char);
    $pd = pack('C*', $pad);
    $ct = pack('H*', substr( $block, $i, 2 ));
    
    				# build modified block
    my $mod_block = substr($block, 0, $i);
    $mod_block .= unpack('H*', ($pt ^ $ct ^ $pd));
    $mod_block .= build_known( $p, $pad, substr( $block, $i+2));

    return $mod_block;
}

# build byte by byte XOR of ciphertext byte, plaintext byte, and padding byte
sub build_known {
#   inputs: ( ascii, decimal, hex)
    my( $plaintext, $pad, $ciphertext) = @_;
    my $len = length( $plaintext );
    my $bin;

    my $pd = pack('C*', $pad);
    for (my $i = 0; $i < $len; $i++) {
	my $pt = pack('A*', substr($plaintext, $i, 1));
	my $ct = pack('H*', substr($ciphertext, $i*2, 2)); # hex
	$bin .= $pd ^ $pt ^ $ct;
    }
    #    printf "build_known: %s\n", unpack('H*', $bin);
    unless (defined $bin) {
	return "";
    } 
    return unpack('H*', $bin);	
}

# shallow testing
sub test_build_known {
    my $a = &build_known( "a", 0x01, "A0" );
    my $b = &build_known( "b", 0x01, "B0" );
    my $c = &build_known( "c", 0x01, "C0" );
    
    my $ab = &build_known( "ab", 0x01, "A0B0" );
    my $abc = &build_known( "abc", 0x01, "A0B0C0" );

    if ($ab ne ($a . $b)) {
	print "Fail ab\n";
    }
    if ($abc ne ($a . $b . $c)) {
	print "Fail abc\n";
    }
}

# combine blocks into ciphertext
sub build_ciphertext {
    my( $blocks, $mod_block, $blockn )= @_;
    my $c;
    my $nblocks = @$blocks;

    if ($blockn == 0) {
	$c = $$blocks[0];	
    } elsif ($blockn == 1) {
	$c = $mod_block;
	$c .= $$blocks[1];
    } else {
				# add front blocks
	for (0 .. $blockn - 2) {
	    $c .= $$blocks[$_];
	}
	$c .= $mod_block;
	$c .= $$blocks[$blockn];
    }

    return pack('H*', $c);
}

sub test_build_ciphertext {
    my( $iv, $c ) = &encryption_oracle;
    my $hex = unpack('H*', $c);

    my @blocks;
    my $size = 32;		# 16 bytes hex
    for (my $i = 0; $i < length( $hex ); $i += $size) {
	push @blocks, substr( $hex, $i, $size );
    }
    				# test build when attacking block 0
    my $built = build_ciphertext( \@blocks, $blocks[0], 0);
    $built = unpack('H*', $built);
    my $test = $blocks[0];
    if ($built ne $test) {
	die "t_build_ciphertext failed (block 0)\n";
    }
				# test build when attacking block 1
    $built = build_ciphertext( \@blocks, $blocks[0], 1);
    $built = unpack('H*', $built);
    $test = $blocks[0] . $blocks[1];
    if ($built ne $test) {
	die "t_build_ciphertext failed (block 1)\n";
    }

    				# test build when attacking block 2
    $built = build_ciphertext( \@blocks, $blocks[1], 2);
    $built = unpack('H*', $built);
    $test = $blocks[0] . $blocks[1] . $blocks[2];
    if ($built ne $test) {
	die "t_build_ciphertext failed (block 2)\n";
    }
}

#
# Oracles
#

# decrypt ciphertext, verify padding and return true if correct, false if not
sub padding_oracle {
    my( $iv, $c ) = @_;
    my $p = decrypt_aes_cbc( $c, $key, $iv );

    return is_pad_correct( $p );
}

# select string at random, pad and encrypt with AES CBC mode
sub encryption_oracle {
    my $r = int(rand(10));	
    #    my $p = pad( decode_base64( $a[$r] ), 16);
    my $p = pad("000009ith my rag-top down so my hair can blow", 16);
    print "0         1         2         3         4         5\n";
    print "012345678901234567890123456789012345678901234567890123456789\n";
    print "$p\n";
    
    my $iv = &pseudo_random_string;
    my $c = encrypt_aes_cbc( $p, $key, $iv );

    return ($iv, $c);
}

sub test_oracles {
    for (1..20) {
	my( $iv, $c ) = &encryption_oracle;
	#printf "iv:%s c:%s\n", $iv, unpack('H*', $c);
	unless (padding_oracle( $iv, $c )) {	
	    print "verification failed\n";
	}
    }
}
