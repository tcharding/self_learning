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

&test_oracles;
&t_build_known_bin;
&attack_padding_oracle;

sub attack_padding_oracle {
    my( $iv, $c ) = &encryption_oracle;
    my $hex = unpack('H*', $c);

    my @blocks;
    my $size = 32;
    while( length( $hex ) >= $size ) {
	my $block = substr( $hex, 0, $size );
	$hex = substr( $hex, $size );
	push @blocks, $block;
    }
   
    my $s = attack_first_block($iv, $blocks[0], $blocks[1] );
    print "$s\n";
}

sub attack_first_block {
    my( $iv, $first, $second ) = @_;

    my $p = "";

    my $found;
    for my $byte (1 .. 16) {
	my $modi = 32 - ($byte * 2); # byte index we are modifying
	
	my $front = substr($first, 0, $modi); # bit still to do (hex)
	my $mod = substr($first, $modi, 2);   # ciphertext byte to modify
	my $back = "";			      # back of block

	if (length( $p ) > 0) {		       
	    my $b = substr($first, $modi + 2); # +2 for mod_byte
	    $back = build_known( $p, $byte, $b );
	}
	
	my $ct = pack('H*', $mod);    # ciphertext byte
	my $pd = pack('C*', $byte); # target padding byte
	$found = 0;		      
	for (1..16, 32..126) {	# padding characters plus printable characters
	    my $char = chr($_);	
	    my $pt = pack('A*', $char);	# guessed plaintext byte

				# build modified block
	    my $mod_block = pack('H*', $front); # front
	    $mod_block .= $pt ^ $ct ^ $pd;;  # add modified byte
	    if ($back ne "") {		     # add back if defined
		$mod_block .= pack('H*', $back);  
	    }				     
	    $mod_block .= pack('H*', $second);   # add second block

	    			# query the oracle
	    if (&padding_oracle( $iv, $mod_block ) ){
		$p = $char . $p;
		$found = 1;
#		printf "got char:*%s* (hex: %s)\n", $char, ascii_to_hex($char);
	    }
	    last if ($found == 1); # end guess character loop
	} # end guess character loop
	last if ($found == 0); # end byte loop
    } # end byte loop
    if ($found == 0) {
	die "Error, char unfound\n";
    }
    return $p;
}
# 

# build byte by byte XOR of cx, p, pad. Return hex.
sub build_known {
    my( $plaintext, $pad, $cx) = @_;
    my $len = length( $plaintext );
    my $bin;

    my $pd = pack('C*', $pad);
    for (my $i = 0; $i < $len; $i++) {
	my $pt = pack('A*', substr($plaintext, $i, 1));
	my $ct = pack('H*', substr($cx, $i*2, 2)); # 2 because it's hex
	my $b = $pd ^ $pt ^ $ct;
	if ($i == 0) {
	    $bin = $b;
	} else {
	    $bin .= $b;
	}
    }
#    printf "build_known: %s\n", unpack('H*', $bin);
    return unpack('H*', $bin);
}

sub t_build_known_bin {
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
sub print_modcb {
    my( $s, $modcb ) = @_;
    my $h = unpack('H*', $modcb);
    printf "%s%s %s\n", $s, substr($h, 0, 32), substr($h, 32, 32);

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

# decrypt ciphertext, verify padding and return true if correct, false if not
sub padding_oracle {
    my( $iv, $c ) = @_;
    my $p = decrypt_aes_cbc( $c, $key, $iv );
#    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
#    $cipher->set_iv( $iv );
#    my $p = $cipher->decrypt( $c );
    return is_pad_correct( $p );
}

# select string at random, pad and encrypt with AES CBC mode
sub encryption_oracle {
    my $r = int(rand(10));	
    my $p = &pad( &decode_base64( $a[$r] ) );
    my $iv = &pseudo_random_string;
    my $c = encrypt_aes_cbc( $p, $key, $iv );
#    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
#    $cipher->set_iv( $iv );
#    my $c = $cipher->encrypt( $p );
    return ($iv, $c);
}


#
#
# Archive
# 
sub attack_block_iv {
    my ($iv, $block) = @_;
    $iv = ascii_to_hex( $iv );
    my $p = "";

    my $found;
    for my $byte (1 .. 16) {
	my $modi = 32 - ($byte * 2); # byte index we are modifying
	
	my $front = substr($iv, 0, $modi); # bit still to do (hex)
	my $mod = substr($iv, $modi, 2);   # ciphertext byte to modify
	my $back = "";			      # back of block

	if (length( $p ) > 0) {		       
	    my $b = substr($iv, $modi + 2); # +2 for mod_byte
	    $back = build_known( $p, $byte, $b );
	}
	
	my $ct = pack('H*', $mod);    # ciphertext byte
	my $pd = pack('C*', $byte); # target padding byte
	$found = 0;		      
	for (1..16, 32..126) {	# padding characters plus printable characters
	    my $char = chr($_);	
	    my $pt = pack('A*', $char);	# guessed plaintext byte

				# build modified block
	    my $mod_block = pack('H*', $front); # front
	    $mod_block .= $pt ^ $ct ^ $pd;;  # add modified byte
	    if ($back ne "") {		     # add back if defined
		$mod_block .= pack('H*', $back);  
	    }
	    $iv = unpack('H*', $mod_block);
	    $iv = hex_to_ascii( $iv );
	    
	    # query the oracle
	    if (&padding_oracle( $iv, $block ) ){
		$p = $char . $p;
		$found = 1;
#		printf "got char:*%s* (hex: %s)\n", $char, ascii_to_hex($char);
	    }
	    last if ($found == 1); # end guess character loop
	} # end guess character loop
	last if ($found == 0); # end byte loop
    } # end byte loop
    if ($found == 0) {
	die "Error, char unfound\n";
    }
    return $p;

}
