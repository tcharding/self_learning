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

# @a = qw(
# 	   aoauo
# 	   aou
# 	   aou
# 	   aoeuaoeuiao
# 	   iao
# 	   i
# 	   oaiaooai
# 	   iaoioii
# 	   aoi
# 	   aoiaoti
#    );
&test_oracles;
&attack_padding_oracle;

# 
sub attack_padding_oracle {
    my( $iv, $c ) = &encryption_oracle;
    my $c_hex = unpack('H*', $c);
				# TODO loop over blocks
    my $first = substr( $c_hex, 0, 32 );
    my $second = substr( $c_hex, 32, 32 );
    my $p = "";
    print "32******************************** ********************************\n";
    print "i:$first $second\n\n";

    my $found = 0;
    my $modcb;		       # modified ciphertext
    for my $nbytes (1 .. 16) {
	say "\nnbytes: $nbytes";
	my $modi = 32 - ($nbytes * 2); # byte index we are modifying
	
				# these three combined are the modified block 
	my $front = substr($first, 0, $modi); # bit still to do (hex)
	my $mod_byte;			      # the modified byte (bin)
	my $back = "";		      # bit we have done (hex)
	

	
				# bytes for XOR'ing
	my $ctb = pack('H*', substr($first, $modi, 2)); # ciphertext byte
	my $tpb = pack('C*', $nbytes);		# target padding byte
	
				# setup the bytes we already know
	my $lenp = length( $p );
	if ( $lenp > 0) {
	    print "xor result: ";
	    for (my $i = 0; $i < $lenp; $i++) {
		my $modify_pt = substr($p, $i, 1);
		my $already_modded = pack('A*', $modify_pt) ^ $ctb ^ $tpb;
		$back .= unpack('H*', $already_modded);
		printf "($modify_pt = %s) ", unpack('H*', $already_modded);
	    }
	    print "\n";
	    print "from last round: p-ascii/b-hex *$p* *$back*\n";
	} 

	$found = 0;
	# brute force guess plaintext byte
	for (1..16, 32..126) {	# padding characters plus printable characters
	    my $char = chr($_);	
	    my $gpb = pack('A*', $char);		    # guessed plaintext byte
	    my $mod_byte = $gpb ^ $ctb ^ $tpb;
	    
	    			# build modified block
	    $modcb = pack('H*', $front);      # front
	    $modcb .= $mod_byte;	      # add modified byte
	    # add back if defined
	    if ($back ne "") {
		$modcb .= pack('H*', $back);
	    }
	    $modcb .= pack('H*', $second);    # add second block

	    			# query the oracle
	    if (&padding_oracle( $iv, $modcb ) ){
		$p = $char . $p;
		$found = 1;
		printf "got char:*%s* (hex: %s)\n", $char, ascii_to_hex($char);
		last;		# end guess character loop
	    }
	} # end guess character loop
	last if ($found == 0); # end nbytes loop
    } # end nbytes loop
    if ($found == 0) {
	print_modcb("m:", $modcb);
	my $h = unpack('H*', $modcb);
	printf "m:%s %s\n", substr($h, 0, 32), substr($h, 32, 32);
	die "Failed to find character\n" 
    }
    
    printf "plaintext (starred):*%s*\n", $p;
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


