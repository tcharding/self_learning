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

sub attack_padding_oracle {
    my( $iv, $c ) = &encryption_oracle;
    my $hex = unpack('H*', $c);
    my $first = substr( $hex, 0, 32 );
    my $second = substr( $hex, 32, 64 );
    my $rev = ""; # the result backwards
    
    for my $nbytes (1 .. 1) {
	for (32 .. 126) {
	    my $char = chr($_);
	    my $mod; # the ciphertext block we are building

	    my $i = 30;
	    $mod = substr( $first, 0, $i);
	    my $ct = pack('H*', substr($first, $i)); # ciphertext 
	    my $pt = pack('A*', $char);		     # plaintext
	    my $target = pack('C*', $nbytes);	     # target

#	    printf "%s %s %s\n", substr($first, $i), $pt, $target;
	    $mod .= unpack('H*', ($ct ^ $pt ^ $target));


	    # my $i = length($first) - ($_ * 2); # index into $first
	    # $mod = substr( $first, 0, $i);
	    # my $ct = pack('H*', substr($first, $i)); # ciphertext 
	    # my $pt = pack('A*', $char);		     # plaintext
	    # my $target = pack('C*', $nbytes);	     # target
	    # $mod .= unpack('H*', ($ct ^ $pt ^ $target));

	    if (&padding_oracle( $iv,  pack('H*', $mod) . pack('H*', $second))) {
		$rev .= $char;
#		last;
	    }
	}
    }
    if (defined $rev) {
	print scalar reverse $rev;
	say "";
    } else {
	print "Padding oracle attack failed!\n";
    }
    
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

sub padding_oracle {
    my( $iv, $c ) = @_;
    my $p = decrypt_aes_cbc( $c, $key, $iv );
#    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
#    $cipher->set_iv( $iv );
#    my $p = $cipher->decrypt( $c );

    return is_pad_correct( $p );
}

sub encryption_oracle {
    				# select input string
    my $r = int(rand(10));
#    my $p = pad( $a[$r] );
    my $p = decode_base64( $a[$r] );
    chomp $p;
    $p = pad($p);

    my $iv = pseudo_random_string;

#    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
#    $cipher->set_iv( $iv );
#    my $c = $cipher->encrypt( $p );

    my $c = encrypt_aes_cbc( $p, $key, $iv );
    return ($iv, $c);
}


