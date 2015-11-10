#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Data::Dumper;
use Crypt::Rijndael;
use Crypto::Block qw/:all/;
use Crypto::Base qw/:all/;

#
# CBC bitflipping attacks
#
 
my $prepend = "comment1=cooking%20MCs;userdata=";
my $append = ";comment2=%20like%20a%20pound%20of%20bacon";
my $key = &pseudo_random_string( 16 );

#my $c = fn1("fill;admin=true");
my $c = &input_and_encrypt("blah blah blah");
$c = attack_cbc( $c );

print "Set 2 Challenge 16: ";
if (has_admin_perms( $c ) == 1) {
    print "Completed!\n";
} else {
    print "Failed\n";
    print "maybe you should uncomment print Dumper(..)"
}

sub attack_cbc {
    my $c = shift;
    
    my $change_this = "%20MCs;userdata=";
    my $to_this = ";admin=true;king";

    my $hex = unpack('H*', $c);
    my $first = substr($hex, 0, 32);
    my $rest = substr($hex, 32);	# 16 bytes
    
    my $modified;
    my $len = length($change_this);
    for (my $i = 0; $i < $len; $i++) {
	my $a = pack('H*', substr($first, $i*2, 2));
	my $b = pack('A*', substr($change_this, $i, 1));
	my $c = pack('A*', substr($to_this, $i, 1));
	$modified .= ($a ^ $b ^ $c); # xor magic
    }
    $modified .= pack('H*', $rest);
}

# simulate accepting user data and encrypting
sub input_and_encrypt {
    my $s = shift;
				# check for malicious input
    if ( ((index $s, "=") != -1) &&
	     ((index $s, ";") != -1)) {
	$s = "suspicious user input";
    }
    my $p = $prepend . $s . $append;

    #encrypt_aes_cbc( $p, $key ); 
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
    my $c = $cipher->encrypt( pad( $p, 16 ) );
}

sub has_admin_perms {
    my $c = shift;
    my %properties;

#    my $p = decrypt_aes_cbc( $c, $key ); 
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
    my $p = $cipher->decrypt( $c );

    for (split /;/, $p) {
	my( $k, $v ) = split /=/, $_;
	$properties{$k} = $v;
    }
#    print Dumper(\%properties);
    for (keys %properties) {
	if ($_ eq "admin") {
	    if ( $properties{$_} eq "true") {
		return 1;	# true, admin = true
	    }
	}
    }
    return 0;			# false, admin != true
}
