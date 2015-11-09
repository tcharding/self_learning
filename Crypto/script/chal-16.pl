#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Data::Dumper;
use Crypto::Block qw/:all/;

my $prepend = "comment1=cooking%20MCs;userdata=";
my $append = ";comment2=%20like%20a%20pound%20of%20bacon";
my $key = &random_16_chars;

#my $c = fn1("fill;admin=true");
my $c = fn1("blah blah blah");
$c = attack_cbc( $c );

if (fn2( $c ) == 1) {
    print "true\n";
} else {
    print "false\n";
}

sub attack_cbc {
    my $c = shift;
    
    my $plaintext = "%20MCs;userdata=";
    my $desired =   ";admin=true;king";

    my $hex = unpack('H*', $c);
    my $first = substr($hex, 0, 32);
    my $rest = substr($hex, 32);	# 16 bytes
    $rest = pack('H*', $rest);
    
    my $formed;
    while (length( $plaintext ) > 0) {
	my( $a, $b ,$c );
	$a = pack('H*', substr($first, 0, 2));
	$b = pack('A*', substr($plaintext, 0, 1));
	$c = pack('A*', substr($desired, 0, 1));
	$formed .= ($a ^ $b ^ $c);
	# move all along one byte
	$plaintext = substr($plaintext, 1);
	$first = substr($first, 2);
	$desired = substr($desired, 1);
    }

    return $formed . $rest;
}

sub fn1 {
    my $s = shift;

    if ( ((index $s, "=") != -1) &&
	     ((index $s, ";") != -1)) {
	$s = "suspicious user input";
    }
    my $p = $prepend . $s . $append;
    print "$p\n";
    $p = pad( $p, 16 );
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
    my $c = $cipher->encrypt( $p );
}

sub fn2 {
    my $c = shift;
    my %properties;
    
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_CBC() );
    my $p = $cipher->decrypt( $c );

    for (split /;/, $p) {
	my( $k, $v ) = split /=/, $_;
	$properties{$k} = $v;
    }
    print Dumper(\%properties);
    for (keys %properties) {
	if ($_ eq "admin") {
	    if ( $properties{$_} eq "true") {
		return 1;	# true, admin = true
	    }
	}
    }
    return 0;			# false, admin != true
}
