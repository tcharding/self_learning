#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;
use Crypto::Block qw/:all/;

#&test_parse_to_json;
#&test_profile_for;

my $key = &random_16_chars;

my $email = 'me@tobin.cc';
my $profile = &profile_for( $email );
print "Input: $email Profile: $profile\n";
#my $c = &encrypt( $profile, $key );
my $c = &input_and_encrypt( $email );

$c = attack( $c );

my $p = &decrypt( $c, $key );
$profile = &strip_padding( $p );
print "encypted/decrypted (key: $key): ";
print "$profile\n";
my $parsed = &parse_to_json( $profile );
print "$parsed\n";

# attack ECB encoded string
sub attack {
#    my $c = shift;
    my( $input, $formed, $hex );
    my( $norole, $admin );	# encrypted blocks
    
    $input = 'me@abbyte.com'; # 13 bytes
    $formed = &input_and_encrypt( $input );
    $hex = unpack('H*', $formed);
    $norole = substr($hex, 0, 64);

    $input = 'me@tobi.cc';	# gives [email=me@tobi.cc&uid=10&] as the first block
    $input .= pad( "admin", 16); # gives [admin......] as the second block
    $formed = &input_and_encrypt( $input );
    $hex = unpack('H*', $formed);

    $admin = substr($hex, 32, 32);
    my $combined = $norole . $admin;

    $formed = pack('H*', $combined);
    return $formed;
}

# simulate accepting user input and encrypting
sub input_and_encrypt {
    my $input = shift;
    my $profile = &profile_for( $input );
    return &encrypt( $profile, $key );
}

sub test_parse_to_json {
    my $tc_1 = "foo=bar&baz=qux&zap=zazzle";
    my $tc_2 = 'email=foo@bar.com&uid=10&role=user';

    my $parsed;
    $parsed = &parse_to_json( $tc_1 );
    print "tc1: $parsed\n";
    $parsed = &parse_to_json( $tc_2 );
    print "tc2: $parsed\n";
}

sub parse_to_json {
    my $s = shift;
    my $parsed;
    
    $parsed .= sprintf "%s", "{\n";
    for (split /&/, $s) {
	my( $k, $v ) = split /=/, $_;
	$parsed .= sprintf "\t%s:\t'%s',\n", $k, $v;
    }
    $parsed .= sprintf "%s", "}\n";
}

sub test_profile_for {
    my $tc_1 = 'foo@bar.com';
    my $tc_2 = 'foo@bar.com&role=admin';

    printf "testing profile_for\n";
    printf "1 (%s) %s\n", $tc_1, &profile_for( $tc_1 );
    printf "2 (%s) %s\n", $tc_2, &profile_for( $tc_2 );
}

sub profile_for {
    my $email = shift;
    my $options = index $email, "&";
    if ($options != -1) {
    	$email = substr( $email, 0, $options );
    }

    my $encoded = "email=" . $email . "&uid=10&role=user";
    return ( $encoded );
}

sub encrypt {
    my( $p, $k ) = @_;

    my $in = pad( $p, 16 );
    my $cipher = Crypt::Rijndael->new( $k, Crypt::Rijndael::MODE_ECB() );
    return $cipher->encrypt( $in );
}

sub decrypt {
    my( $c, $k ) = @_;
    
    my $cipher = Crypt::Rijndael->new( $k, Crypt::Rijndael::MODE_ECB() );
    return $cipher->decrypt( $c );
}


