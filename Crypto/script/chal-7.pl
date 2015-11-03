#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypt::Rijndael;

my $key = "YELLOW SUBMARINE";
my $file = "script/7.txt";
open my $fh, '<', $file or die "Cannot open $file";

my $text = do { local( $/ ) ; <$fh> } ;
my $ciphertext = decode_base64($text);

my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
my $plaintext  = $cipher->decrypt($ciphertext);
print "$plaintext\n";
