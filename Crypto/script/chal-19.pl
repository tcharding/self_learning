#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;
use autodie;

#
# Break fixed-nonce CTR mode using substitions
#

use MIME::Base64 qw(encode_base64 decode_base64);
use Crypto::Base qw( :all );
use Crypto::Stream qw( :all );

my $key = &pseudo_random_string;
my $nonce = 0;
my @ciphertexts;

my $file = "script/18.txt";
open my $fh, '<', $file;
while (<$fh>) {
    chomp;
    my $counter = 0;
    my $p = decode_base64( $_ );
    my $c = encrypt_aes_ctr( $p, $key, $nonce, $counter );
    push @ciphertexts, $c;
}

my $num = @ciphertexts;
print "num: $num\n";
