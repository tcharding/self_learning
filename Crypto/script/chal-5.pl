#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( :all );
use Crypto::Analysis qw(:all);

use Data::Dumper;

my $input = "Burning 'em, if you ain't quick and nimble\n";
$input  .=  "I go crazy when I hear a cymbal";

my $out;
$out = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272";
$out .= "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f";
$out = uc $out;

my $key;
for (split //, "ICE") {
    $key .= byte_val( $_ );
}
my $m = decode_ascii( $input );

my $c = &repeating_xor( $m, $key );
$c = encode_hex( $c );
				# check result
print "Set 1 Challenge 5: ";
if ($c eq $out) {
    print "Completed!\n";
} else {
    print "Failed\n";
    print "$out\n";
    print "$c\n";
}
