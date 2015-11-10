#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# Break repeating-key XOR
#

use Crypto::Base qw( :all );
use Crypto::Vigenere qw(:all);
use MIME::Base64 qw(encode_base64 decode_base64);

my $file = "script/6.txt";
open my $fh, '<', $file or die "Cannot open file: $file\n";

my $asc = do { local( $/ ) ; <$fh> } ;
my $ciphertext = decode_base64($asc);
my $hex = unpack( 'H*', $ciphertext );

# say "Guessing key length using Friedman method";
my $max_keylen = 40;
my $nprint = 5;
&printn_sorted_by_value( $nprint, keylen_ic( $hex, $max_keylen ));
my $keylen = 29;		# from &keylen_ic

my $key = &build_key( $hex, $keylen );
$key = &ascii_to_hex( $key );
#print hex_to_ascii($key);# -> "Terminator X: Bring the noise";
#key = "5465726d696e61746f7220583a204272696e6720746865206e6f697365\n"; # &build_key

my $p = &repeating_xor( $hex, $key );
$p = &hex_to_ascii($p);
print "$p\n";

# multi character key repeating_xor, all inputs and outputs are in hex
sub repeating_xor {
    my( $in, $key ) = @_;
    my $klen = length( $key );
    my $out;

    my $ki = 0;
    while (length( $in ) > 0) {
    	$ki = $ki % $klen;
    	my $a = substr($key, $ki, 2);
    	my $b = substr($in, 0, 2);
    	$in = substr($in, 2);
    	$ki += 2;
    	$out .= unpack('H*',pack('H*', $a) ^ pack('H*', $b));
    }
    return $out;
}

# single char xor each block to find key character
sub build_key {
    my( $hex, $keylen ) = @_;
    my $trans_blocks = &transpose( $hex , $keylen);
    my( $key, $fail );

    $fail = 0;
    for (@$trans_blocks) {
	my $scx = &bruteforce_scx( $_ );
	&rate_msgs( $scx );
	my $top_hex = get_top_rated( $scx );
	if (defined $top_hex) {
	    for my $char (keys %$scx) {
		my $this_hex = $$scx{ $char }{ hex };
		if ( $this_hex eq $top_hex ) {
		    $key .= $char;
		}
	    }
	} else {
	    $key .= "*";
	    $fail = 1;
	}
    }
    return $key;
}


# # Resulting Key length: 29
# sub guess_keylen {
#     my $n_to_print = 5;		
#     say "Guessing key length using Friedman method";
#     &printn_sorted_by_value( $n_to_print, keylen_ic( $c, 40 ));
# }

sub printn_sorted_by_value {
    my( $n, $hash ) = @_;
    my $cnt;
    for (sort { $$hash{$b} <=> $$hash{$a} } keys %$hash) {
#	printf "klen: %-2s %s\n", $_, $$hash{$_};
	$cnt++;
	if ($cnt >= $n) {
	    last;
	}
    }
}
