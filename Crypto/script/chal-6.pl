#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( decode_b64 encode_ascii decode_ascii);
use Crypto::Analysis qw(:all);
use Crypto::Keylen qw(:all);
use Data::Dumper;

				# get input data
my $file = "script/6.txt";
open my $fh, '<', $file or die "Cannot open file: $file\n";

my $input;
while ( <$fh> ) {
    chomp;
    $input .= $_;
}
my $c = verify_and_decode( $input );

#my $n_to_print = 5;		# configure output 
#say "Guessing key length using edit distance method";
#&printn_sorted_by_value( $n_to_print, keylen_ed( $c, 40 ));

#say "Guessing key length using Friedman method";
#&printn_sorted_by_value( $n_to_print, keylen_ic( $c, 40 ));
#
# Resulting Key length: 29
my $KEYLEN = 29;
my $trans_blocks = &transpose( $c , $KEYLEN);

my( $key, $fail );
for (@$trans_blocks) {
    my $scx = &bruteforce_scx( $_ );
    &rate_msgs( $scx );
    my $top_rated = &get_top_rated( $scx );
    my $num = @$top_rated;
    if ( $num == 1 ) {
	my $m = pop @$top_rated;
	$key .= $$scx{ $m }{ key };
    } else {
	print "found $num top_rated\n";
	$fail = 1;
	$key .= "*";
    }
}
#if ($fail == 0) {
    print "($fail): $key\n";	
#}

sub printn_sorted_by_value {
    my( $n, $hash ) = @_;
    my $cnt;
    for (sort { $$hash{$b} <=> $$hash{$a} } keys %$hash) {
	printf "klen: %-2s %s\n", $_, $$hash{$_};
	$cnt++;
	if ($cnt >= $n) {
	    last;
	}
    }
}

sub verify_and_decode {
    my $input = shift;
    my $ilen = length( $input );
    if( ($ilen * 6) % 8 != 0 ) {
	warn "input is not byte aligned, will need padding";
    }
    decode_b64($input);
}

