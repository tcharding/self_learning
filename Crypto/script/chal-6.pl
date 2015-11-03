#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( decode_b64 encode_ascii decode_ascii dump_bits);
use Crypto::Analysis qw(:all);
use Crypto::Keylen qw(:all);
use Data::Dumper;

my $c = &get_input;
my $KEYLEN = 29;		# see &guess_keylen
&build_key( $c, $KEYLEN );

#for ( 2 .. 40 ) {
#    &build_key( $c, $_ );
#}

# single char xor each block to find key character
sub build_key {
    my( $c, $keylen ) = @_;
    my $trans_blocks = &transpose( $c , $keylen);
    my( $key, $fail );

    for (@$trans_blocks) {
	my $scx = &bruteforce_scx( $_ );
	&rate_msgs( $scx );
	# for my $m (keys %$scx) {
	#     my $p = encode_ascii($m);
	#     #	dump_bits($m);
	#     print( "$$scx{$m}{key} ($$scx{$m}{rating}) $p\n");
	# }
 
	my $top_rated = &get_top_rated( $scx );
	my $num = @$top_rated;
	if ( $num == 1 ) {
	    my $m = pop @$top_rated;
	    $key .= $$scx{ $m }{ key };
	} else {
	    print "found $num top_rated\n";
	    for my $m (@$top_rated) {
		my $p = encode_ascii($m);
		print( "$$scx{$m}{key} ($$scx{$m}{rating}) $p\n");
	    }
	    say "";
	    $fail = 1;
	    $key .= "*";
	}
    }
    print "k: $keylen ($fail): $key\n";	

}


# Resulting Key length: 29
sub guess_keylen {
    my $n_to_print = 5;		
    say "Guessing key length using Friedman method";
    &printn_sorted_by_value( $n_to_print, keylen_ic( $c, 40 ));
}

# get input data
sub get_input {
    my $file = "script/6.txt";
    open my $fh, '<', $file or die "Cannot open file: $file\n";
    
    my $input;
    while ( <$fh> ) {
	chomp;
	$input .= $_;
    }
    my $ilen = length( $input );
    if( ($ilen * 6) % 8 != 0 ) {
	warn "input is not byte aligned, will need padding";
    }
    return decode_b64($input);
}

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


