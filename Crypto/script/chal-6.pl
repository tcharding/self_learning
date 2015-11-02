#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use lib qw(/home/tobin/build/github/self_learning/Crypto/lib);
use Crypto::Util qw( decode_b64 encode_ascii );
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

my $n_to_print = 5;		# configure output 

say "Guessing key length using edit distance method";
&printn_sorted_by_value( $n_to_prit, keylen_ed( $c, 40 ))

#say "Guessing key length using Friedman method";
#&printn_sorted_by_value( $n_to_prit, keylen_ic( $c, 40 ))


sub printn_sorted_by_value {
    my( $n, $hash ) = @_;
    my $cnt;
    for (sort { $$hash{$a} cmp $$hash{$b} } keys %$hash) {
	printf "%-2s: %s\n", $_, $$hash{$_};
	$cnt++;
	if ($cnt >= $n_to_print) {
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

sub transpose {
    my( $data, $n ) = @_;
    my @blocks;
    
    for (my $i = 0; $i < length( $data ); $i += 8) {
	my $block = ($i / 8) % $n;
	$blocks[$block] .= substr( $data, $i, 8 );
    }
    return \@blocks;
}

