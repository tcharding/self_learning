#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

use Data::Dumper;
use MIME::Base64 qw(encode_base64 decode_base64);
use Crypto::Block qw/:all/;

my $file = "7.out.txt";
open my $fh, '<', $file or die "Cannot open $file";
my $message = do { local( $/ ) ; <$fh> } ;
my $key = &random_16_chars;

# from question
my $append = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg" .
    "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq" .
    "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK";

# get block size
my $block_size = &guess_block_size;
print "Cipher block size: $block_size\n";

# decect ECB mode
my $input = "this is 16 bytes";
$input = $input . $input;
my $c = encryption_oracle( $input );
if (has_repeats( unpack('H*', $c))) {
    print "Cipher appears to be running in ECB mode\n";
}

my $first_bit = &cryptanalize;
print "$first_bit\n";

sub cryptanalize {
    my $fn = \&encryption_oracle;
    my $block_size = 16;	# from above
    my $p = "";
    my $bufsz = length( &$fn("") );
    
    for (1 .. $bufsz) {
	my $di = "A" x ($bufsz - $_);
	$di .= $p;
	my $dict = &build_dictionary( $di );

	my $ci = "A" x ($bufsz - $_);
	my $c = encryption_oracle( $ci );

	my $hex = unpack('H*', $c);
	$hex = substr( $hex, 0, ($bufsz * 2)); # first half
	for (keys %$dict) {
	    if ($$dict{$_} eq $hex) {
		$p .= $_;
	    }
	}
    }
    return $p;
}


#    my $nrepeats = ($blockn * $block_size) + ($block_size - 1);
sub build_dictionary {
    my $s = shift;
    my %d;
    my $match = (length( $s ) + 1) * 2;

    my @chars = (32 .. 126);
    push @chars, 10;		# we need a newline also

    for (@chars) {
	my $char = chr($_);
	my $input = $s . $char;
	my $c = encryption_oracle( $input );
	my $hex = unpack('H*', $c);
	$d{$char} = substr($hex, 0, $match);
    }
    return \%d;
}

sub guess_block_size {
    my $len;
    my $c = encryption_oracle("");
    $len = length( $c );
    my $next = 0;
    my($s, $cnt) = ("", 0);
    while ($next <= $len) {
	$cnt++;
	$s = "A" x $cnt;
	$next = length( encryption_oracle($s));
    }
    return $next - $len;
}

sub encryption_oracle {
    my $message = shift;
    $message .= decode_base64($append);

    my $input = &pad( $message, 16 );
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    
    return $cipher->encrypt( $input );
}
sub cryptanalize_block {
    my $fn = \&encryption_oracle;
    my $block_size = 16;	# from above
    my $p = "";
    
    for (1 .. $block_size) {
	my $di = "A" x ($block_size - $_);
	$di .= $p;
	my $dict = &build_dictionary( $di );

	my $ci = "A" x ($block_size - $_);

	my $c = encryption_oracle( $ci );
	my $hex = unpack('H32', $c);
	for (keys %$dict) {
	    if ($$dict{$_} eq $hex) {
		$p .= $_;
	    }
	}
    }
    return $p;
}
