#!/usr/bin/perl
use strict;
use warnings;
use feature qw/say/;

#
# An ECB/CBC detection oracle
#

use Crypt::Rijndael;
use Crypto::Block qw/:all/;
use Crypto::Base qw/:all/;
use Crypto::Cipher qw/:all/;

use constant CBC => 1;
use constant ECB => 2;

print STDOUT "error message\n";

my $debug = "on";		# comment out to turn off

my $file = "script/7.out.txt";
open my $fh, '<', $file or die "Cannot open $file";
my $message = do { local( $/ ) ; <$fh> } ;

# verbose testing
print "(actual => guess)\n";
for (1 .. 10) {
    print "(";
    my $c = encryption_oracle( $message );
    print " => ";
    printf "%s", ecb_or_cbc( $c );
    print ")\n";
}

# guess mode CBC or ECB
sub ecb_or_cbc {
    my $hex = unpack('H*', shift);
    if (&has_repeating_blocks( $hex ) == -1) {
	return "CBC";		# cbc mode
    }
    return "ECB";		# ecb mode
}

sub encryption_oracle {
    my $input = shift;
    my $iv = "THIS IS 16 BYTES";
    my $key = &pseudo_random_string( 16 );
    my $mode = pseudo_random_int( 1, 3 );

    my $append = pseudo_random_string( pseudo_random_int( 5, 11 ) ); # 5 - 10 bytes
    my $p = $append . $input . $append;

    my $c;
    if ($mode == ECB) {		# ecb mode
	my $input = &pad( $p );
	my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
	$c = $cipher->encrypt( $input );
    } elsif ($mode == CBC) {			# cbc mode
	$p = pad( $p, 16 );
	$c = &encrypt_aes_cbc( $p, $key, $iv );
    } else {
	die "Unknown mode: $mode\n";	# programmer error
    }
    if ($debug) {
	printf "%s", mode_as_string( $mode );	
    }
    return $c;
}

# convert mode into string
sub mode_as_string {
    my $mode = shift;
    
    if ($mode == ECB) {
	return "ECB";
    } elsif ($mode = CBC) {
	return "CBC";
    } else {
	return "Unknown mode: $mode";
    }
}
