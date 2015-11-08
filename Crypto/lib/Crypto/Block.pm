package Crypto::Block;

use 5.022000;
use strict;
use warnings;
use Carp;
use Crypt::Rijndael;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = (
    'all' => [ qw(
		     pad_pkcs_7 pad strip_padding
		     split_string_into_blocks
		     split_bin_into_blocks
		     encrypt_aes_cbc
		     decrypt_aes_cbc 
		     has_repeats
		     random_16_chars
	     )],
);

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

# split 'string' into 'size' blocks, pad if necessary with &pad_pkcs_7
sub split_string_into_blocks {
    my( $string, $size ) = @_;
    my( @blocks, $block );
    
    while( length( $string ) >= $size ) {
	$block = substr( $string, 0, $size );
	$string = substr( $string, $size );
	push @blocks, $block;
    }
    				# handle last bit of string if present
    if( length( $string ) > 0 ) {
	die "error $!" if( length( $string ) >= $size ); # defensive programming
	$block = pad_pkcs_7( $string, $size );
	push @blocks, $block;
    }
    return \@blocks;
}

# split binary data into nbyte blocks
sub split_bin_into_blocks {
    my( $bin, $nbyte ) = @_;
    my( @bin, @blocks, $block );

    my $string = unpack( 'H*', $bin );
    my $size = $nbyte * 2;
    
    while( length( $string ) >= $size ) {
	$block = substr( $string, 0, $size );
	$string = substr( $string, $size );
	push @blocks, $block;
    }
    for (@blocks) {
	push @bin, pack('H*', $_);
    }
    				# no padding
    return \@bin;
}

sub pad {
    my( $s, $size ) = @_;
    my $len = length( $s );

    if (( $len % $size ) == 0) {
	return $s;
    }
    my $i = $len - (($len + $size) % $size);
    my $padded = substr( $s, 0, $i );
    $padded .= pad_pkcs_7( substr( $s, $i ), $size );
    return $padded;
}
# add padding bytes 0x04 to string input
sub pad_pkcs_7 {
    my( $s, $size ) = @_;

    my $nbytes = length( $s );
    croak "requested block size is smaller than block"
	if( $nbytes > $size );
    
    my $padlen = $size - $nbytes;
    $s .= chr(0x04) x $padlen;
    return $s;
}
sub strip_padding {
    my $s = shift;
    my $i = index $s, chr(0x04);
    
    if ($i != -1) {		# found padding
	$s = substr( $s, 0, $i );
    }
    return $s;
}
sub encrypt_aes_cbc {
    my( $plaintext, $key, $iv ) = @_;
				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    				# hexify inputs
    my $blocks = split_string_into_blocks( $plaintext, 16 );	# 16 byte blocks
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );
    				# now chain block cipher 
    my( $bin, $ciphertext );
    my $feedback = pack( 'A*', $iv );
    for (@$blocks) {
	my $input = $feedback ^ pack('A*', $_);
	my $bin = $cipher->encrypt( $input ); # encrypt as hex string (As 128)
	$ciphertext .= $bin;
	$feedback = $bin;
    }

    return $ciphertext;
}
sub decrypt_aes_cbc {

    my( $ciphertext, $key, $iv ) = @_;
				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    				# hexify inputs
    my $blocks = split_bin_into_blocks( $ciphertext, 16 );
    my $cipher = Crypt::Rijndael->new( $key, Crypt::Rijndael::MODE_ECB() );

    		# now chain block cipher mode
    my $feedback = pack( 'A*', $iv );
    my $plaintext;
    for (@$blocks) {
	$plaintext .=  $cipher->decrypt( $_ ) ^ $feedback;
	$feedback = $_;
    }
    return $plaintext;
}
sub has_repeats {
    my $hex = shift;
    my( $i, $j );
    my $chunk = 32;		# 32 hex digits = 16 bytes
    for( $i = 0; $i < length( $hex ); $i += $chunk ) {
	my $byte = substr( $hex, $i, $chunk );
	for( $j = $i + $chunk; $j < length( $hex ); $j += $chunk ) {
	    my $cmp = substr( $hex, $j, $chunk );
	    if( $byte eq $cmp ) {
		return 1;	# true
	    }
	}
    }
    return 0;			# false
}

sub random_16_chars {
    my $key;

    for (1 .. 16) {
	# 32 - 126
	my $n = 0;
	while( $n < 32) {
	    $n = int(rand(126));
	}
	$key .= chr($n);
    }
    return $key;
}

1;
