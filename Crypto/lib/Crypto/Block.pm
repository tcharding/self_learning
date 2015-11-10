package Crypto::Block;

use 5.022000;
use strict;
use warnings;
use Carp;
require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				      has_repeating_blocks
				      pad pad_pkcs_7 strip_padding
				      encrypt_aes_cbc encrypt_aes_ecb
				      split_string_into_blocks
				      split_bin_into_blocks

			      ) ],
		     'pad' => [ qw(
				      pad pad_pkcs_7 strip_padding
			      )],
		 );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';
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

# pad 'string' to multiple of 'size' using &pad_pkcs_7
sub pad {
    my( $string, $size ) = @_;
    my $len = length( $string );
    $size = 16 unless defined $size; # default 16 bytes
    
    if (( $len % $size ) == 0) {
	return $string;
    }
    my $i = $len - (($len + $size) % $size);
    my $padded = substr( $string, 0, $i );
    $padded .= pad_pkcs_7( substr( $string, $i ), $size );
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
	# 			# check padding
	# my $padding = substr( $s, $i );
	# for (split //, $padding) {
	#     die "Padding error" if ($_ != chr(0x04));
	# }
	$s = substr( $s, 0, $i );
    }
    return $s;
}

# Checks input hex string for repeating blocks (16 byte length)
# Returns start of first repeating block if found or -1 if not.
#
# Big-O: nlog(n) ?
sub has_repeating_blocks {
    my( $hex, $block_size ) = @_;
    $block_size = 16 unless defined $block_size;

    my $len = length( $hex );
    my $chunk = 2 * $block_size;

    for(my $i = 0; $i < $len; $i += $chunk ) {
	my $byte = substr( $hex, $i, $chunk );
	for(my $j = $i + $chunk; $j < $len; $j += $chunk ) {
	    my $cmp = substr( $hex, $j, $chunk );
	    if( $byte eq $cmp ) {
		return $i;	# true
	    }
	}
    }
    return -1;			# false
}



1;
__END__

=head1 NAME

Crypto:: -

=head1 SYNOPSIS

  use Crypto::;

=head1 DESCRIPTION

=head2 EXPORT

=head1 SEE ALSO

=head1 AUTHOR

Tobin Harding, E<lt>me@tobin.ccE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2015 by Tobin Harding

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.22.0 or,
at your option, any later version of Perl 5 you may have available.


=cut
