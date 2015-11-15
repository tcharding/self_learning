package Crypto::Block;

use 5.022000;
use strict;
use warnings;

use Carp;
require Exporter;

use Crypt::Rijndael;
use Crypto::Base qw/ pseudo_random_string /;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				      has_repeating_blocks
				      encrypt_aes_cbc decrypt_aes_cbc
				      split_string_into_blocks
				      split_bin_into_blocks
				      pad strip_padding 
				      is_pad_correct
			      ) ],
		     'pad' => [ qw(
				      pad pad_pkcs_7
				      strip_padding strip_pkcs_7
				      is_pad_correct
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
	$block = pad( $string, $size );
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

    $iv = &pseudo_random_string( 16 ) unless $iv;
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
    $iv = &pseudo_random_string( 16 ) unless $iv;
				# sanity checks
    if( length($iv) != 16) {	# 16 bytes
	die "IV must be 16 bytes long";
    }
    if( length($key) != 16) {	# 16 bytes
	die "key must be 16 bytes long";
    }
    				# hexify input
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

# add padding as specified by pkcs#7
sub pad {
    my( $string, $size ) = @_;
    my $len = length( $string );
    $size = 16 unless defined $size; # default 16 bytes
				
    my $pad;			# padding length and byte value
    if ($len > $size) {		     # more than one block
	$pad = $size - ($len % $size);	
    } else {
	$pad = $size - $len;
    }
    $pad = $size if $pad == 0;	# must have some padding

    $string .= chr($pad) x $pad;
    return $string;

}
# # add padding bytes to single block
# sub pad_pkcs_7 {
#     my( $s, $size ) = @_;

#     my $len = length( $s );
#     my $pad = $size - $len;
#     $pad = $size if $pad == 0;

#     croak "requested block size is smaller than block"
# 	if( $len < 0 );
    
#     $s .= chr($pad) x $pad;
#     return $s;
# }
# # strip padding bytes from single block
# sub strip_pkcs_7 {
#     my( $block, $len ) = @_;
#     $len = 16 unless defined $len;
#     my $pad = ord(substr($block, -1));
#     if ($pad < 0 || $pad > $len) {
# 	croak "Block is incorrectly padded";
#     }
#     return substr( $block, 0, $len - $pad );
# }

# strip padding
sub strip_padding {
    my( $s, $block_size ) = @_;
    $block_size = 16 unless defined $block_size;
    
    if (is_pad_correct( $s, $block_size ) == 0) {
	croak "Data not padded correctly";
    }
    my $pad = ord(substr($s, -1));
    if ($pad < 0 || $pad > $block_size) {
	die "programmer error";
    }
    
    return substr( $s, 0, length($s) - $pad );
}

# verify padding
sub is_pad_correct {
    my( $s, $block_size ) = @_;
    $block_size = 16 unless defined $block_size;
    
    my $pad = ord(substr($s, -1));
    if ($pad < 0 || $pad > $block_size) {
	return 0;
    }
    my $padding = substr($s, 0 - $pad);
    for (split //, $padding) {
	return 0 if ($_ ne chr($pad));
    }
    return 1;			# yes
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
