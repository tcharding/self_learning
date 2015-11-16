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
				      split_string_into_blocks
				      split_bin_into_blocks
				      pad 
				      strip_padding 
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

# split 'string' into 'size' blocks
sub split_string_into_blocks {
    my( $string, $size ) = @_;
    my( @blocks, $block );
    
    while( length( $string ) >= $size ) {
	$block = substr( $string, 0, $size );
	$string = substr( $string, $size );
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
    if ($pad < 0 || $pad > $block_size) { # padding byte correct
	return 0;
    }
				# all padding bytes correct
    my $count;
    my $padding = substr($s, 0 - $pad);
    for (split //, $padding) {
	++$count;
	return 0 if ($_ ne chr($pad));
    }
    if ($count != $pad) {
	return 0;	      # padding byte equals number of padded bytes
    }
				# final byte of 0x01 gives false positive
    if ($pad == 1) {
	my $second_last = ord(substr($s, -2, 1));
	if ($second_last > 0 && $second_last <= $block_size) {
	    return 0;
	}
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
