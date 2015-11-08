package Crypto::Block;

use 5.022000;
use strict;
use warnings;
use Carp;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = (
    'all' => [ qw(
		     pad_pkcs_7 split_into_blocks
	     )],
);

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

# split 'string' into 'size' blocks, pad if necessary with &pad_pkcs_7
sub split_into_blocks {
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

1;
