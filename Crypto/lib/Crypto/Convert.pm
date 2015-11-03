package Crypto::Convert;

use 5.022000;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = (
    'all' => [ qw(
		     hex_to_ascii ascii_to_hex
	     )],
);

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

# hex -> ascii
sub hex_to_ascii {
    my $hex = shift;
    $hex =~ s/(([0-9a-f][0-9a-f])+)/pack('H*', $1)/ie;
    return $hex;
}

# ascii -> hex
sub ascii_to_hex {
    my $hex;
    for (split //, shift) {
	my $digit = sprintf("%x", ord($_));
	if (length( $digit ) == 1) {
	    $digit = "0" . $digit;
	}
	$hex .= $digit;
    }
    return $hex;
#    $s =~ s/((.)+)/unpack('H*', $1)/ie;
#    return $s;
}

1;
