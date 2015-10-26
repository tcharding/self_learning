package Crypto::Filters;
use strict;
use Exporter;
use Crypto::Utils;

our @ISA = ('Exporter');
our @EXPORT = qw(
		    filter_unprintable
		    filter_morethan
	    );

# return true (input string) if more than n characters from set are present
#
# case insensitive 
# 0 < num < 1 : percentage
# n > 0 : absolute
sub filter_morethan {
    my( $bits, $min, @set ) = @_;

    my( $n, $percent );
    my %symbols;

    my $s = encode_ascii( $bits );
    my $nchars = length( $s );
    $s = lc $s;
    				# count symbols
    $symbols{ $_ }++ for ( split //, $s );
    $n = 0;
    for ( @set ) {
	if ( exists $symbols{ $_ } ) {
	    $n += $symbols{ $_ };
	}
    }
    $n /= $nchars if ($nchars); # percentage
    
    if ( $n < $min ) {
	return undef;
    }
    return $bits;
}

# return input string if more than max are printable, undef if not
sub filter_unprintable {
    my( $bits, $max ) = @_;
    my $retval = $bits;
    my $n = 0;			# num unprintable
    my( $byte, $len );
    
    if ( (length( $bits ) % 8) != 0 ) {
	die "filter_unprintable: Bit string not byte aligned\n";
    }

    while ( length( $bits ) > 0 ) {
	$byte = substr( $bits, 0, 8);
	$bits = substr( $bits, 8);
	$n++ unless _is_printable( $byte );
	$len++;
    }
    if ( $max < 1 ) {		# percentage
	$n /= $len;
    }
    if ( $n > $max ) {
	return undef;
    }
    return $retval;
}

# return 1 if byte is an ASCII character else 0
sub _is_printable {
    my $byte = shift;		# 8 digit bit string

    my $hex = encode_hex( $byte );

    if ( (hex( $hex ) >= 32) && (hex( $hex ) <= 126 ) ) {
	return 1;		# true
    }
    return 0;			# false
}

1;
