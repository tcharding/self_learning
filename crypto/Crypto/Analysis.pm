package Crypto::Analysis;
use strict;
use Exporter;

use Crypto::Util;
use Crypto::Rate;

our @ISA = ('Exporter');
our @EXPORT = qw(
		    bruteforce_scx
		    rate_msgs
		    get_top_rated
	    );

sub bruteforce_scx {
    my $c = shift;
    my %scx;
    
    for ( 32..126 ) {
				# convert integer to bit string length 8
	my $bk = dec2bin( $_ );
	while ( length($bk) < 8) {
	    $bk = "0" . $bk;
	}
				# decrypt and store result
	my $k = encode_ascii( $bk );
	my $m = &repeating_xor( $c, $bk );
	$scx{ $m }{ key } = $k;
    }
    return \%scx;
}

# sub num_top_rated {
#     my $scx = shift;
#     my( $max, $cnt );
# 				# get max rating
#     $max = -1;
#     for my $m ( keys %$scx ) {
# 	my $rating = $$scx{ $m }[1];
# 	$max = $rating if ( $rating > $max );
#     }
# 				# count msgs with max rating
#     for my $m ( keys %$scx ) {
# 	my $rating = $$scx{ $m }[1];
# 	$cnt++ if ( $rating == $max );
#     }
#     return $cnt;
# }

sub get_top_rated {
    my $scx = shift;
    my( $max, @top_rated );
				# get max rating
    $max = -1;
    for my $m ( keys %$scx ) {
	my $rating = $$scx{ $m }{ rating };
	$max = $rating if ( $rating > $max );
    }
				# count msgs with max rating
    for my $m ( keys %$scx ) {
	my $rating = $$scx{ $m }{ rating };
	if ( $rating == $max ) {
	    push @top_rated, $m;
	}
    }
    return \@top_rated;
}

# accepts optional rating function otherwise uses &rate_simple
sub rate_msgs {
    my( $scx, $fn ) = @_;

    for my $m (keys %$scx ) {
	my $rating;
	if ( defined $fn ) {
	    $rating = &$fn( $m )
	} else {
	    $rating = &rate_simple( $m );
	}
	$$scx{ $m }{ rating } = $rating;
    }
}
