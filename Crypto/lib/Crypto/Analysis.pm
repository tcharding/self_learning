package Crypto::Analysis;

use 5.022000;
use strict;
use warnings;

use lib qw( /home/tobin/build/github/self_learning/Crypto/lib/ );
use Crypto::Util qw( :all );

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = (
    'all' => [ qw(
		    bruteforce_scx
		    rate_msgs
		    num_top_rated
		    get_top_rated
	     )] );


our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

sub bruteforce_scx {
    my( $c, $scx ) = @_;
    my %hash;
    $scx = \%hash unless (defined $scx );
    
    for ( 32..126 ) {
				# convert integer to bit string length 8
	my $bk = dec2bin( $_ );
	while ( length($bk) < 8) {
	    $bk = "0" . $bk;
	}
				# decrypt and store result
	my $k = encode_ascii( $bk );
	my $m = &repeating_xor( $c, $bk );
	$$scx{ $m }{ key } = $k;
    }
    return $scx;
}

sub num_top_rated {
    my $scx = shift;
    my( $max, $cnt );
				# get max rating
    $max = -1;
    for my $m ( keys %$scx ) {
	my $rating = $$scx{ $m }{ rating };
	$max = $rating if ( $rating > $max );
    }
				# count msgs with max rating
    for my $m ( keys %$scx ) {
	my $rating = $$scx{ $m }{ rating };
	$cnt++ if ( $rating == $max );
    }
    return $cnt;
}

sub get_top_rated {
    my $scx = shift;
    my( $max, @top_rated );
				# get max rating
    my $first_time = 0;
    for my $m ( keys %$scx ) {
	my $rating = $$scx{ $m }{ rating };
	if ($first_time == 0) {
	    $max = $rating;
	    $first_time = 1;
	} else {
	    $max = $rating if ( $rating > $max );	    
	}
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

my @alpha = ("a".."z","A".."Z");
my @alphanum = ("a".."z","A".."Z", 0..9);
my @common = ("a".."z","A".."Z", 0..9, '\'', '\"', ' ', '.', ',', '?', '!', '(', ')', '\n');
my @most = qw/ e t a o i /;
my @least = qw/ z q x j k /;

sub rate_simple {
    my $m = shift;
    my $score;

    $score += rate_printable( $m );
    $score += rate_punctuation( $m );
    $score += rate_most_frequent( $m );
    $score += rate_least_frequent( $m );
    $score += rate_common( $m );

    return $score;
}

# return -100 - 100
sub rate_printable {
    my $m = shift;
    my( $printable, $total ) = &cnt_rate( $m, \&is_printable);

    return 0 if ($total == 0);	# no data, zero rating
    return 100 if ($printable == $total);
    
    # rate on absolute value of cnt
    my $unprintable = $total - $printable;

    return 50 if ($unprintable == 1);
    return 0 if ($unprintable < 3);
    return -50 if ($unprintable < 5);
    return -100;
}

# return -100 - 100
sub rate_common {
    my $m = shift;
    my( $cnt, $total ) = &cnt_rate( $m, \&is_member, \@common);
    return 0 unless $total;	# no data, zero rating

    my $p = $cnt / $total;
    return 100 if ( $p > 0.95 );
    return 75 if  ( $p > 0.90 );
    return 50 if  ( $p > 0.85 );
    return 25 if  ( $p > 0.80 );
    return 0 if  ( $p > 0.75 );
    return -25 if  ( $p > 0.60 );
    return -50 if  ( $p > 0.45 );
    return -75 if  ( $p > 0.30 );
    return -100;
}

# return -50 - 50
sub rate_punctuation {
    my $m = shift;
    my( $cnt, $total ) = &cnt_rate( $m, \&is_punc);

    return 0 unless $total;	# no data, zero rating
				# short circuit if too many unprintable
    my $printable;
    ( $printable, $total ) = &cnt_rate( $m, \&is_printable );
    my $unprintable = $total - $printable;
    if ($unprintable > 3) {
	return 0;
    }
    				# rate on percentage value of cnt
    my $p = $cnt / $total;
    return 50 if ($p < 0.10);
    return 25 if ($p < 0.20);
    return 0 if ($p < 0.30);
    return -25 if ($p < 0.50);
    return -50;
}

# return -50 - 50
sub rate_most_frequent {
    my $m = shift;
    my( $cnt, $total ) = &cnt_rate( $m, \&is_member, \@most);
    return 0 unless $total;	# no data, zero rating

    my $p = $cnt / $total;
    return 50 if is_between( $p, 0.35, 0.40 );
    return 25 if is_between( $p, 0.30, 0.45 );
    return 0 if is_between( $p, 0.25, 0.50 );
    return -25 if is_between( $p, 0.15, 0.60 );
    return -50;
}

# return -50 - 50
sub rate_least_frequent {
    my $m = shift;
    my $printable;
    my( $cnt, $total ) = &cnt_rate( $m, \&is_member, \@least );
    ( $printable, $total ) = &cnt_rate( $m, \&is_printable );
#    print "$cnt, $total\n";
				# short circuit if too many unprintable
    my $unprintable = $total - $printable;
    if ($unprintable > 3) {
	return 0;
    }
    return 0 unless $total;	# no data, zero rating

    my $p = $cnt / $total;
    return 50 if ( $p < 0.02 );
    return 25 if ( $p < 0.05 );
    return 0 if ( $p < 0.10 );
    return -25 if ( $p < 0.20 );
    return -50;
}

# return number of characters that pass rate_fn ($set is passed to rate_fn)
sub cnt_rate {
    my( $m, $rate_fn, $set ) = @_;
    my( $cnt, $total ) = ( 0, 0 );

    while( length( $m ) > 0 ) {
	$total++;
	my $byte = substr( $m, 0, 8);
	$m = substr( $m , 8);
	$cnt += &$rate_fn( $byte, $set );
    }
    return( $cnt, $total );
}

# return 1 if byte is an ASCII character else 0
sub is_printable {
    my $byte = shift;		# 8 digit bit string

    my $hex = encode_hex( $byte );

    if ( (hex( $hex ) >= 32) && (hex( $hex ) <= 126 ) ) {
	return 1;		# true
    }
    return 0;			# false
}

# does not include space (ASCII 32)
sub is_punc {
    my $byte = shift;
    my $v = encode_hex( $byte );
    $v = hex( $v );

    if ( ($v >= 33 && $v <= 47) ||
	     ($v >= 58 && $v <= 64) ||
	     ($v >= 91 && $v <= 96) ||
	     ($v >= 123 && $v <= 126) ) {
	return 1;
    }
    return 0;
}

sub is_member {
    my( $byte, $set ) = @_;
    my $char = encode_ascii( $byte );
    for (@$set) {
	if ($_ eq $char) {
	    return 1;
	}
    }
    return 0;
}

# return 1 if v is between min and max exclusive
sub is_between {
    my( $v, $min, $max ) = @_;
    if ($v > $min && $v < $max) {
	return 1;
    }
    return 0;
}
