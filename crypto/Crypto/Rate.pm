package Crypto::Rate;
use strict;

use Exporter;
use Crypto::Util;

our @ISA = ('Exporter');
our @EXPORT = qw(
		    rate_simple
		    rate_printable
		    rate_punctuation
		    rate_most_frequent
		    rate_least_frequent
		    rate_common
	    );

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

sub rate_printable {
    my $m = shift;
    my( $printable, $total ) = &cnt_rate( $m, \&is_printable);

    return 0 if ($total == 0);	# no data, zero rating
    return 100 if ($printable == $total);
    
    my $score = 0;
    # rate on absolute value of cnt
    my $unprintable = $total - $printable;
    if ($unprintable < 2) {
	$score += 100
    } elsif ($unprintable < 5) {
	$score += 60;
    } elsif ($unprintable < 10) {
	$score += 10;
    }
    # 				# rate on percentage value of cnt
    # my $p = $unprintable / $total;
    # if ($p < 0.05) {
    # 	$score += 50;
    # } elsif ($p < 0.10) {
    # 	$score += 30;
    # } elsif ($p < 0.20) {
    # 	$score += 10;
    # }
    return $score;
}

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
    return 100 if ($p < 0.10);
    return 50 if ($p < 0.30);
    return 20 if ($p < 0.50);
}

sub rate_most_frequent {
    my $m = shift;
    my( $cnt, $total ) = &cnt_rate( $m, \&is_member, \@most);
    return 0 unless $total;	# no data, zero rating

    my $p = $cnt / $total;
    return 100 if is_between( $p, 0.35, 0.40 );
    return 75 if is_between( $p, 0.30, 0.45 );
    return 50 if is_between( $p, 0.25, 0.50 );
    return 25 if is_between( $p, 0.20, 0.55 );
    return 0;
}
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
    return 100 if ( $p < 0.02 );
    return 60 if ( $p < 0.05 );
    return 20 if ( $p < 0.10 );
    return 0;
}

sub rate_common {
    my $m = shift;
    my( $cnt, $total ) = &cnt_rate( $m, \&is_member, \@common);
    return 0 unless $total;	# no data, zero rating

    my $p = $cnt / $total;
    return 100 if ( $p > 0.95 );
    return 75 if  ( $p > 0.90 );
    return 50 if  ( $p > 0.85 );
    return 25 if  ( $p > 0.80 );
    return 0;
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
