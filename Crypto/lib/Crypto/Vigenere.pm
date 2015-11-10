package Crypto::Vigenere;

use 5.022000;
use strict;
use warnings;
use Crypto::Base qw( :all );

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				      bruteforce_scx
				      rate_msgs
				      get_top_rated
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

sub bruteforce_scx {
    my $hex = shift;
    my %scx;

    for ( 32 .. 126 ) {
	my $char = chr($_);
	my $xor;

	for (my $i = 0; $i < length( $hex ); $i+=2) {
	    my $byte = substr($hex, $i, 2);
	    $xor .= unpack('H*',pack('H*', $byte) ^ pack('A*', $char));
	}
	$scx{ $char }{ hex } = $xor;
    }
    return \%scx;
}

# return $hex string of top rated message
sub get_top_rated {
    my $scx = shift;
    my( $max, @top_rated, $hex );
				# get max rating
    my $first_time = 0;
    for my $char ( keys %$scx ) {
	my $rating = $$scx{ $char }{ rating };
	if ($first_time == 0) {
	    $max = $rating;
	    $first_time = 1;
	} else {
	    $max = $rating if ( $rating > $max );	    
	}
    }

    for my $char ( keys %$scx ) {
	my $rating = $$scx{ $char }{ rating };
	if ( $rating == $max ) {
	    push @top_rated, $$scx{ $char }{ hex };
	}
    }
    my $n = @top_rated;
    if ($n == 1) {
	return pop @top_rated;
    } 
    return undef;
}

# accepts optional rating function otherwise uses &rate_simple
sub rate_msgs {
    my( $scx ) = @_;

    for my $char (keys %$scx ) {
	my $score;
	my $hex = $$scx{ $char }{ hex };
	$score += rate_printable( $hex );
	$score += rate_punctuation( $hex );
	$score += rate_most_frequent( $hex );
	$score += rate_least_frequent( $hex );
	$score += rate_common( $hex );
	$$scx{ $char }{ rating } = $score;
    }
}

my @alpha = ("a".."z","A".."Z");
my @alphanum = ("a".."z","A".."Z", 0..9);
my @common = ("a".."z","A".."Z", 0..9, '\'', '\"', ' ', '.', ',', '?', '!', '(', ')', '\n');
my @most = qw/ e t a o i /;
my @least = qw/ z q x j k /;

# return -100 - 100
sub rate_printable {
    my $hex = shift;
    my( $printable, $total ) = &cnt_rate( $hex, \&is_printable);

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
    my $hex = shift;
    my( $cnt, $total ) = &cnt_rate( $hex, \&is_member, \@common);
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
    my $hex = shift;
    my( $cnt, $total ) = &cnt_rate( $hex, \&is_punc);

    return 0 unless $total;	# no data, zero rating
				# short circuit if too many unprintable
    my $printable;
    ( $printable, $total ) = &cnt_rate( $hex, \&is_printable );
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
    my $hex = shift;
    my( $cnt, $total ) = &cnt_rate( $hex, \&is_member, \@most);
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
    my $hex = shift;
    my $printable;
    my( $cnt, $total ) = &cnt_rate( $hex, \&is_member, \@least );
    ( $printable, $total ) = &cnt_rate( $hex, \&is_printable );
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
    my( $hex, $rate_fn, $set ) = @_;
    my( $cnt, $total ) = ( 0, 0 );

    while( length( $hex ) > 0 ) {
	$total++;
	my $byte = substr( $hex, 0, 2);
	$hex = substr( $hex , 2);
	$cnt += &$rate_fn( $byte, $set );
    }

    return( $cnt, $total );
}

# return 1 if byte is an ASCII character else 0
sub is_printable {
    my $hex = shift;		# 2 digit hex string

    if ( (hex( $hex ) >= 32) && (hex( $hex ) <= 126 ) ) {
	return 1;		# true
    }
    return 0;			# false
}

# does not include space (ASCII 32)
sub is_punc {
    my $hex = shift;
    my $v = hex( $hex );

    if ( ($v >= 33 && $v <= 47) ||
	     ($v >= 58 && $v <= 64) ||
	     ($v >= 91 && $v <= 96) ||
	     ($v >= 123 && $v <= 126) ) {
	return 1;
    }
    return 0;
}

sub is_member {
    my( $hex, $set ) = @_;
    my $char = hex_to_ascii( $hex );
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
