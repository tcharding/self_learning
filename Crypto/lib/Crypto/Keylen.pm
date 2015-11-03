package Crypto::Keylen;

use 5.022000;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = (
    'all' => [ qw(
		     keylen_ic
		     transpose
	     )],
);


our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

# guess key length using Friedman method
sub keylen_ic {
    my( $hex, $max ) = @_;
    my %ic_per_knchars;
    $max = 40 unless defined $max;
    my $C = 26;			# Normalizing co-efficient 

    for my $nchars (2..$max) {
	my $trans_blocks = &transpose( $hex, $nchars );
	my( $sum, $total );
	for my $t ( @$trans_blocks ) {
	    my( $n, $N ); # https://en.wikipedia.org/wiki/Index_of_coincidence
	    $N = length( $t ) / 2; # 2 hex digits per char
	    for ( 0 .. $N-1 ) {
		$n = 0;
		my $i = $_ * 2;
		my $byte = substr( $t, $i, 2 );
		for ( 0 .. $N-1 ) {
		    my $i = $_ * 2;
		    my $cmp = substr( $t, $i, 2 );

		    if ( $cmp eq $byte ) {
			$n++;
		    }
		}
	    }
	    my $step = ($n * ($n - 1)) / ($N * ($N - 1)); # n(n-1) / N(N-1)
	    $sum += $step;
	    my $ic = $sum * $C;
#	    print "$nchars: $ic\n";
	    $total += $ic;
	}
	$ic_per_knchars{$nchars} = $total / $nchars;
    }
    return \%ic_per_knchars;
}


# guess key length using edit distance
sub keylen_ed {
    my( $c, $max ) = @_;
    my %ed_per_knchars;
    $max = 40 unless defined $max;
    for my $nchars (2..$max) {
	my $klen = $nchars * 8;
	my( $a, $b );
	$a = substr( $c, 0, $klen );
	$b = substr( $c, $klen, $klen );
	my $ed = &edit_distance( $a, $b );
	my $normalized = $ed / $nchars;
	$ed_per_knchars{$nchars} = $normalized;
    }
    return \%ed_per_knchars;
}
# compute edit distance of between same length bit strings
sub edit_distance {
    my( $a, $b ) = @_;
    my $len = length( $a );

    if ($len != length( $b )) {
	warn "edit_distance: strings are not same length\n";
    }

    my $ed;
    for (my $i = 0; $i < $len; $i++) {
	if (substr( $a, $i, 1 ) != substr( $b, $i, 1 )) {
	    $ed++;
	}
    }
    return $ed;
}

# return array size n containing input bit string transposed by byte
sub transpose {
    my( $hex, $n ) = @_;
    my @blocks;
    
    for (my $i = 0; $i < length( $hex ); $i += 2) {
	my $block = ($i / 2) % $n;
	$blocks[$block] .= substr( $hex, $i, 2 );
    }
    return \@blocks;
}

1;

__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

Crypto::Util - Perl extension for blah blah blah

=head1 SYNOPSIS

  use Crypto::Util;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for Crypto::Util, created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head2 EXPORT

None by default.

=head1 SEE ALSO

Mention other useful documentation such as the documentation of
related modules or operating system documentation (such as man pages
in UNIX), or any relevant external documentation such as RFCs or
standards.

If you have a mailing list set up for your module, mention it here.

If you have a web site set up for your module, mention it here.

=head1 AUTHOR

Tobin Harding, E<lt>me@tobin.ccE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2015 by Tobin Harding

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.22.0 or,
at your option, any later version of Perl 5 you may have available.


=cut

