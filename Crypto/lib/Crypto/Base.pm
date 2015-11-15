package Crypto::Base;

use 5.022000;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				      hex_to_ascii ascii_to_hex
				      pseudo_random_int
				      pseudo_random_string
				      dump_hex
				   )],
		 );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

sub dump_hex {
    my $hex = shift;
    while (length($hex) > 0) {
	my $h = substr($hex, 0, 2);
	$hex = substr($hex, 2);
	print "0x$h ";
    }
    print "\n";
}

# return a pseudo random integer min <= n < max
sub pseudo_random_int {
    my( $min, $max ) = @_;
    return (int(rand($max - $min)) + $min);
}

sub pseudo_random_string {
    my( $len ) = @_;
    my $s;
    $len = 16 unless defined $len; # default to 16
    my( $min, $max ) = ( 33, 126 ); # printable characters excluding 'space'

    for (1 .. $len) {
	$s .= chr(&pseudo_random_int($min, $max));
    }
    return $s;  
}

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
}

1;
__END__

=head1 NAME

Package::Name -

=head1 SYNOPSIS

  use Package::Name;

=head1 DESCRIPTION

Description goes here.

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
