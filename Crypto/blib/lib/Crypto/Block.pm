package Crypto::Block;

use 5.022000;
use strict;
use warnings;

require Exporter;

our @ISA = qw(Exporter);

our %EXPORT_TAGS = ( 'all' => [ qw(
				      has_repeating_blocks
) ] );

our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );

our @EXPORT = qw(
	
);

our $VERSION = '0.01';

# Big-O: nlog(n) ?
sub has_repeating_blocks {
    my( $hex, $block_size ) = @_;
    my $len = length( $hex );
    my $chunk = 2 * $block_size;

    for(my $i = 0; $i < $len; $i += $chunk ) {
	my $byte = substr( $hex, $i, $chunk );
	for(my $j = $i + $chunk; $j < $len; $j += $chunk ) {
	    my $cmp = substr( $hex, $j, $chunk );
	    if( $byte eq $cmp ) {
		return 1;	# true
	    }
	}
    }
    return 0;			# false
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
