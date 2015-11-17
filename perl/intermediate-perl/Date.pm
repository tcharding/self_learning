package Date;

use strict;
use warnings;
use feature qw/say/;
use Carp;

sub UNIVERSAL::debug {
    my( $class, $s ) = @_;
    my $date = `date`;
    chomp $date;
    print "$date: $s\n";
}

sub AUTOLOAD {
    our $AUTOLOAD;
    (my $method = $AUTOLOAD) =~ s/.*:://s; # remove package name
	if ($method eq "date") {
	    ## define date
	    eval q{
sub date {
my $date = `date`;
my @d = split / /, $date;
printf "%s %s %s\n", $d[0], $d[2], $d[1];
}
}; # end of eval
	    die $@ if $@;	# die if typo
	    goto &date;
	} else {
	    croak "$_[0] does not know how to method\n";
	}

}
1;
