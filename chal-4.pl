#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

my %hex_digit = (
    0 => '0000',
    1 => '0001',
    2 => '0010',
    3 => '0011',
    4 => '0100',
    5 => '0101',
    6 => '0110',
    7 => '0111',
    8 => '1000',
    9 => '1001',
    A => '1010',
    B => '1011',
    C => '1100',
    D => '1101',
    E => '1110',
    F => '1111',
);
   
while (<>) {
    chomp $_;
    &crypanalysis( uc $_ );
}

sub crypanalysis {
    my $c = decode( shift, \%hex_digit ); # ciphertext
    
    for ( 32..127 ) {		# one char xor
	my $key = dec2bin($_);
	while( length($key) < 8) {
	    $key = "0" . $key;
	}
	my $m = &repeating_xor( $c, $key );

	# now filter
	$m = filter_by_printable( $m );
    
	if ( defined $m ) {
	    $m = filter_by_character( $m );
	}
	if ( defined $m ) {
	    my $string = &encode_ascii( $m );
	    print "$string\n";
	}
    }
}

# maximum character frequencies
sub filter_by_character {
    my $bits = shift;
    my $s = encode_ascii($bits);
    $s = lc $s;
    my $len = length( $s );
    
    my %symbols;
    $symbols{ $_ }++ for ( split //, $s );
    my ($nalpha, $percent);
    
    for ("a" .. "z") {
	if (exists $symbols{$_}) {
	    $nalpha += $symbols{$_};
	}
    }
    if (exists $symbols{' '}) {
	    $nalpha += $symbols{' '};
	}

    # filter punctuation
    if (defined $nalpha) {
	$percent = $nalpha / $len;
    }
    unless (defined $percent) {
	return undef; }
    if ($percent < 0.95) {
	return undef; }
    # my $len = length($s);
    # my $percent = $sum /    length( $s );
    # print "max: sum: $sum l: $len p: $percent\n";

    return $bits;
}

# return input string if all characters are printable, undef if not
sub filter_by_printable {
    my $input = shift;
    my $bits = $input;
    my $unp = 0;
#    print length ( $bits );
    if( (length( $bits ) % 8) != 0 ) {
	die "Bit string not byte aligned\n";
    }
    
    while( length( $bits ) > 0 ) {
	my $byte = substr( $bits, 0, 8 );
	$bits = substr( $bits, 8 );
	$unp++ unless is_printable( $byte );
#	return undef unless is_printable( $byte );
    }
    # my $len = length($input);
    # # filter punctuation
    # if (defined $unp) {
    # 	$percent = $unp / $len;
    # }
    # if (defined $percent) {
    # 	if ($percent > 0.05) {
    # 	    return undef;
    # 	}
    # }
    if ($unp > 5) {
	return undef;
    }
#    print "unp: $unp\n";
	    
    return $input;
}

sub is_printable {
    my $byte = shift;		# 8 digit bit string

    my $hex = encode( $byte, \%hex_digit ); # two hex digits
#    $hex = "Ox" . $hex;		   # let Perl know it is hex
 #   print "hex: $hex\n";
    if ( (hex( $hex ) >= 32) && (hex( $hex ) <= 126 ) ) {
	return 1;		# true
    }
    return 0;			# false
}
sub dec2bin {
    my $str = unpack("B32", pack("N", shift));
    $str =~ s/^0+(?=\d)//;   # otherwise you'll get leading zeros
    return $str;
}

# bits string it ASCII characters
sub encode_ascii {
    my $bits = shift;
    my $hex = encode( $bits, \%hex_digit );

    $hex =~ s/(([0-9a-f][0-9a-f])+)/pack('H*', $1)/ie;
    return $hex;
}

# return front 'n' chars and rest of string
sub front {
    my ($string, $nchars) = @_;
    my $front = substr( $string, 0, $nchars );
    $string = substr( $string, $nchars );
    ($string, $front);
}

# xor input bit string against key
sub repeating_xor {
    my( $m, $key ) = @_;
    my $out;

    my $klen = length( $key );
    my $mlen = length( $m );
    if ( $mlen % $klen != 0 ) {
	die "length( m ) % klen != 0\n klen: $klen mlen: $mlen\n";
    }
    while( length( $m ) > 0 ) {
	my $front;
	($m, $front) = &front($m, $klen);
	$out .= xor_same_length( $front, $key );
    }
    return $out;
}

# xor same length bit strings
sub xor_same_length {
    my ($m, $k) = @_;
    my $c;
    if (length($m) != length($k)) {
	die "Strings should be same length $m $k\n";
    }
    while (length($m) > 0) {
	my( $a, $b );		# front bits
	($m, $a) = &front($m, 1);
	($k, $b) = &front($k, 1);

	if ( ($a eq "1" and $b eq "1") ||
		 ($a eq "0" and $b eq "0") ) {
	    $c .= "0";
	} else {
	    $c .= "1";
	}
    }
    return $c;
}


# bits string to encoded format
sub encode {
    my( $bits, $digits ) = @_;
    my $es;
    my $len;			# bits per digit

    my @values = @$digits{keys %$digits};
    $len = length( $values[0] );

    while (length($bits) > 0) {
	my $b = substr($bits, 0, $len);
	$bits = substr($bits, $len);
	$es .= bits_to_digit($b, $digits);
    }

    return $es;
}

# encoded string to bit string
sub decode {
    my( $es, $digits ) = @_;
    my $bits;
    while (length($es) > 0) {
	my $digit = substr($es, 0, 1); # get first digit
	$es = substr($es, 1);	# and move along
	$bits .= $$digits{$digit};
    }
    return $bits;
    
}

# convert bits to digit
sub bits_to_digit {
    my ($bits, $digits) = @_;
    for (keys %$digits) {
	if ($$digits{$_} eq $bits) {
	    return $_;
	}
    }
    die "Digit not found\n";
}
