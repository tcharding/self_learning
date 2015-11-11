sub attack_block {
    my( $iv, $first, $second ) = @_;

    my $p = "";

    my $found;
    for my $byte (1 .. 16) {
	my $modi = 32 - ($byte * 2); # byte index we are modifying
	
	my $front = substr($first, 0, $modi); # bit still to do (hex)
	my $mod = substr($first, $modi, 2);   # ciphertext byte to modify
	my $back = "";			      # back of block

	if (length( $p ) > 0) {		       
	    my $b = substr($first, $modi + 2); # +2 for mod_byte
	    $back = build_known( $p, $byte, $b );
	}
	
	my $ct = pack('H*', $mod);    # ciphertext byte
	my $pd = pack('C*', $byte); # target padding byte
	$found = 0;		      
	for (1..16, 32..126) {	# padding characters plus printable characters
	    my $char = chr($_);	
	    my $pt = pack('A*', $char);	# guessed plaintext byte

				# build modified block
	    my $mod_block = pack('H*', $front); # front
	    $mod_block .= $pt ^ $ct ^ $pd;;  # add modified byte
	    if ($back ne "") {		     # add back if defined
		$mod_block .= pack('H*', $back);  
	    }				     
	    $mod_block .= pack('H*', $second);   # add second block

	    			# query the oracle
	    if (&padding_oracle( $iv, $mod_block ) ){
		$p = $char . $p;
		$found = 1;
#		printf "got char:*%s* (hex: %s)\n", $char, ascii_to_hex($char);
	    }
	    last if ($found == 1); # end guess character loop
	} # end guess character loop
	last if ($found == 0); # end byte loop
    } # end byte loop
    if ($found == 0) {
	die "Error, char unfound\n";
    }
    return $p;
}
