# Eric E. Palmer - 29 Aug 2015
# Version 1.0
# Breaks apart a MAPINFO formatted file (created by RESIDUALS) by resolution
# It makes a list of all the landmarks of that resolution
# Run this by using 
#		awk -f breakByRez.awk MAPINFO.TXT
# Use this with USED_MAPS.TXT to make a sublist at limited resolutions by
#		grep -f USED_MAPS.TXT breakOut-10.txt

# Init some variables -- not needed
BEGIN { 
}

# When it finds the end of the list of landmarks, stop running
#		the awk script (and go to END)
/END/	{ exit; }

# Matches every string
//	{ 
	# When the resolution is different, print statistics & change filenames
	if (currVal != $2 ) {

		if ( filename) {   # skip the first time
			print "END" > filename 
			print currVal, count;
		}#if filename

		# Set the variables for the next resoluiton's filename
		currVal = $2;
		filename = "breakout-" $2
		print "#", $2, $3 > filename
		count = 0;
	}#if

	# This is done every line
	count++;
	print $1 > filename
}

# Once it finishes running, print out the statistics for the last file
END { 
	print currVal, $3, count;
}
