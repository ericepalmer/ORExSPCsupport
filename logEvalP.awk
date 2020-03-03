BEGIN {
	nofit = 0;
	nocorr = 0;
	str = "";
	done = 0;
	imgNr = 0;
	}
// { 
}
/picnm/ {			# resets each time we do a 1-0-1
	imgNr = 0;
	}

						# counts the image number that correlates
/\+/ {
	imgNr ++;
	}

						# How many overlaps don't fit
/No fit/ {
	nofit++;
	}

						# Flag for image that has no correlation
/0.0000    0.0000/ {
	nocorr++;
	str = str "\n		" ($1) " "  $2 
	}

						# The program finished and exited cleanly
/DONE/ {
	done = 1;
	}
	
						# Print out the values
END {
	if (nofit || nocorr) 
		if (imgNr < 4)
			printf ("	%s (%d) ", id, imgNr);
		else
			printf ("	%s ", id);
	if (nofit) printf ("  nofit: %d ", nofit);
	if (nocorr) printf ("  NOCORR: %d %s", nocorr, str);
	if (nofit || nocorr) printf ("\n");
	if (!done) printf ("	Running %s\n", id)
	}

