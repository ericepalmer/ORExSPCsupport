// Eric E. Palmer
// 	16 Feb 2017
// Evaluate Cardinal.
// This is used to see how well a suite of images covers
//		a maplet.  It is evaluating the 4 topographic image
//		stations and albedo.
//		The basis is that a topo image has about 90° between each
// Options
//		l is for long format.  Show all images by category
//		s is for short format.  Only show missing
//		v is version
//		h is help.  Tells the arguments

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

float lat = -10;		// Latitude of the feature
float g_version = 1.00;

// Gets rid of the first line of headers out of the CSPLOT.TXT
void dumpLine (FILE *in) {
	char ch;

	ch = fgetc (in);
	while (ch != '\n') {
		ch = fgetc (in);
		if (feof (in)) return;
//		printf ("%c", ch);
	}//while
//	printf ("\n");

}

enum dirType {none=0, north=1, east, south, west};
enum componentType { azimuth=0, zenith=1};
enum statusType { eUnused, eFail, eAlbedo, eTopo};

int gridA [360][360];
float iV [500][2];	
float eV [500][2];	
int dirA [500];
enum statusType	statusA [500];
char	flag ;

int main (int argc, char *argv []) {
	char *filename = "CSPLOT.TXT";

	flag = 0;
	if (argc == 2) {
		if (argv [1][0] == '-') {
			flag = argv [1][1];
			if (flag == 'v'){
				printf ("Version: %3.2f\n", g_version);
				exit (0);
			}//ifflag
			if (flag == 'h'){
				printf ("Usage: %s [-hvls] [file]\n", argv [0]);
				exit (0);
			}//ifflag
		}//if
		else
			filename = argv [1];
	}//ifargc

	FILE	*in = fopen (filename, "r");
	if (in == NULL) { printf ("Can't read %s\n", filename); exit (-1); }

	dumpLine (in);

	int	i;
	int 	dir;
	int 	index = 0;
	int 	fail;
	int 	albedo;
	int	topo;
								// Running in 30° bins
	int	azBin [20];		// logs which quadrant it is in

	// initialize the variable
	for (i=0; i<20; i++)	azBin[i] = 0;
	for (i=0; i<500; i++)	statusA[i] = eUnused;

	char buf [80];
	float sun_el, sun_az, sc_el, sc_az;
	while ( ! feof (in) ) {
		fscanf (in, "%s %f %f %f %f", buf, &sun_el, &sun_az, &sc_el, &sc_az);
		if (feof (in)) break;
		fail = 0;
		dir = 0;
		albedo = 0;

		// Get the numbers into stable environments
		if (sc_az < 0) sc_az += 360;
		if (sun_az < 0) sun_az += 360;
		iV [index][zenith] = 90 - sun_el;
		iV [index][azimuth] = sun_az;
		eV [index][zenith] = 90 - sc_el;
		eV [index][azimuth] = sc_az;

		// Check for valid images
		if (iV [index][zenith] > 60) fail = 1;  // skip beyond 60deg
		if (eV [index][zenith] > 60) fail = 1;		// skip beyond 60deg

		// Look for local hour near noon -- for Northern view`
		if ( (iV [index][azimuth] >= 170) && 
			  (iV [index][azimuth] <= 190) ) albedo = 1;  // flag high sun

		// Look for local hour near noon -- for Southern view`
		if ( (iV [index][azimuth] <= 10) || 
			  (iV [index][azimuth] >= 350) ) albedo = 1;  // flag high sun

		// Determine which 30° bin the emission azimuth is in
		if ( (albedo == 0) && (fail == 0) )
		if (eV [index][zenith] > 20) {		// skip very low emission angles
			float val = eV [index][azimuth] / 30.0;
			val += .5;
			if (val > 12) val = 0;		// deal with 350° to 10° issue
			dir = (int) val;
			azBin [dir]++;			
			statusA [index] = eTopo;
		}//if

	if (albedo) 
		statusA [index] = eAlbedo;

	if (fail) 
		statusA[index] = eFail;

	dirA [index] = dir;

		index++;

	}//while

	if (flag == 'l') {
		printf ("#Sun	Sun		Spacecraft	Spacecraft	Dir\n");
		printf ("#Albedo images\n");
	}

	albedo = 0;
	for (i=0; i<index; i++) 
		if (statusA[i] == eAlbedo) {
		albedo++;
		if (flag == 'l')
		fprintf (stderr, "	%3.1f %3.1f	%3.1f %3.1f\n", 
					iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith]);
		}//if

	if (flag == 'l') {
	for (dir=0; dir<12; dir++) {
		printf ("#Topo images Direction %d\n", dir);
		for (i=0; i<index; i++)
			if (statusA[i] == eTopo)
				if (dirA[i] == dir)
				fprintf (stderr, "	%3.1f %3.1f	%3.1f %3.1f\n", 
					iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith]);
		}//fori

	printf ("# Zenith angle too high\n");
	for (i=0; i<index; i++)
		if (statusA[i] == eFail)
		fprintf (stderr, "	%3.1f %3.1f	%3.1f %3.1f\n", 
					iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith]);

	printf ("# Unused - too low emission angle and not albedo\n");
	for (i=0; i<index; i++)
		if (statusA[i] == eUnused)
		fprintf (stderr, "	%3.1f %3.1f	%3.1f %3.1f\n", 
					iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith]);

	printf ("Sections\n");
	for (i=0; i<13; i++)
		printf ("%d %d\n", i, azBin [i]);
	}//if flag

	int	groupA[3];
	int	location;
	groupA[0] = (azBin[0]>0) + (azBin[3]>0) + (azBin[6]>0) + (azBin[9]>0); 
	groupA[1] = (azBin[1]>0) + (azBin[4]>0) + (azBin[7]>0) + (azBin[10]>0); 
	groupA[2] = (azBin[2]>0) + (azBin[5]>0) + (azBin[8]>0) + (azBin[11]>0); 

	location = 0;
	int hold = groupA[0];
	if (groupA[1] > hold) {
		hold = groupA[1];
		location = 1;
	}//if
	if (groupA[2] > hold) {
		hold = groupA[2];
		location = 2;
	}//if
	
	if (flag != 's') {
		printf ("\n\nFull listing\n");

		for (i=0; i<3; i++) {
			printf ("	%d-%d-%d-%d", 0+30*i, 90+30*i, 180+30*i, 270+30*i);
			printf ("	- (%d %d %d %d) = %d\n", 
				azBin[0+i], azBin[3+i], azBin[6+i], azBin[9+i], groupA[i]);
			}//fori
		printf ("#############################\n");
		printf ("Best %d-%d-%d-%d\n", 
			0+30*location, 90+30*location, 180+30*location, 270+30*location);
		printf ("%d --- (%d %d %d %d)\n", groupA[location], 
				azBin[0+location], azBin[3+location], 
				azBin[6+location], azBin[9+location]);
		printf ("There are %d albedo images\n", albedo);
	}//ifflag

	if (hold == 4) printf ("You have all 4 topographic images\n");
	else {
		printf ("Missing: ");
		if (azBin [0+location] == 0) printf ("North ");
		if (azBin [3+location] == 0) printf ("East ");
		if (azBin [6+location] == 0) printf ("South ");
		if (azBin [9+location] == 0) printf ("West ");
		if (albedo == 0) printf ("	Albedo");
		printf ("\n");
	}//else
	

}//main
