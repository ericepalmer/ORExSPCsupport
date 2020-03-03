// Eric E. Palmer
// 	16 Feb 2017
// Evaluate Cardinal.
// This is used to see how well a suite of images covers
//		a maplet.  It is evaluating the 4 topographic image
//		stations and albedo.
//		The basis is that a topo image has about 90° between each
// Options
//		l is for long format.  Show all good images by category
//		s is for short format.  Only show missing
//		a is for all format.  Shows everything, including not used images
//		v is version
//		h is harsh.   Has a more strict/harsh limits for angles
// Examples
//		cardinalCS  filename 	- shows basic version
//		cardinalCS -l filename 	- shows long version
//		cardinalCS -h filename 	- shows basic version with harsh critiera
//		cardinalCS -lh filename - shows long version with harsh critiera

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define true 1
#define false 0
#define Pi 3.14159

float lat = -10;		// Latitude of the feature
float g_version = 1.05;

// SIN - does the sine function with degree input
float SIN (float deg) {
	float rad = deg / 180.0 * Pi;
	return (sinf (rad));
}//sin

// COS - does the cosine function with degree input
float COS (float deg) {
	float rad = deg / 180.0 * Pi;
	return (cosf (rad));
}//cos


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

enum statusType { eNoStereo, eUnused, eFail, eAlbedo, eTopo, eNight, eSunLow, eEmission, eAvail};
enum dirType {zenith, azimuth};
enum levelType {e_strict, e_loose};

int gridA [360][360];
float iV [500][2];	
float eV [500][2];	
int dirA [500];
float sunA [500];
enum statusType	statusA [500];
char	flag ;
float	all = false;
int	level = e_loose;			
float noon;
int	maxI, maxE;

int main (int argc, char *argv []) {
	char *filename = "CSPLOT.TXT";

	if (argc == 1) {
		printf ("Usage: %s [-vlsa][h] file\n", argv [0]);
		exit (0);
	}//ifargc

	flag = 0;
	if (argv [1][0] == '-') {
		flag = argv [1][1];
		if (flag == 'v'){
			printf ("Version: %3.2f\n", g_version);
			exit (0);
		}//ifflag
	}//if
	filename = argv [argc-1];
	if (flag == 'a') {					// sets flag to get both long and all
		flag = 'l';
		all = true;
	}//if
	lat = atof (&(filename[8]));

	if (flag == 'h'){ 			// sets hard/strict flag
		level = e_strict;
		flag = '\0';
	}//ifflag

	if (argv[1][2] == 'h')
		level = e_strict;


	FILE	*in = fopen (filename, "r");
	if (in == NULL) { printf ("Can't read %s\n", filename); exit (-1); }
	printf ("\n# Using %s for Lat %3.1f: ", filename, lat);

	if (level == e_loose) {
		printf ("Loose\n");
		maxI = 70;
		maxE = 60;
		noon = 1.0;
	}//iflevel
	else {
		printf ("Harsh\n");
		maxI = 60;
		maxE = 50;
		noon = 0.5;
	}//elselevel

	dumpLine (in);

	int	i;
	int 	dir;
	int 	index = -1;
	int 	fail;
	int 	albedo;
	int	topo;
								// Running in 30° bins
	int	azBin [20];		// logs which quadrant it is in

	// initialize the variable
	for (i=0; i<20; i++)	
		azBin[i] = 0;
	for (i=0; i<500; i++)	
		statusA[i] = eAvail;

	char buf [80];
	float sun_el, sun_az, sc_el, sc_az;
	float lat, lon;
	float term1, term2, a, b, c, A;
	if (all)
		fprintf (stderr, "index	sun_el	sun_az	sc_el	sc_az\n");

///////////////////////////////
// Major loop --- read until EOF
	while ( ! feof (in) ) {
		index++;
		//fscanf (in, "%f %f %f %f %f %f", buf, &sun_el, &sun_az, &sc_el, &sc_az) // Kidd
		sun_el = sun_az = sc_el = sc_az = 0;		// reset var
		fscanf (in, "%s %f %f %f %f ", buf, &sun_el, &sun_az, &sc_el, &sc_az);	// Bob's CS
		//fscanf (in, "%s %f %f %f %f %f %f", buf, &lat, &lon, &sun_az, &sun_el, &sc_az, &sc_el);	// Lil
		if (feof (in)) break;

		if (all)
			printf ("%2d 	%3.1f 	%3.1f 	%3.1f 	%3.1f\n", index, sun_el, sun_az, sc_el, sc_az);
		fail = 0;
		dir = 0;
		albedo = 0;
		statusA [index] = eAvail;

		// Get the numbers into stable environments
		if (sc_az < 0) 
			sc_az += 360;
		if (sun_az < 0) 
			sun_az += 360;
		if (sun_el < 0) 							// Check night, must be done before zenith
			statusA [index] = eNight;

		iV [index][zenith] = 90 - sun_el;	// convert from elevation to zenith
		iV [index][azimuth] = sun_az;
		eV [index][zenith] = 90 - sc_el;		// convert from elevation to zenith
		eV [index][azimuth] = sc_az;

		// Solve for solar time of day using spherical trig
		b = iV [index][zenith];
		c = lat;
		A = 180.0 - iV [index][azimuth];
		term1 = COS (b) * COS (c);
		term2 = SIN (b) * SIN (c) * COS (A);
		a = acos (term1 + term2) * 180.0 / Pi;
		sunA [index] = a / 30.0;				// convert to hours

		/* debugging
		if (sunA [index] < 1.3) {
			printf ("T1 %3.2f\n", term1);
			printf ("T2 %3.2f\n", term2);
			printf ("b %3.2f		c %3.2f    A %3.2f\n", b, c, A);
			exit (0);
		}//if
		*/


		// Set flags for unusable images
		if (statusA [index] == eNight)
			continue;

		// Check for valid images
		if (iV [index][zenith] > maxI) {  					// skip beyond 60deg
			statusA [index] = eSunLow;
			continue;
		}//if

		if (eV [index][zenith] > maxE) { 						// skip beyond 50deg
			statusA [index] = eEmission;
			continue;
		}//if

		if (sunA [index] < noon)								// local hour within 7.5
			  statusA [index] = eAlbedo;  				// flag high sun

		// Determine which 30° bin the emission azimuth is in
		if (statusA[index] == eAvail) {
			if (eV [index][zenith] > 20) {		// skip very low emission angles
				float val = eV [index][azimuth] / 30.0;
				val += .5;
				if (val > 12) val = 0;		// deal with 350° to 10° issue
				dir = (int) val;
				azBin [dir]++;			
				statusA [index] = eTopo;
				dirA [index] = dir;
			}//if
			else
				statusA [index] = eNoStereo;
		}//ifstatus


	}//while


	albedo = 0;
	for (i=0; i<index; i++) 
		if (statusA[i] == eAlbedo) {
		albedo++;
		}//if

	///////////////////////////////
	// print long display
	if (flag == 'l') {				

		printf ("#	Sun	Sun		S/C	S/C\n");
		printf ("#	Az 	Zen 		Az 	Zen		DeltaHr\n");

		printf ("#Albedo Images\n");
		for (i=0; i<index; i++) 
			if (statusA[i] == eAlbedo) 
				fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f		%3.1fh\n", 
						iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith], sunA[i]);

		for (dir=0; dir<12; dir++) {
			printf ("#Topo images Direction %d\n", dir+1);
			for (i=0; i<index; i++)
				if (statusA[i] == eTopo)
					if (dirA[i] == dir)
					fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f		%3.1fh\n", 
						iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith], sunA[i]);
			}//fori

		if (all) {				// display all data, to include crummy images
			printf ("# Emission angle too high\n");
			for (i=0; i<index; i++)
				if (statusA[i] == eEmission)
					fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f		%3.1fh\n", 
						iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith], sunA[i]);
		
			printf ("# Incidence angle too high\n");
			for (i=0; i<index; i++)
				if (statusA[i] == eSunLow)
					fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f		%3.1fh\n", 
						iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith], sunA[i]);
		
			printf ("# Sun below the horizon (night)\n");
			for (i=0; i<index; i++)
				if (statusA[i] == eNight)
					fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f		%3.1fh\n", 
						iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith], sunA[i]);
		
			printf ("# Unused - too low emission angle and not albedo\n");
			for (i=0; i<index; i++)
				if (statusA[i] == eUnused)
				fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f\n", 
							iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith]);
	
			printf ("# Low stereo angle e < 20\n");
			for (i=0; i<index; i++)
				if (statusA[i] == eNoStereo)
					fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f		%3.1fh\n", 
						iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith], sunA[i]);
	
			printf ("# Avail\n");
			for (i=0; i<index; i++)
				if (statusA[i] == eAvail)
					fprintf (stdout, "	%3.1f 	%3.1f		%3.1f 	%3.1f		%3.1fh\n", 
						iV[i][azimuth], iV[i][zenith], eV[i][azimuth], eV[i][zenith], sunA[i]);
		}//ifall
	
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
		printf ("# Full listing\n");

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
