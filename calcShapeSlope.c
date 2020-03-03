#include <math.h>
#include <stdio.h>
#include <stdlib.h>


int main (int argc, char *argv[])
{
	char *str = "shape3.4-256";
	FILE *in;
	if (argc == 2) str = argv [1];
	int i, j;

	in = fopen (str, "r");
	if (! in) {
		printf ("Couldn't open %s\n", str);
		fprintf (stderr, "Couldn't open %s\n", str);
		exit (-1);
	}

	int num;
	fscanf (in, "%d\n", &num);
	printf ("# num: %d\n", num);

	long max = num*num*num;
	max = 99847;		// Q=128
	if (num == 256) max = 396293;		// Q=256
	if (num == 128) max = 99847;		// Q=128

	fprintf (stderr, "Max %ld\n", max);
	float maxVal, maxLat, maxLon;
	maxVal = 0;
   float grid [180][360];
	for (i=0; i<180; i++) 
		for (j=0; j<360; j++) 
			grid [i][j] = 0;
	
	for (i=0; i<max; i++) {
		float x, y, z, sig;
		float	lat, lon, r;

		fscanf (in, "%f %f %f \n", &x, &y, &z);
		x *= 1000;		// convert to meters
		y *= 1000;		// convert to meters
		z *= 1000;		// convert to meters
		r = sqrt (x*x + y*y + z*z);
		lat =acos (z/r);
		lon = atan2 (y,x);
		lat *= 180/3.1415;
		//lat = 90 - lat;
		lon *= 180/3.1415;
		if (lon < 0) lon += 360;
		lon = 360 - lon;		// Switching to W Lon

		if (sig > maxVal) {
			maxVal = sig;
			maxLat = lat;
			maxLon = lon;
		}// 
      int iLat = (int) lat;
      int iLon = (int) lon;
		//printf ("%d %d %d %f %f	\n", i, iLat, iLon, x, y);
      grid [iLat][iLon] = r;


		//fprintf (out, "%3.5f	%3.5f	%3.5f	%3.9f\n", lat, lon, r, sig);
	//break;
	}
	//printf ("# %3.5f	%3.5f	%3.9f\n", maxLat, maxLon, maxVal);
	//fprintf (out, "# %3.5f	%3.5f	%3.9f\n", maxLat, maxLon, maxVal);

	FILE *out = fopen ("slope.txt", "w");
	for (i=0; i<180; i++) {
		for (j=1; j<360; j++) {
			float val = grid [i][j];
			float prev = grid [i][j-1];
			float slope = val-prev;
			if (! val) slope = 0;
			if (! prev) slope = 0;
			fprintf (out, "%8.5f	", slope);
			}//forj
		fprintf (out, "\n");
		}//for
	

}//main
