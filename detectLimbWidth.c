// DetectLimbWidth.c
// Eric E. Palmer - 28 Feb 2018
// Caclulates how many pixels from the edge the asteroid is.  It helps
//   us determine if there is sufficinet non-limb info
//   in an image for SPC to use it

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

unsigned short imgA [1025][1025];
int	debug  = 0;

float search (short dx, short dy, short index) {


	short x = 512;
	short y = 512;
	short valid = 1;

	while (valid) {
		
		if (imgA [x][y] > 10) {
			if (debug) fprintf (stdout, "#  %d:  %d %d %d %d\n", index, x, y, dx, dy);
			short maxX = 512 + 512 * dx;
			short maxY = 512 + 512 * dy;
			short tmpX = maxX - x;
			short tmpY = maxY - y;
			float dist = sqrt (tmpX * tmpX + tmpY * tmpY);
			if (debug) fprintf (stdout, "#  %d:  %d - %d and %d - %d gives %3.1f\n", index, maxX, x, maxY, y, dist);

			return dist;
		}//imgA

		x += dx;
		y += dy;
		if (x  < 0) valid = 0;
		if (y  < 0) valid = 0;
		if (x  > 1023) valid = 0;
		if (y  > 1023) valid = 0;

	}//while
	return 0;

}//search


int main (int argc, char *argv[])
{
	char *nameStr = "TEMPFILE.pgm";
	float version = 1.0;

	if (argc == 2) {
		nameStr = argv[1];
	}//argc

	if (strcmp (nameStr, "-v") == 0) {
		printf ("%s Version %3.2f\n", argv[0], version);
		exit (0);
	}//if

	if (debug) printf ("name %s\n", nameStr);
	FILE *inF = fopen (nameStr, "r");
	if (! inF) {
                fprintf (stderr,"%s Cannot be read\n", nameStr);
                exit (-1);
	}//if

	int pixels, lines, bits;
	char buf [10];
	buf [0] = ' ';
	
	while (buf [0] != '#') {
		fscanf(inF, "%s", buf);
		if (debug) printf ("# Magic %s\n", buf);
	}//while


	fscanf(inF, "%d", &pixels);
	fscanf(inF, "%d", &lines);
	fscanf(inF, "%d", &bits);
	if (debug) printf ("# pixels: %d, lines %d\n", pixels, lines);
	if (debug) printf ("# bits %d\n", bits);

	int i, j;
	unsigned char ch;
	for (i=0; i<pixels; i++) {
		for (j=0; j<lines; j++) {
			ch = fgetc (inF); 
			imgA [i][j] = (unsigned short) ch;
			//printf ("%u ", ch);
		}//for j
		//printf ("\n");
	}//fori

	if (debug) printf ("# done reading file\n");



	float dist, holdDist;
	float newDist;
	dist = 0;

	// Line
	holdDist = search (1, 0, 1);
	if (holdDist > dist) dist = holdDist;
	holdDist = search (0, 1, 2);
	if (holdDist > dist) dist = holdDist;
	holdDist = search (-1, 0, 3);
	if (holdDist > dist) dist = holdDist;
	holdDist = search (0, -1, 4);
	if (holdDist > dist) dist = holdDist;

	// Diag
	holdDist = search (1, 1, 5);
	if (holdDist > dist) dist = holdDist;
	holdDist = search (1, -1, 6);
	if (holdDist > dist) dist = holdDist;
	holdDist = search (-1, 1, 7);
	if (holdDist > dist) dist = holdDist;
	holdDist = search (-1, -1, 8);
	if (holdDist > dist) dist = holdDist;


	fprintf (stdout, "# Name                  Distance \n");
	fprintf (stdout, "%20s    %3.1f\n", nameStr, dist);

	return (0);

}
