#include <math.h>
#include <stdio.h>
#include <stdlib.h>


void adjustGrid(float grid[180][360], float difference) {
	int	i, j;
	float	sum=0;

	for (i=0; i<180; i++)
		for (j=0; j<360; j++) {
			if (grid[i][j] == 0) continue;
			grid [i][j] -= difference;
			}
}//meanVal

float meanVal(float grid[180][360]) {
	int	i, j;
	float	sum=0;

	int count = 0;
	//for (i=0; i<180; i++)
	for (i=20; i<160; i++)
		for (j=0; j<360; j++) {
			if (grid[i][j] == 0) continue;
			count++;
			sum += grid [i][j];
		}

	return (sum/(float)count);
}//meanVal

void loadGrid(char *filename, float grid[180][360]) {
	int i, j;
	int iLat, iLon;

	for (i=0; i<180; i++)
		for (j=0; j<360; j++)
			grid [i][j] = 0;

	FILE *in = fopen (filename, "r");
	if (! in) {
		printf ("Couldn't open %s\n", filename);
		fprintf (stderr, "Couldn't open %s\n", filename);
		exit (-1);
	}//if
   int num;
   fscanf (in, "%d\n", &num);
   //printf ("# num: %d\n", num);

   long max = num*num*num;
max = 396293;
   //fprintf (stderr, "max %ld\n", max);
   float maxVal, maxLat, maxLon;
   maxVal = 0;
  
   for (i=0; i<max; i++) {
      float x, y, z, sig;
      float lat, lon, r;

      fscanf (in, "%f %f %f\n", &x, &y, &z);
      x *= 1000;     // convert to meters
      y *= 1000;     // convert to meters
      z *= 1000;     // convert to meters
      r = sqrt (x*x + y*y + z*z);
      lat =acos (z/r);
      lon = atan2 (y,x);
      lat *= 180/3.1415;
      //lat = 90 - lat;
      lon *= 180/3.1415;
      if (lon < 0) lon += 360;
      lon = 360 - lon;     // Switching to W Lon

      if (r > maxVal) {
         maxVal = sig;
         maxLat = lat;
         maxLon = lon;
      }// 
      //printf ("%3.5f %3.5f %3.5f\n", lat, lon, r);
		//lat += 90;		// go from 0-180 rather than -90 to 90
		iLat = (int) lat;
		iLon = (int) lon;
		grid [iLat][iLon] = r;
	}//for

}//

int main (int argc, char *argv[])
{
	char *str1, *str2;
	if (argc == 3) {
		str1 = argv [1];
		str2 = argv [2];
	}
	else {
		printf ("Usage: %s <file1> <file2>\n", argv[0]);
		exit (0);
	};

	float grid1[180][360];
	float grid2[180][360];

	// Read the files and load the vectors into a gridded product
	loadGrid (str1, grid1);
	loadGrid (str2, grid2);

	// Calulcate difference and print
	float max = 0;
	int mLat, mLon;
	FILE *out;
	out = fopen ("delta.txt", "w");
	int i, j;
	float delta[180][360];
	float sum=0;
	for (i=0; i<180; i++){
		for (j=0; j<360; j++) {
			delta [i][j] = grid1[i][j] - grid2[i][j];

			if (grid1[i][j] == 0) delta [i][j] = 0;
			if (grid2[i][j] == 0) delta [i][j] = 0;
			if (grid1[i][j] > 1e7) delta [i][j] = 0; // 10,000 km
			if (grid2[i][j] > 1e7) delta [i][j] = 0; // 10,000 km
			sum += delta[i][j] * delta[i][j];
			//if (delta [i][j] < -3) 		// error checking
				//printf ("%e %e %e\n", grid1[i][j], grid2[i][j], delta[i][j]);
			if (max < delta[i][j]) {
				max = delta[i][j];
				mLat = i;
				mLon = j;
			}//if

			fprintf (out, "%3.5e	", delta [i][j]);
		}//forj
		fprintf (out, "\n");
	}//fori
	fprintf (stderr, "# RMS %3.3f\n", sqrt (sum/360.0/180.0));
	fprintf (out, "# RMS %3.3f\n", sqrt (sum/360.0/180.0));
	fprintf (out, "# Max %3.3f (%d Lat %d Lon)\n", max, 90-mLat, mLon);
	fprintf (stderr, "# Max %3.3f (%d Lat %d Lon)\n", max, 90-mLat, mLon);
	fclose (out);

	float mean1 = meanVal (grid1);
	float mean2 = meanVal (grid2);
	float difference = mean1 - mean2;
	printf ("# Mean Val %3.4f	%3.4f	%3.4f\n", mean1, mean2, difference);
	adjustGrid (grid1,  difference);

	out = fopen ("delta-adj.txt", "w");
	sum=0;
	for (i=0; i<180; i++){
		for (j=0; j<360; j++) {
			delta [i][j] = grid1[i][j] - grid2[i][j];

			if (grid1[i][j] == 0) delta [i][j] = 0;
			if (grid2[i][j] == 0) delta [i][j] = 0;
			if (grid1[i][j] > 1e7) delta [i][j] = 0; // 10,000 km
			if (grid2[i][j] > 1e7) delta [i][j] = 0; // 10,000 km
			sum += delta[i][j] * delta[i][j];
			fprintf (out, "%3.5e	", delta [i][j]);
		}//forj
		fprintf (out, "\n");
	}//fori
	fprintf (stderr, "# Adjusted RMS %3.3f\n", sqrt (sum/360.0/180.0));
	fprintf (out, "# Adjusted RMS %3.3f\n", sqrt (sum/360.0/180.0));
	fclose (out);


}//main
