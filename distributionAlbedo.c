/////////////////////////////////////////////////////////////
//		distributionAlbedo
//		Eric E. Palmer
//		version 1.0 -- reads in two files and creates a dual histogram
/////////////////////////////////////////////////////////////
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define MAX 20002
int	truthSize = 251;
int	modelSize = 251;
float	version = 1.02;

struct stat_type {
	float	average;
	float	stdev;
	int	min, max;
};


//////////////////////////////////////////////////////////
// Calculate basic statistics for the matrix
struct stat_type *stats (int	*array [MAX], int aLen)
{
	int	i, j, count, val, max, min;
	double sum, average, stdev, delta, deltaSum;

	count = 0;
	sum = 0;
	min=1000;
	max=0;
	for (i=0; i<aLen; i++)
		for (j=0; j<aLen; j++) {
			val = array [i][j];
			if (val  == 0) continue;		// Skip 0 values (they are not real data)
			sum += val;
			count++;
			if (val < min) min = val;
			if (val > max) max = val;
		}//forj

	average = sum / (float) count;
	fprintf (stderr, "		# Average %f\n", average);

	deltaSum = 0;
	for (i=0; i<aLen; i++)
		for (j=0; j<aLen; j++) {
			val = array [i][j];
			if (val  == 0) continue;		// Skip 0 values (they are not real data)
			delta = average - val;
			deltaSum += delta*delta;
		}//forj

	stdev = sqrt (deltaSum/ (count-1 ));
	fprintf (stderr, "		# Min/Max/Del %d	%d	%d\n", min, max, max - min);
	fprintf (stderr, "		# Stdev %f\n", stdev);

	struct stat_type *curr = (struct stat_type *) malloc (sizeof (struct stat_type));
	curr->average = average;
	curr->stdev = stdev;
	curr->min = min;
	curr->max = max;

	return (curr);


}//stats

//////////////////////////////////////////////////////////
// Read in two matrices and do math
// ARguments -- baasic is just two matrixes
//		a add constant to adjust the matrix
//		b
//		m multiply constant to adjust the matrix
//		b

int main (int argc, char *argv[])
{
	FILE *modelF ;
	FILE *truthF ;
	int	*modelA [MAX];
	int	*truthA [MAX];
	struct stat_type *modelStat, *truthStat;
	char	*filename1, *filename2;
	char	flag = 0;
	float	value = 0;
	float	value2 = 0;

	if (argc == 3) {
		filename1 = argv[1];
		filename2 = argv[2];
	} else if (argc == 5) {
		if (argv[1][0] != '-') { printf ("Error in arguments\n"); exit (-1); }
		flag = argv [1][1];
		value = atof (argv [2]);
		fprintf (stderr, "# control: %c %f\n", flag, value);
		filename1 = argv[3];
		filename2 = argv[4];
	} else if (argc == 6) {
		if (argv[1][0] != '-') { printf ("Error in arguments\n"); exit (-1); }
		flag = argv [1][1];
		value = atof (argv [2]);
		value2 = atof (argv [3]);
		fprintf (stderr, "# control: %c %f	%f\n", flag, value, value2);
		filename1 = argv[4];
		filename2 = argv[5];
	} else {
		fprintf (stderr, "Please define both files\n");
		exit (-1);
	}
	fprintf (stderr, "Version: %3.2f\n", version);

	truthF = fopen (filename1, "r");
	if (! truthF) { printf ("Can't open the file %s\n", filename1); exit (-1);}
	modelF = fopen (filename2, "r");
	if (! modelF) { printf ("Can't open the file %s\n", filename2); exit (-1);}

	int	i, j;
	int	mCount[500];
	int	tCount[500];
	int	sum = 0;
	int 	index;
	float	data;

	// Initialize things
	for (i=0; i<200; i++){
		tCount [i] = 0;
		mCount [i] = 0;
		}

	for (i=0; i<MAX; i++) {
		modelA [i] = malloc (sizeof(int) * MAX);
		truthA [i] = malloc (sizeof(int) * MAX);
	}//for i

	// Read in truth
	char 	ch;
	for (i=0; i<MAX; i++) {
		for (j=0; j<MAX; j++) {
			ch = fgetc (truthF);			// look for newline
			if (ch == '\n') break;		// end the current interior loop
			if (ch == '\r') break;		// end the current interior loop
			if (ch == '#') break;		// end the current interior loop
			ungetc (ch, truthF);			// put temp char back

			int err = fscanf (truthF, "%f", &data);
			if (err == 0) break;
			truthA [i][j] = (int)(data*100.0);
		}//forj

	// end the loop
	if (feof (truthF)) { printf ("Truth %d\n", i); break;; }
	}//fori
	fclose (truthF);
	truthSize = i;
	
	if (i == MAX) {
		printf ("Error:  i reached max %d\n", i);
		printf ("Ensure there are no extra lines in the file");
		exit (-1);
	}//if

	// Read in model data
	for (i=0; i<MAX; i++) {
		for (j=0; j<MAX; j++) {
			ch = fgetc (modelF);			// look for newline
			if (ch == '\n') break;		// end the current interior loop
			if (ch == '#') break;		// end the current interior loop
			ungetc (ch, modelF);			// put temp char back

			int err = fscanf (modelF, "%f", &data);
			if (err == 0) break;
			modelA [i][j] = (int)(data*100);
		}//forj
		if (feof (modelF)) { printf ("Model %d \n", i); break;; }
	}//fori
	fclose (modelF);
	modelSize=i;

	if (i == MAX) {
		printf ("Error:  i reached max %d\n", i);
		exit (-1);
	}//if


	// Adjust model data
	if ((flag == 'm') || (flag == 'b'))  {
		fprintf (stderr, "		# Adjusting mult %f\n", value);
		for (i=0; i<modelSize; i++)
			for (j=0; j<modelSize; j++) 
				modelA [i][j] *= value;
	}//if

	
	if (value2) value = value2;
	if ((flag == 'a') || (flag == 'b'))  {
		fprintf (stderr, "		# Adjusting mult %f\n", value);
		for (i=0; i<modelSize; i++)
			for (j=0; j<modelSize; j++) 
				modelA [i][j] += value;
	}//if

	fprintf (stderr, "# Filename: %s\n", filename1);
	truthStat = stats (truthA, truthSize);

	fprintf (stderr, "# Filename: %s\n", filename2);
	modelStat = stats (modelA, modelSize);
			

	// Computer the distribution of the arrays
	for (i=0; i<truthSize; i++)
		for (j=0; j<truthSize; j++) {
			index = truthA [i][j];
			tCount [index]++;
		}//for j
	
	for (i=0; i<modelSize; i++)
		for (j=0; j<modelSize; j++) {
			index = modelA [i][j];
			mCount [index]++;
		}//for j
	
	// output the histogram
	FILE *outF = fopen ("histogram-alb.txt", "w");
	fprintf (outF, "# bin	truth	model\n");
	for (i=0; i<200; i++)
		fprintf (outF, "%d	%d	%d\n", i, tCount [i], mCount[i]);
	fclose (outF);

	// Saving the modified model output
	outF = fopen ("updated-albedo", "w");
	for (i=0; i<modelSize; i++) {
		for (j=0; j<modelSize; j++) 
			fprintf (outF, "%d	", modelA [i][j]);
		fprintf (outF, "\n");
	}// forj
	fclose (outF);
		

	// Calculate out the differences between the distrubtions
	fprintf (stderr, "Add: match average:	%f\n", truthStat->average - modelStat->average);
	float delta1 = truthStat->max - truthStat->min;
	float delta2 = modelStat->max -modelStat->min;
	fprintf (stderr, "Multiply: match average:	%f\n", truthStat->average / modelStat->average);
	fprintf (stderr, "Multiply: match min/max: %f	\n", delta1 / delta2 );
	fprintf (stderr, "Multiply: match stdev: %f	\n", truthStat->stdev / modelStat->stdev);


return 0;
}
