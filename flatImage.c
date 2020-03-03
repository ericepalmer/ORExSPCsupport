// flatImage.c -- 5 May 2017 -- Eric E. Palmer
// 	This takes an SPC IMAGEFILES <filename.DAT> file and 
//		outputs it into an ASCII text file.

#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv []) {
	char	buf [256];
	int	i, j;

	printf ("Input DAT file\n");
	scanf ("%s", buf);

	FILE *in = fopen (buf, "r");
	if (in == NULL) { fprintf (stderr, "Can't open %s\n", buf); exit (-1); }

	unsigned short	val;
	unsigned char a[2], b[2];
	FILE *out = fopen ("out.txt", "w");
	for (i=0; i<1024; i++) {
		for (j=0; j<1024; j++) {
			int err = fread (a, sizeof (unsigned char), 2, in);
			val = a[0];
			val = val<<8;
			val += a[1];
			fprintf (out, "%u	", val);
		}//forj
		fprintf (out, "\n");
	}//fori

}//main


