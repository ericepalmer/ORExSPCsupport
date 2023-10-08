// Eric E. Palmer
// 	6 Oct 2023
//		This takes a list of points from a triax and merges it with limb points
//		It will "blank" the triax where there are limbs

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>


struct t_pt {
	float	x;
	float y;
	float z;
};

//--------------------------------------------------
float dist (struct t_pt tri, struct t_pt limb){

	float dx, dy, dz;
	dx = tri.x - limb.x;
	dy = tri.y - limb.y;
	dz = tri.z - limb.z;
	return (sqrt (dx*dx  + dy*dy + dz*dz));

}//dist

//--------------------------------------------------
int readList (char *name ){
	
	FILE *in = fopen (name);
	if (! in) {
		fprintf (stderr, "Can't open %s\n", name);
		exit (-1);
	}


	while (! feof (in) ){
		struct t_pt val;
		fscanf (in, "%f %f %f\n", &(pt.x), &(pt.y), &(pt.z)
		printf( ("%f %f %f\n", pt.x, pt.y, pt.z);
	}//while

}//readlist

//--------------------------------------------------
struct ptsL[1000];
struct ptsT[1000];
float cutoff = .050;		// distance to clear the tri around the limb
int	numL;
int	numT;
//--------------------------------------------------
void blankVertex (struct t_pt limb) {
	int	i;

	for (i=0; i<numT; i++) {
		dist = dist (ptsT[i], limb);
		if (dist < cutoff) {
			ptsT[i].x = -9999;
			ptsT[i].y = -9999;
			ptsT[i].z = -9999;
}//blankvertex

//--------------------------------------------------
int main (int argc, char *argv []) {

	char *nameL="pts41a";
	char *nameT="tri-15";

	int numL = readList (nameL);
	int numT = readList (nameT);
	
	int	i;
	for (i=0; i<numL; i++) {
		blankVertex (limbPt [i]);
	}//for


}//main





