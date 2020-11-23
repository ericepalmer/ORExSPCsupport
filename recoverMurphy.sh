#!/bin/bash
# Eric E. Palmer
# recoveryMurphy.sh
#		This will copy the needed LMK & MAP files.
#		It will look for your user ID version of murphy 
#				(symbolically link or sh user to change)
#			For example:  ../epMurphy from working
#		Filelist is a list of maplet names to be moved into the working directory
#
# Usage:
#		recoverMurphy.sh EF0010
# or
#		recoverMurphy.sh tmpList
#
#			tmpList would have a single maplet name on each line.  
#			No END is needed, but can be included



name=$1



##########################################################################3
# Make sure you are in a valid directory (hopefully a working dir)
#		Tests to see if ../<id>Murphy exists
#		If not, then there is nothing to copy, so exit
##########################################################################3
who=`whoami | cut -c 1-2`
path="../${who}Murphy"
if [ ! -e $path ]
then
		echo "Error, $path does not exist"
		exit 
fi


# Create a del directory to give minimal backup 
mkdir -p ../del





##########################################################################3
# Ensure there is an argument given
##########################################################################3
if [ "$name" == "" ]
then
	echo "Error, need a file that is a list of files"
	echo "Usage: $0 <filename | maplet_name>"
	exit 
fi

##########################################################################3
# If the name is a valid path to a file, use that file to make a list.
#		Otherwise, set the list to be the name of the maplet
#		If an invalid name, it will error later in the lop
##########################################################################3
if [ -e $name ]
then

	list=`cat $name`
else
	list=$name
fi


# log
echo "#" $0 $1 | tee tmpOut.txt
echo "#" `date` | tee -a tmpOut.txt


##########################################################################3
# Loop through each item in the list (until done)
# If it exists
#		- Move to ../del
#		- Copy from <id>Murphy/whatever? to the correct directory
##########################################################################3
for item in $list
do


	# Check for "END" and stop
	if [ "$item" == "END" ]
	then
		echo "END found - terminating" | tee -a tmpOut.txt
		exit
	fi

	echo -n "$item" | tee -a tmpOut.txt

	# Do the MAPFILES
	file="$path/MAPFILES/$item.MAP"

	if [ ! -e  $file ]
	then
		echo -n " MAPFILE failed ($file)" | tee -a tmpOut.txt
	else
		/bin/mv -f MAPFILES/$item.MAP ../del/
		/bin/cp -f $file MAPFILES/
	fi

	# Do the LMKRLIST
	file="$path/LMKFILES/$item.LMK"
	#echo "		Working on $file"

	if [ ! -e  $file ]
	then
		echo -n " LMKFILES failed  ($file)" | tee -a tmpOut.txt
	else
		/bin/mv -f LMKFILES/$item.LMK ../del/
		/bin/cp -f $file LMKFILES/
	fi

	echo | tee -a tmpOut.txt

done

