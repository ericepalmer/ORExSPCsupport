# 3 Nov 2014
# 	This builds the shape using standard techniques
#	Requires the starting shape model is 32 -- the same size as the Nolan model


if [ "$1" == "" ]
then
	echo "Usage: $0 <suffix>"
	exit
fi

myPath="/opt/local/spc/bin/"
suffix=-$1-

vers=$0

size=`head -1 SHAPEFILES/SHAPE.TXT | cut -c 1-5`
echo "Starting Shape Model is: $size"

date > shape.log

Q=`head -1 SHAPEFILES/SHAPE.TXT`
echo $Q

echo "SHAPEFILES/SHAPE.TXT" > tmpRun.txt
echo "SHAPEFILES/dumb$suffix" >> tmpRun.txt
echo "4" >> tmpRun.txt
echo "y" >> tmpRun.txt

cp SHAPEFILES/SHAPE.TXT SHAPEFILES/dumb$suffix			# used only with initial base model Q=32

list="64 128 256 512"
prev=dumb$suffix

for i in $list 
do
	current=shape$suffix$i
	echo "Working on $i"
	echo "SHAPEFILES/$prev" > tmpRun.txt
	echo "2 100 1.67773" >> tmpRun.txt
	echo "SHAPEFILES/$current" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo ".005" >> tmpRun.txt
	echo ".025" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "1" >> tmpRun.txt
	echo "0" >> tmpRun.txt
	#densify < tmpRun.txt | tee -a shape.log
	densifya < tmpRun.txt | tee -a shape.log
	prev=$current

	if [ $i == "128" ]; then
		echo "Got 128"
		echo "y" | cp SHAPEFILES/SIGMA.TXT sigma-$suffix$i
	fi

done
echo SHAPEFILES/$current | shape2maps
$myPath/viewShape.sh


