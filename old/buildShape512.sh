# 3 Nov 2014
# 	This builds the shape using standard techniques
#	Requires the starting shape model is 512

if [ "$1" == "" ]
then
   echo "Usage: $0 <suffix>"
   exit
fi

myPath="/opt/local/spc/bin/"
suffix=$1
prev=dumb-$suffix

vers="1.1"

date > shape.log

Q=`head -1 SHAPEFILES/SHAPE.TXT`
echo "Starting Q $Q"  >> shape.log
echo "Version $vers"  >> shape.log

echo "SHAPEFILES/SHAPE.TXT" > tmpRun.txt
echo "SHAPEFILES/$prev" >> tmpRun.txt
echo "16" >> tmpRun.txt
echo "y" >> tmpRun.txt
cat tmpRun.txt >> shape.log

dumber < tmpRun.txt  | tee -a shape.log		# normally used

list="64 128 256 512"

for i in $list 
do
	current=shape-$suffix-$i
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
	cat tmpRun.txt >> shape.log
	densify < tmpRun.txt | tee -a shape.log
	#densifya < tmpRun.txt | tee -a shape.log
	#bin/bin/densify_Hav < tmpRun.txt | tee -a shape.log
	prev=$current

	if [ $i == "128" ]; then
		densify < tmpRun.txt | tee -a shape.log
		echo "Got 128"
		echo "y" | cp SHAPEFILES/SIGMA.TXT sigma-$suffix-$i
	fi

done
echo SHAPEFILES/$current | shape2maps
$myPath/viewShape.sh


