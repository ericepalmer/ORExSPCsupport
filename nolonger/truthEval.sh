# 13 Mar 2016 - Eric E. Palmer
# Truth eval.  Give to paths and it will give the difference between each 
#	directories' sumfiles

vers=1.0

which=SUMFILES
which=NOMINALS

baseNum=16

model=$1
truth=$2

#echo $truth
#echo $model
if [ -z $truth ]
then
echo $truth
	echo "Usage: $0 <path-truth> <path-model> [listofimages]"
	exit
fi

if [ -z $model ]
then
echo $model
	echo "Usage: $0 <path-truth> <path-model> [listofimages]"
	exit
fi

echo "#" `date` > evalOut.txt
echo "#"  $0 $vers >> evalOut.txt

file=$3
if [ -z $file ]
then
	list=`ls $truth/Bennu/$which`
else
	list=`cat $file`
fi

echo "# imageN" "dx" "dy" "dz" " : " "magDelta" " in km" >> evalOut.txt
for item in $list 
do
	if [ ! -f $truth/Bennu/$which/$item ] 
	then
		echo "$item-$truth-missing -1 -1 -1 -1 -1"
		continue
	fi

	if [ ! -f $model/Bennu/$which/$item ] 
	then
		echo "$item-$model-missing -1 -1 -1 -1 -1"
		continue
	fi
	v1=`grep SCOBJ $truth/Bennu/$which/$item | sed s/D/E/g | cut -c 1-63`
	v2=`grep SCOBJ $model/Bennu/$which/$item | sed s/D/E/g | cut -c 1-63`
	#tmp=`wc -l $truth/Bennu/$which/$item | cut -c 1-9`
	#wc1=`echo $tmp - 16 | bc`
	tmp=`wc -l $model/Bennu/$which/$item | cut -c 1-9`
	wc2=`echo $tmp - 16 | bc`

	mag1=`echo $v1 | awk '// { print sqrt ($1*$1 + $2*$2 + $3*$3) }'`
	mag2=`echo $v2 | awk '// { print sqrt ($1*$1 + $2*$2 + $3*$3) }'`
	#echo  $v1 | awk '// { print $1*1.0, $2*1.0, $3*1.0 }'
	#echo  $v2 | awk '// { print $1*1.0, $2*1.0, $3*1.0 }'
	dx=`echo $v1 $v2 | awk '// { print $1 - $4 }'`
	dy=`echo $v1 $v2 | awk '// { print $2 - $5 }'`
	dz=`echo $v1 $v2 | awk '// { print $3 - $6 }'`

	dMag=`echo $dx $dy $dz | awk '// { print sqrt ($1*$1 + $2*$2 + $3*$3) }'`
	echo $item $dx $dy $dz " || " $dMag $wc2 >> evalOut.txt
	
done

