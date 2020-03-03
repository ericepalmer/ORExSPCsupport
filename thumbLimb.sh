# 13 Dec 2015 - Eric E. Palmer
# Just takes a list of images, converts them into jpg and 
#		puts them into ~/send/

file=$1

if [ "$file" == "" ]
then
	echo "No file"
	exit 
fi

arg=$2


list=`cat $file`

for i in $list
do

	c=`echo $i | cut -c 1`
	if [ "$c" == "!" ]
	then
		continue
	fi
	if [ "$i" == "END" ]
	then
		exit
	fi

# This is needed to show the limb points
	echo "$i" > tmp
	echo "y" >> tmp
	echo "0" >> tmp
	echo "n" >> tmp
	echo "y" >> tmp
	echo "n" >> tmp
	echo "n" >> tmp
	echo "n" >> tmp

	/usr/local/bin/Display < tmp


	echo $i
	if [ "$arg" == "-pgm" ]
	then
		cp TEMPFILE.pgm ~/send/$i.pgm
	
	else
		convert TEMPFILE.pgm ~/send/thumb-$i.jpg
	fi


done

