
file=$1

list=`cat $file`

for item in $list
do
	landmarkEval.sh $item
done
