# 7 Mar 2016 - Eric E. Palmer
# Gets the pgm and calculates how many black pixels there are.  

mkdir -p eval

echo "#" `date` > eval/evalOut.txt
echo "#",  $0, $vers >> eval/evalOut.txt
echo "#" `date` > eval/evalBlank.txt
echo "#",  $0, $vers >> eval/evalBlank.txt
echo "#" `date` > eval/evalNotBlank.txt
echo "#",  $0, $vers >> eval/evalNotBlank.txt
echo "#" `date` > eval/evalTuck.txt
echo "#",  $0, $vers >> eval/evalTuck.txt
echo > tmp


file=$1
vers=1.0

if [ "$file" == "" ]
then
	echo "usage: $0 <list-o-pictures>"
	exit
else
	list=`grep -v "#" $file | cut -c 1-13`
fi


cnt=0
num=0
for item in $list 
do
	echo $item > tmpRun.txt
	echo "y" >> tmpRun.txt
	echo "0" >> tmpRun.txt
	echo "n" >> tmpRun.txt
	echo "n" >> tmpRun.txt
	/usr/local/bin/Display < tmpRun.txt >> eval/evalOut.txt
	val=`/opt/local/spc/bin/processEval`
	echo $item $val "($cnt)"  | tee -a eval/evalOut.txt
	echo $item $val  >> tmp
	cnt=`echo $cnt + 1 | bc`

	if [ "$val" == "100.000" ]
	then
		echo "################### dump" | tee -a eval/evalOut.txt
		echo "p" >> eval/evalTuck.txt
		echo $item >> eval/evalTuck.txt
		echo "1" >> eval/evalTuck.txt
		num=`echo $num + 1 | bc`
		echo $item >> eval/evalBlank.txt
	else
		echo $item >> eval/evalNotBlank.txt
	fi

done

#sort -n -k 2 tmp >> eval/evalBlank.txt

echo "#### $num images were found to be blank ####"
echo
echo "#### Copy and paste if desired ####"
echo "q" >> eval/evalTuck.txt
echo "lithos < eval/evalTuck.txt"
