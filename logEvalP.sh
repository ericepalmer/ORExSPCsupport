# 14 Jan 2016 - Eric E. Palmer
# Parses the OOT files from make_scriptP/lithosP
#		It shows # of landmarks without overlap and/or correlated images

file=$1

date > logResults.txt

if [ "$file" == "" ]
then
	list=`ls *OOT`
	total=`ls *INN | wc -l`
else
	list=`cat $file`
	total=`echo $list | wc -l`
fi


cnt=0

for i in $list 
do
	awk -v str=$str -v id=$i -f support/logEvalP.awk $i | tee -a logResults.txt
	cnt=`echo $cnt + 1 | bc`
done

echo Evaluated $cnt 
echo Total $total
