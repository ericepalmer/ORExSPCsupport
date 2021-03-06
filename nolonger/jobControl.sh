# Eric E. Palmer - 17 Jan 2015
# Script to send the kill CONT/STOP commands to LITHOS
# Needed for pausing work when you want to run residuals or geometry during an iterate
# Also let's you kill an iterate easily

echo 
select answer in "Pause" "Resume" "Kill" "Graceful"; do
	case $answer in
		Pause ) 
			search=LITHOS
			#list=`ps ax | grep LITHOS | grep -v grep | cut -f 1 -d " "`
			list=`ps ax | grep -v awk | awk '/LITHOS/ { print $1} '`
			flag=STOP; 
			break;;
		Resume ) 
			search=LITHOS
			list=`ps ax | grep -v awk | awk '/LITHOS/ { print $1} '`
			flag=CONT; 
			break;;
		Kill ) 
			search=LITHOS
			list=`ps ax | grep -v awk | awk '/LITHOS/ { print $1} '`
			flag=9; 
			break;;
		Graceful ) 
			search=run.sh
			list=`ps ax | grep -v awk | awk '/run.sh/ { print $1} '`
			flag=9; 
			break;;
	esac

done

echo "Doing kill -$flag on $search"


for i in $list 
do
	cmnd="kill -$flag $i"
	echo $cmnd
	`$cmnd`
done
 ls TESTFILES
ps ax | grep monitor
