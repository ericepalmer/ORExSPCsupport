# Builds lots of useful tools

truth=TTAG15
model=MTAG15

#truth=MTAG15
#model=MT1-02

trans=50
script=MTAG15.in

#binPath="/usr/local/localbin/"
binPath="/SPC_Test/localbin/"
#dataPath="/Library/WebServer/Documents/data"
dataPath="/Library/Server/Web/Defaults/Sites/Data/data"

#bigmap < $script

$binPath/bigMapRef < $script
echo $model |  showmap
convert $model.pgm $dataPath/bigmap.jpg

convert SIGMAS.pgm $dataPath/sigmas.jpg

echo $model > tmp
echo "-1 -1 1 45" >> tmp
view_map_rgb < tmp
convert view.ppm $dataPath/viewMap.jpg

$binPath/metaBigmap $model USED_MAPS.TXT
echo y | /bin/cp position.json $dataPath/
sort -u resolutions.txt > res.txt

lowest=`tail -1 res.txt`
echo $model > tmp
echo $lowest $lowest >> tmp
map_coverage < tmp
convert coverage_m.pgm $dataPath/coverage3.jpg

lowest=`tail -2 res.txt | head -1`
echo $model > tmp
echo $lowest $lowest >> tmp
map_coverage < tmp
convert coverage_m.pgm $dataPath/coverage2.jpg

lowest=`tail -3 res.txt | head -1`
echo $model > tmp
echo $lowest $lowest >> tmp
map_coverage < tmp
convert coverage_m.pgm $dataPath/coverage1.jpg

#make_lmrklistX
#grep -f USED_MAPS.TXT LMRKLISTX.txt > t_locations
#cp t_locations $dataPath/locations.txt
#echo 5 .005 .00 5 | residuals

echo $model | $binPath/flatMapVec 
mv $model.TXT model.txt
echo $truth | $binPath/flatMapVec 
mv $truth.TXT truth.txt
echo "trans=$trans" > config.gpi

gnuplot $binPath/bigmapPlot.gpi
convert out.png $dataPath/trans.jpg




