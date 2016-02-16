#/bin/bash
for f in *.gz
do 
    ./sqldumptocsv.sh $f
done