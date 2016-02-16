#/bin/bash
export PGPASSWORD='flexbox'
file=$1
filename="${file%.*}"

echo "truncate inside_temps,ambient,switch,house_power,fridge_power;" | psql -U flexbox flexbox_db
zcat $1 | psql -U flexbox flexbox_db
mkdir ${filename}
echo "select * from ambient;" | psql -U flexbox flexbox_db | tr "|" "," > ${filename}/${filename}_ambient_table.csv
echo "select * from inside_temps;" | psql -U flexbox flexbox_db | tr "|" "," > ${filename}/${filename}_inside_temps_table.csv
echo "select * from switch;" | psql -U flexbox flexbox_db | tr "|" ","  > ${filename}/${filename}_switch_table.csv
echo "select * from fridge_power;" | psql -U flexbox flexbox_db | tr "|" ","  > ${filename}/${filename}_fridge_power_table.csv
echo "select * from house_power;" | psql -U flexbox flexbox_db | tr "|" "," > ${filename}/${filename}_house_power_table.csv