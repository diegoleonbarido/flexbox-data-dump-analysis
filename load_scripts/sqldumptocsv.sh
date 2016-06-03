#/bin/bash
export PGPASSWORD='flexbox'
file=$1
filename="${file%.*}"

echo "truncate inside_temps,ambient,switch,house_power,fridge_power;" | psql -U flexbox flexbox_db
zcat $1 | psql -U flexbox flexbox_db
echo "select * from ambient;" | psql -U flexbox flexbox_db | tr "|" "," | awk '!((NR==2))' > ${filename}_ambient_table.csv
echo "select * from inside_temps;" | psql -U flexbox flexbox_db | tr "|" "," | awk '!((NR==2))' > ${filename}_inside_temps_table.csv
echo "select * from switch;" | psql -U flexbox flexbox_db | tr "|" ","  | awk '!((NR==2))' > ${filename}_switch_table.csv
echo "select * from fridge_power;" | psql -U flexbox flexbox_db | tr "|" "," | awk '!((NR==2))' > ${filename}_fridge_power_table.csv
echo "select * from house_power;" | psql -U flexbox flexbox_db | tr "|" "," | awk '!((NR==2))' > ${filename}_house_power_table.csv
