while read -r lon lat; do
	gdallocationinfo -wgs84 -valonly /nfs/qread-data/raw_data/landuse/NLCD/NLCD_2016_Land_Cover_L48_20190424.img $lon $lat
done < stopcoords.txt > stopnlcd.txt
