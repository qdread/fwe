# Script to download data from Bureau of Labor Statistics
# Specifically QCEW (Quarterly census of employment and wages)
# Annual averages of number of employees by NAICS detailed industry
# These are also available by county but probably not necessary for now

cd /nfs/qread-data/raw_data/Census/QCEW

baseurl="https://data.bls.gov/cew/data/files/"
# https://data.bls.gov/cew/data/files/2017/csv/2017_annual_by_industry.zip

# These are separate CSVs for every industry
for ((y=2012; y <= 2017 ; y++))
do
	wget "${baseurl}/${y}/csv/${y}_annual_by_industry.zip"
done

for file in `ls *.zip`
do
	unzip $file
done

# Easier to do by single files even if they are big.
for ((y=2012; y <= 2017 ; y++))
do
	wget "${baseurl}/${y}/csv/${y}_annual_singlefile.zip"
done

for file in `ls *.zip`
do
	unzip $file
done

# Added 24 Oct 2019: get by size of firm as well
# https://data.bls.gov/cew/data/files/2012/csv/2012_q1_by_size.zip

for y in $(seq 2012 2019)
do
	wget "${baseurl}/${y}/csv/${y}_q1_by_size.zip"
done

for file in `ls *.zip`
do
	unzip $file
done