# Download USDA cropland data layer for some years.
# Only 2018 data layer downloaded 05 Sep 2019
# 2014 and up to public-data 13 Sep 2019

cd /nfs/qread-data/raw_data/landuse/USDAcropland/CDL

for ((y=2014; y <= 2018 ; y++))
do
   wget ftp://ftp.nass.usda.gov/download/res/${y}_30m_cdls.zip &
done

for ((y=2015; y <= 2018 ; y++))
do
   unzip ${y}_30m_cdls.zip &
done
wait

# Need to get 2009 too 
wget ftp://ftp.nass.usda.gov/download/res/2009_30m_cdls.zip
unzip 2009_30m_cdls.zip
