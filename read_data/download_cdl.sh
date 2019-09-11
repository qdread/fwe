# Download USDA cropland data layer for some years.
# Only 2018 data layer downloaded 05 Sep 2019

cd /nfs/fwe-data/landuse/USDAcropland/CDL

for ((y=2009; y <= 2018 ; y++))
do
   wget ftp://ftp.nass.usda.gov/download/res/${y}_30m_cdls.zip &
 done
