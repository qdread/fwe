# Download some USDA data 
# 24 Oct 2019

# Census Data Query Tool 2012 and 2017
cd /nfs/fwe-data/USDA
wget https://www.nass.usda.gov/Publications/AgCensus/2012/Online_Resources/Census_Data_Query_Tool/2012_cdqt_data.txt.gz
gunzip 2012_cdqt_data.txt.gz

wget https://www.nass.usda.gov/Publications/AgCensus/2017/Online_Resources/Census_Data_Query_Tool/2017_cdqt_data.txt.gz
gunzip 2017_cdqt_data.txt.gz
