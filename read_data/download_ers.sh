# Script to download data from ERS for food prices.
# 2 sources: 
# 1 Quarterly food-at-home price database (last updated 2012) for price by weight for different categories
# 2 Food expenditure series for total amount spent on food at home and food away from home

# Downloaded by QDR on 13 March 2019

cd /nfs/qread-data/raw_data/ERS

#---------------------------------------------------------------
# QFAHPD
mkdir QFAHPD
filenames=( "fatsandpreparedfoods" "fruitsandvegetables" "grainsanddairy" "meatsandeggs" )
baseurl="https://www.ers.usda.gov/webdocs/DataFiles/52760/"

for y in "${filenames[@]}"
do
	wget -P ./QFAHPD/ ${baseurl}${y}_q1.xls
done

wget -P ./QFAHPD/ ${baseurl}qfahpd1codebook.xls

for y in "${filenames[@]}"
do
	wget -P ./QFAHPD/ ${baseurl}qfahpd2${y}.xls
done

wget -P ./QFAHPD/ ${baseurl}qfahpd2codebook.xls

#-----------------------------------------------------------------
# Food expenditure series
mkdir foodexpenditure
filenames=( "nominal_expenditures" "nominal_expenditures_no_taxes_tips" "constant_dollar_expenditures" "constant_dollar_expenditures_no_taxes_tips" "normalized_food_expenditures" "food_expenditures_source_funds" "monthly_sales" "archived_nominal_expenditures_1" "archived_expenditures_purchaser_2" "archived_normalized_expenditures_3" "archived_monthly_4" )
baseurl="https://www.ers.usda.gov/webdocs/DataFiles/50606/"

for y in "${filenames[@]}"
do
	wget -P ./foodexpenditure/ ${baseurl}${y}.xlsx
done

#-----------------------------------------------------------------
# Added 27 March 2019: download FICRCD from food surveys research group (joint effort of ARS and ERS)
cd /nfs/qread-data/raw_data/FICRCD
filenames=( "2007_2008" "2005_2006" "2003_2004" "2001_2002" "1999_2000" "1994_1998" )
baseurl="https://www.ars.usda.gov/ARSUserFiles/80400530/apps/FICRCD_"

for y in "${filenames[@]}"
do
	wget ${baseurl}${y}_sas.exe
done

for y in "${filenames[@]}"
do
	chmod +x FICRCD_${y}_sas.exe
	./FICRCD_${y}_sas.exe
done

#-----------------------------------------------------------------
# Added 27 March 2019: download the *not* loss-adjusted food availability data system (FADS) from ERS
# These are the 29 Oct 2018 data.
cd /nfs/qread-data/raw_data/ERS/FADS
filenames=( "ctcsp" "dyfluid" "dymfg" "eggs" "fats" "mtfish" "frtot" "fruitcan" "fruitdr" "fruitfr" "fruitfz" "fruitju" "fruitveg" "grains" "nuts" "pop" "mtpoulsu" "mtredsu" "mtpcc" "sweets" "vegtot" "vegcan" "vegfr" "vegfrz" "legumes" "potatoes" )
baseurl="https://www.ers.usda.gov/webdocs/DataFiles/50472/"

for y in "${filenames[@]}"
do
	wget ${baseurl}${y}.xls
done

# Two of the files are xlsx for some reason.
wget ${baseurl}vegcan.xlsx
wget ${baseurl}vegfr.xlsx

# Convert to XLSX (run powershell as administrator on windows)
$filenames = @("ctcsp", "dyfluid", "dymfg", "eggs", "fats", "mtfish", "frtot", "fruitcan", "fruitdr", "fruitfr", "fruitfz", "fruitju", "fruitveg", "grains", "nuts", "pop", "mtpoulsu", "mtredsu", "mtpcc", "sweets", "vegtot", "vegfrz", "legumes", "potatoes")
foreach ($file in $filenames) {
	& "C:\Program Files (x86)\Microsoft Office\Office16\excelcnv.exe" -oice "Z:\ERS\FADS\${file}.xls" "Z:\ERS\FADS\${file}.xlsx"
}
