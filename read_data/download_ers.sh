# Script to download data from ERS for food prices.
# 2 sources: 
# 1 Quarterly food-at-home price database (last updated 2012) for price by weight for different categories
# 2 Food expenditure series for total amount spent on food at home and food away from home

# Downloaded by QDR on 13 March 2019

cd /nfs/fwe-data/ERS

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
