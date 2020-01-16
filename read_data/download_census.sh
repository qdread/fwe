# Script to download business and economic data from US Census 

cd /nfs/qread-data/raw_data/Census

# Statistics of US Businesses
mkdir SUSB

for ((y=2012; y <= 2016 ; y++))
do
   wget -P ./SUSB/ https://www2.census.gov/econ/susb/data/${y}/us_state_6digitnaics_${y}.txt
   wget -P ./SUSB/ https://www2.census.gov/econ/susb/data/${y}/us_state_naics_detailedsizes_${y}.txt
   wget -P ./SUSB/ https://www2.census.gov/econ/susb/data/${y}/msa_3digitnaics_${y}.txt
done

for ((y=2014; y <= 2015 ; y++))
do
   wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/datasets/${y}/us_state_6digitnaics_${y}.txt
   wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/datasets/${y}/us_state_naics_detailedsizes_${y}.txt
   wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/datasets/${y}/msa_3digitnaics_${y}.txt
done

wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/tables/2016/us_state_6digitnaics_2016.txt
wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/tables/2016/us_state_naics_detailedsizes_2016.txt

# Record layouts for SUSB
wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/technical-documentation/record_layout_us_and_state.txt
wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/technical-documentation/record_layout_msa.txt
wget -P ./SUSB/ https://www2.census.gov/programs-surveys/susb/technical-documentation/record_layout_us_and_state_emplchange.txt # layout for employment change dataset (might not be used)

# Community Business Patterns
mkdir CBP

filenames=( "co" "msa" "pr_ia_co" "pr_ia_st" "st" "us" )
for y in "${filenames[@]}"
do
	wget -P ./CBP/ https://www2.census.gov/econ2012/CBP_CSV/cbp12${y}.zip
done

wget -P ./CBP/ https://www2.census.gov/econ2012/CBP_CSV/zbp12detail.zip 
wget -P ./CBP/ https://www2.census.gov/econ2012/CBP_CSV/zbp12totals.zip 

for i in `ls | grep zip`
do
	unzip $i
done

rm *.zip

# Get the other CBP years besides 2012.
baseurl="https://www2.census.gov/programs-surveys/cbp/datasets"
filenames=( "co" "msa" "pr_ia_co" "pr_ia_st" "st" "us" )

for ((y=13; y <= 16 ; y++))
do
	for ((i=0; i <= 5; i++))
	do
		wget -P ./CBP/ "${baseurl}/20${y}/cbp${y}${filenames[i]}.zip"
	done
done

# Download the record layouts for CBP
# One for 2015-2016 and one for 2012 probably sufficient.
wget https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/county_layout_2015.txt
wget https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/metro_area_layout_2015.txt
wget https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/state_pr_layout_2015.txt
wget https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/state_layout_2015.txt
wget https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/us_layout_2015.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/zip_detail_layout.txt
wget https://www2.census.gov/programs-surveys/rhfs/cbp/technical%20documentation/2015_record_layouts/zip_totals_layout_2015.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/combined-record-layout/record_layout.txt

# 2007-2013 record layouts
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/county_layout.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/metro_area_layout.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/state_layout.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/state_x_lfo_layout.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/us_lfo_layout.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/noise-layout/zip_totals_layout10.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/records-layouts/combined-record-layout/record_layout_pre2015.txt

# Overall reference documents
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/naics-descriptions/naics2012.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/state-county-geography-reference/georef12.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/metro-area-geography-reference/msa_county_reference12.txt
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/combined-reference/reference_file_pre2015.xlsx
wget https://www2.census.gov/programs-surveys/cbp/technical-documentation/reference/combined-reference/reference_file.xlsx