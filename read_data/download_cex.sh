# Download CEX (Bureau of Labor Statistics Consumer Expenditure Survey) data
# Interview zip, diary zip, interview survey documentation, diary survey documentation, interview data dictionary, diary data dictionary, hierarchical groupings
cd /nfs/fwe-data/CEX
yrs=( "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" )

# Download the main data files
for y in "${yrs[@]}"
do
	wget https://www.bls.gov/cex/pumd/data/comma/intrvw${y}.zip
	wget https://www.bls.gov/cex/pumd/data/comma/diary${y}.zip
	unzip intrvw${y}.zip
	unzip diary${y}.zip
done

rm *.zip

# Download the documentation PDFs (probably not necessary to look at a different one each year)
for y in "${yrs[@]}"
do
	wget -P ./intrvw${y}/ https://www.bls.gov/cex/20${y}/csxintvw.pdf 
	wget -P ./diary${y}/ https://www.bls.gov/cex/20${y}/csxdiary.pdf
	wget -P ./intrvw${y}/ https://www.bls.gov/cex/20${y}/csxintvwdata.pdf
	wget -P ./diary${y}/ https://www.bls.gov/cex/20${y}/csxdiarydata.pdf
done

# Download the 3 hierarchical groupings files for each year (IntStub, IStub, DStub)
for y in "${yrs[@]}"
do
	mkdir ./stub${y}
	wget -P ./stub${y}/ https://www.bls.gov/cex/pumd/20${y}/csxintstub.txt
	wget -P ./stub${y}/https://www.bls.gov/cex/pumd/20${y}/csxistub.txt
	wget -P ./stub${y}/ https://www.bls.gov/cex/pumd/20${y}/csxdstub.txt
done
