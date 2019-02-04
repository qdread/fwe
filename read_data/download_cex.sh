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
	wget -P ./stub${y}/ https://www.bls.gov/cex/pumd/20${y}/csxistub.txt
	wget -P ./stub${y}/ https://www.bls.gov/cex/pumd/20${y}/csxdstub.txt
done

# Consolidate all data for each year into a single folder instead of having a separate one for the interview and for the diary.
# This is needed for the sample R scripts to run with minimal modification. (added 04 Feb 2019)

cd /nfs/fwe-data/CEX
yrs=( "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" )

for y in "${yrs[@]}"
do
	mkdir ./data${y}
	mv ./diary${y}/* ./data${y}
	mv ./expn${y}/* ./data${y}
	mv ./intrvw${y}/* ./data${y}
	mv ./para${y}/* ./data${y}
	mv ./stub${y}/* ./data${y}
done

mv ./data08/diary08/* ./data08
rm -rf ./data08/diary08/
mv ./data09/diary09/* ./data09
rm -rf ./data09/diary09/
mv ./data10/diary10/* ./data10
rm -rf /data10/diary10/
mv ./data11/diary11/* ./data11
rm -rf ./data11/diary11/
mv ./data12/diary12/* ./data12
rm -rf ./data12/diary12/

rm -rf ./stub*
rm -rf ./para*
rm -rf ./diary*
rm -rf ./intrvw*
rm -rf ./expn*
