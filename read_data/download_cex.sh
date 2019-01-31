# Download CEX
# Interview zip, diary zip, interview survey documentation, diary survey documentation, interview data dictionary, diary data dictionary
cd /nfs/fwe-data/CEX
yrs=( "08" "09" "10" "11" "12" "13" "14" "15" "16" "17" )
for y in "${yrs[@]}"
do
	wget https://www.bls.gov/cex/pumd/data/comma/intrvw${y}.zip
	wget https://www.bls.gov/cex/pumd/data/comma/diary${y}.zip
	unzip intrvw${y}.zip
	unzip diary${y}.zip
done

rm *.zip

for y in "${yrs[@]}"
do
	wget -P ./intrvw${y}/ https://www.bls.gov/cex/20${y}/csxintvw.pdf 
	wget -P ./diary${y}/ https://www.bls.gov/cex/20${y}/csxdiary.pdf
	wget -P ./intrvw${y}/ https://www.bls.gov/cex/20${y}/csxintvwdata.pdf
	wget -P ./diary${y}/ https://www.bls.gov/cex/20${y}/csxdiarydata.pdf
done
