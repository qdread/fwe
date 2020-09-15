# Write LAFA as an RData file to the temporary folder for FWI data.

source('read_data/read_lafa.r')
save(dairy, fat, fruit, grain, meat, sugar, veg, file = '/nfs/qread-data/temp/data_for_fwi/lafa_processed.RData')
