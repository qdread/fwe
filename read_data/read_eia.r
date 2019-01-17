# Read EIA data
# QDR FWE 16 Jan 2019

library(XLConnect)

eia_list <- lapply(1:10, function(i) {
  
})

# HC1: fuels used and end uses
hc1.1 <- readWorksheetFromFile('/nfs/qread-data/EIA/hc1.1.xlsx', sheet = 1)
hc2.1 <- readWorksheetFromFile('/nfs/qread-data/EIA/hc2.1.xlsx', sheet = 1)
hc1.2 <- readWorksheetFromFile('/nfs/qread-data/EIA/hc1.2.xlsx', sheet = 1)

num_rows <- apply(hc1.1, 1, function(x) any(!is.na(as.numeric(x))))
hc1.1_numeric <- hc1.1[num_rows,]
hc1.1[!num_rows, 1]
