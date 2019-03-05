# My own script for turning the stub text file into a hierarchically organized data frame
year <- 2017
data_dir <- paste0("/nfs/fwe-data/CEX/data", substr(year,3,4))

stub_raw <- read.csv(file.path(data_dir, 'csxistub_cleaned.csv'), stringsAsFactors = FALSE)

nl <- length(unique(stub_raw$level))

# For each row find all the parent groups in a higher (lower numbered) level than it.

grps <- matrix(0, nrow = nrow(stub_raw), ncol = nl)
uccs <- matrix(as.character(NA), nrow = nrow(stub_raw), ncol = nl)

for (i in 1:nrow(stub_raw)) {
  for (j in 1:stub_raw$level[i]) {
    grps[i, j] <- max(which(stub_raw$level[1:i] == j))
    uccs[i, j] <- stub_raw$ucc[grps[i, j]]
  }
}

