# Get price per serving for different foods, using the crosswalk tables

library(tidyverse)

fp_qfahpd <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'ERS/QFAHPD/tidy_data')
qfahpd2 <- read.csv(file.path(fp_qfahpd, 'qfahpd2.csv'), stringsAsFactors = FALSE)
fp_usda <- file.path(ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data'), 'USDAnutrients')
