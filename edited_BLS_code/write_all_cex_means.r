# Output all CEX tables as CSVs for each year.

# 2009,2010,2011,2012 throw errors -- fixed by hardcoding line widths!
for (year in c(2008:2017)) {
  source('~/fwe/edited_BLS_code/Integrated Mean and SE-QDRedit.R') # output the means by income class
  source('~/fwe/edited_BLS_code/integrated_mean_se_byregion.R') # Output the means by region
}
