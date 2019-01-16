# USGS water use data

library(XML)
library(purrr)

usgswatermetaxml <- xmlParse('Q:/USGS_wateruse/Estimated_Use_of_Water_in_the_United_States_County-Level_Data_for_2015.xml')
usgswatermetadata <- xmlToList(usgswatermetaxml)

usgswatermeta_bycolumn <- map_dfr(usgswatermetadata$eainfo$detailed[-1], function(x) data.frame(label = x$attrlabl, def = x$attrdef, source = x$attrdefs))

write.csv(usgswatermeta_bycolumn, 'Q:/USGS_wateruse/usgs_wateruse_columns.csv', row.names = FALSE)

usgswater <- read.csv('Q:/USGS_wateruse/usco2015v2.0.csv', stringsAsFactors = FALSE, skip = 1)

# Extremely rough estimate of household water use by end use
enduse <- read.csv('Q:/USGS_wateruse/enduse.csv')
