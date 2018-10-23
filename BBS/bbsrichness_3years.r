# BBS richness in 2001, 2006, and 2011

# Using by route workspace
load('Q:/BBS/bbsworkspace_byroute.r')
library(dplyr)

idx <- which(bbscov$year %in% c(2001, 2006, 2011))
bbs010611 <- cbind(bbscov[idx,], fixedbbsmat_byroute[idx,]) %>% arrange(year, rteNo)
write.csv(bbs010611, 'Q:/BBS/bbs010611.csv', row.names = FALSE)

# Using "consolidated" matrix which may have more routes
load('Q:/BBS/bbsmatrix_byroute.RData')

idx <- which(bbsmat_allrows[,'year'] %in% c(2001, 2006, 2011))
bbs010611 <- bbsmat_allrows[idx,]
bbs010611 <- bbs010611[order(bbs010611[,'year'], bbs010611[,'rteNo']), ]
write.csv(bbs010611, 'Q:/BBS/bbs010611_allroutes.csv', row.names = FALSE)

# Get locations from routes.csv
rtemeta <- read.csv('Q:/BBS/routes.csv')
rtemeta$rteNo <- with(rtemeta, 1000*statenum + Route)

rtecoords <- left_join(as.data.frame(bbs010611[,c('year', 'rteNo')]),
                       rtemeta[,c('statenum', 'Route', 'rteNo', 'Latitude', 'Longitude', 'BCR')])

write.csv(rtecoords, 'Q:/BBS/bbs010611_routeinfo.csv', row.names = FALSE)
