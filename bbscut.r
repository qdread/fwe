# Look at the number of bbs plots within certain distance of each other.

library(sp)
bbspts <- read.csv('/nfs/qread-data/BBS/bbs_route_midpoints.csv')
bigdistmat <- spDists(as.matrix(bbspts[,c('lon','lat')]), longlat = TRUE)

diag(bigdistmat) <- NA

bigcut <- apply(bigdistmat, 1, cut, breaks = c(5, 10, 20, 50, 100, 200, 500, 1000, Inf))
tablebyrow <- function(x) table(rep(rownames(x),ncol(x)),c(x))
dimnames(bigcut)[[1]] <- 1:nrow(bigcut)

bigtable <- tablebyrow(bigcut)

apply(bigtable, 2, table)

xless10 <- apply(bigdistmat, 1, function(x) sum(x <= 10, na.rm = TRUE))
xless20 <- apply(bigdistmat, 1, function(x) sum(x <= 20, na.rm = TRUE))
xless50 <- apply(bigdistmat, 1, function(x) sum(x <= 50, na.rm = TRUE))
xless100 <- apply(bigdistmat, 1, function(x) sum(x <= 100, na.rm = TRUE))


