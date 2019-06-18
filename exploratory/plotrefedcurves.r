# Use ReFED to create realistic cost curves for each stage.
# QDR / FWE / 17 June 2019

# to use:
# farm loss: product grading standards
# processing loss: manufacturing line optimization
# retail loss: combination of cold chain and inventory management
# foodservice loss: waste tracking and analytics (eg Leanpath)
# institutional loss: waste tracking and analytics
# household loss: consumer education campaigns

# Refed includes: total waste, addressable waste, diversion potential, and cost to reach diversion potential
# Thus, we have 2 points on the curve, a lower asymptote, and an upper asymptote
# This can be used to parameterize the cost curve!

fpcrosswalk <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
refed <- read.csv('Q:/crossreference_tables/refed_testvalues.csv', stringsAsFactors = FALSE)

refed$Wu <- with(refed, 1 - addressable/net)
refed$W1 <- with(refed, 1 - diversion.potential/net)

refed$b <- with(refed, log(2 * (1-Wu)/(W1-Wu))/cost)

w <- function(x, W0, Wu, b, ...) {
  2*(w0-wu)/(exp(b*x) + 1) + wu
}

# Plot assuming W0=1 and other numbers as a function of W0.

library(tidyverse)
Cmax <- 500

refedplotdat <- refed %>%
  group_by(stage) %>%
  do(data.frame(C = seq(0, Cmax, by = 1),
                W = w(x = seq(0, Cmax, by = 1), W0 = 1, Wu = .$Wu, b = .$b)))

refedplotdat <- pmap_dfr(refed, w, x = seq(0, Cmax, by=1), W0 = 1)

ggplot(refedplotdat, aes(x=C, y=W, color=stage)) + geom_line() 
