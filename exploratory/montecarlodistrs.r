# Mean and variance of beta distribution
mb <- function(a,b) a/(a+b)
vb <- function(a,b) (a*b)/((a+b)^2 * (a+b+1))

mb(30,70)
vb(30,70)
mb(40,60)
vb(40,60)

curve(vb(x, 100-x), from=0, to=100)

curve(dbeta(x, 40,60))
curve(dbeta(x, 30,70), add = T, col='red')
curve(dbeta(x, 20,80), add = T, col='blue')
curve(dbeta(x, 55,45), add = T, col='green')
curve(dbeta(x, 50,50), add = T, col='orange')
curve(dbeta(x, 80,20), add = T, col='purple')

library(truncdist)

curve(dtrunc(x, 'beta', a=0, b=0.5, shape1=50, shape2=50))

library(triangle)

curve(dtriangle(x, a = 10, b = 20, c = 15), from = 0, to = 25)

library(MCMCpack)

rdirichlet(1, alpha = c(700, 200, 100)) # Similar to the beta distribution but the numbers are related.

# Sensitivity analysis distributions
# w0 ~ beta(100*w0, 100*(1-w0))
# p ~ beta(100*p,100*(1-p))
# w1 ~ truncated beta (100*w1, 100*(1-w1)), max at whatever the current value of w0 is.
# wu ~ truncated beta(100-wu, 100*(1-wu)), max at whatever the current value of w1 is.
# c1 ~ triangular(0.5*c1, 1.5*c1, c1)


# Plot beta distributions -------------------------------------------------

library(ggplot2)
cols <- RColorBrewer::brewer.pal(8, 'Set3')[-2]
ps <- c(10,20,40,50,60,80,90)

distplot <- ggplot(data.frame(x=c(0,1)), aes(x)) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(0,15), expand=c(0,0)) +
  theme_bw()

for (i in 1:length(ps)) distplot <- distplot + stat_function(fun = dbeta, args=list(shape1=ps[i], shape2=100-ps[i]), color = cols[i], size = 1)
distplot
