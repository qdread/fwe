# Investigate shape of logistic curve for our optimization analysis

# Generalized logistic or Richards curve

richards <- function(x, A, K, B, nu, Q, C) A + (K - A)/((C + Q*exp(-B*x))^(1/nu))

curve(richards(x, A = 0.6, K = 0.2, B = 0.5, nu = 0.5, Q = 20, C = 1), from = 0, to = 10)

# My waste function. Q and C are set to 1. The A parameter is expressed as a function of W0, Wu, and nu
# Wu is the carrying capacity K
alternative_richards <- function(x, W0, Wu, nu, B) {
  A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
  A + (Wu - A)/((1 + exp(-B*x))^(1/nu))
}

curve(alternative_richards(x, W0 = 0.6, Wu = 0.2, B = 0.6, nu = 0.001), from = 0, to = 30)

# No flat part at the beginning, just diminishing returns

negexp <- function(x, W0, Wu, B) (W0 - Wu) * exp(-B * x) + Wu
curve(negexp(x, W0 = 0.6, Wu = 0.2, B = 0.1), from = 0, to = 30)

# Plot alternative functions representing
# (1) diminishing returns, no startup costs
#   (a) shallow curve
#   (b) steep curve
# (2) diminishing returns, with startup costs
#   (a) shallow curve, high startup costs
#   (b) shallow curve, low startup costs
#   (c) steep curve, high startup costs
#   (d) steep curve, high startup costs

library(ggplot2)

cs <- RColorBrewer::brewer.pal(6, 'Set2')

ggplot(data.frame(x = c(0, 1000)), aes(x)) +
  stat_function(fun = negexp, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.005), color = cs[1], size = 1) +
  stat_function(fun = negexp, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.001), color = cs[2], size = 1) +
  stat_function(fun = alternative_richards, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.1), color = cs[3], size = 1) +
  stat_function(fun = alternative_richards, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.02), color = cs[4], size = 1) +
  stat_function(fun = alternative_richards, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.005, nu = 0.1), color = cs[5], size = 1) +
  stat_function(fun = alternative_richards, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.005, nu = 0.02), color = cs[6], size = 1) +
  theme_bw() +
  ggtitle('Examples of possible cost curves')
  

# Manually create data and plot cost curves so we can have a legend.
library(tidyverse)

# Cost curve function that includes both types
cost_curve <- function(x, W0, Wu, B, nu = NA) {
  # If null is not provided return the exponential curve
  # Else return the Richards logistic curve
  if (is.na(nu)) {
    (W0 - Wu) * exp(-B * x) + Wu
  } else {
    A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
    A + (Wu - A)/((1 + exp(-B*x))^(1/nu))
  }
}

# We can include no initial flat portion (no startup costs) by setting nu to 1
params <- data.frame(W0 = 0.4,
                     Wu = 0.1,
                     B = c(0.01, 0.003, 0.01, 0.003, 0.01, 0.003),
                     nu = c(1, 1, 0.1, 0.1, 0.02, 0.02))

vals <- pmap(params, cost_curve, x = 0:1200)
params$description <- c('Steep curve, no startup costs',
                        'Shallow curve, no startup costs',
                        'Steep curve, low startup costs',
                        'Shallow curve, low startup costs',
                        'Steep curve, high startup costs',
                        'Shallow curve, high startup costs')

plotdat <- imap_dfr(vals, ~ data.frame(params[.y,], x = 0:1200, y = .x))

ggplot(plotdat, aes(x, y, color = description)) +
  geom_path(size = 0.75) +
  geom_hline(yintercept = c(0.1, 0.4), linetype = 'dotted', size = 1) +
  scale_color_brewer(type = 'qual', palette = 'Set2') +
  scale_x_continuous(name = 'Amount spent on waste reduction', limits=c(0,1000), expand=c(0,0)) +
  scale_y_continuous(name = 'Waste rate', limits=c(0,0.41), expand=c(0,0)) +
  theme_bw() +
  ggtitle('Possible cost curves for waste reduction',
          'Assumes 40% baseline waste rate, 10% unavoidable waste rate')
ggsave('Q:/figures/costcurve_schematic_alternatives.png', height = 5, width = 6, dpi = 300)  
