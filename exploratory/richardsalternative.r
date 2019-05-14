# Richards with additional param for starting cost (M)

richards_v3 <- function(x, W0, Wu, B, nu, M) {
  # If nu = 1, M = 0, this becomes the exponential curve
  A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
  A + (Wu - A)/((1 + exp(-B*(x - M)))^(1/nu))
}

library(ggplot2)

cs <- RColorBrewer::brewer.pal(6, 'Set2')

ggplot(data.frame(x = c(0, 1000)), aes(x)) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.1, M = 0), color = cs[3], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.02, M = 0), color = cs[4], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.005, nu = 0.1, M = 100), color = cs[5], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.005, nu = 0.02, M = 100), color = cs[6], size = 1) +
  theme_bw() +
  ggtitle('Examples of possible cost curves')

ggplot(data.frame(x = c(0, 1000)), aes(x)) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.1, M = 0), color = cs[3], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.1, M = 50), color = cs[4], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.1, M = 100), color = cs[5], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.1, M = 150), color = cs[6], size = 1) +
  theme_bw() +
  ggtitle('Examples of possible cost curves')

ggplot(data.frame(x = c(0, 1000)), aes(x)) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.01, M = 0), color = cs[1], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.01, M = 50), color = cs[2], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.01, M = 150), color = cs[3], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.05, M = 0), color = cs[4], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.05, M = 50), color = cs[5], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.05, M = 150), color = cs[6], size = 1) +
  theme_bw() +
  ggtitle('Examples of possible cost curves')

ggplot(data.frame(x = c(0, 1000)), aes(x)) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.01, M = 50), color = cs[1], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.015, nu = 0.01, M = 50), color = cs[2], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.02, nu = 0.01, M = 50), color = cs[3], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.05, M = 50), color = cs[4], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.015, nu = 0.05, M = 50), color = cs[5], size = 1) +
  stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.02, nu = 0.05, M = 50), color = cs[6], size = 1) +
  theme_bw() +
  ggtitle('Examples of possible cost curves')

richards_v4 <- function(x, W0, Wu, B, nu, Q) {
  # If nu = 1, Q = 1, this becomes the exponential curve
  A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
  A + (Wu - A)/((1 + Q * exp(-B*(x)))^(1/nu))
}

ggplot(data.frame(x = c(0, 1000)), aes(x)) +
  stat_function(fun = richards_v4, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 1, Q = 1), color = cs[1], size = 1) +
  stat_function(fun = richards_v4, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 1, Q = 1.5), color = cs[2], size = 1) +
  stat_function(fun = richards_v4, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 1, Q = 2), color = cs[3], size = 1) +
  # stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.05, Q = 0), color = cs[4], size = 1) +
  # stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.05, Q = 50), color = cs[5], size = 1) +
  # stat_function(fun = richards_v3, n = 1001, args = list(W0 = 0.4, Wu = 0.1, B = 0.01, nu = 0.05, Q = 150), color = cs[6], size = 1) +
  theme_bw() +
  ggtitle('Examples of possible cost curves')
