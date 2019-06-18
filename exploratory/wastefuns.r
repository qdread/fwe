w <- function(x, w0, wu, b) {
  2*w0 - wu + (2*wu - 2*w0)/(1 + exp(-b*x))
}

curve(w(x, w0=0.5, wu=0.1, b=0.1), from = 0, to = 100)

w2 <- function(x, w0, wu, b) {
  2*(w0-wu)/(exp(b*x) + 1) + wu
}

curve(w2(x, w0=0.5, wu=0.1, b=0.1), from = 0, to = 100)

waste_rate_by_cost <- function(x, W0, Wu, B, nu) {
  # If nu = 1, this becomes the exponential curve
  A <- (W0 - (2 ^ (-1/nu)) * Wu) / (1 - 2 ^ (-1/nu))
  pmin(W0, A + (Wu - A)/((1 + exp(-B*x))^(1/nu)))
}

curve(waste_rate_by_cost(x, W0 = 0.5, Wu = 0.1, B= 0.1, nu = 1), from=0, to=100)

demand_change_fn <- function(W0, r, p) p * ((1 - W0) / (1 - (1 - r) * W0) - 1) + 1
1-demand_change_fn(0.5, 0.8, 1e-3)

curve(w2(x, w0=0.5, wu=0.1, b=2e-8), from = 0, to = 2e8)
