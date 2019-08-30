# test
# We have one species with affinity 1 for old habitat, affinity 0.5 for new habitat
# original S = 1 in this case (or P orig  = 1)
# original A = 1
# z = 0.25

z <- 0.25
Porig <- 1
Aorig <- 1

# function to get Pnew
Pnew <- function(h, Alost, Porig, Aorig, z){
  Anew <- Aorig - Alost
  Porig * ( 1 - ((Anew + h * Alost) / Aorig) ^ z )
}

Pnew(h=0, Alost = 0.5, Porig = 1, Aorig = 1, z = 0.25) # If 0.5 of the area is lost, and z = 0.25, we get 84% chance of survival of the species if h=0, but 93% chance of survival if h = 0.5

# Now vectorize the function for a number of different species with different affinities.
# Also look at affinity of cropland and of pastureland
h_crop <- c(0.5, 0.3, 0.9, 1, 1.2)
h_pasture <- c(0.9, 0.4, 0.9, 1, 1)
h_all <- cbind(h_crop, h_pasture)
Alost <- c(0.2, 0.1) # Replace 20% of original land with crops, and 10% with pasture.

Slost <- function(h, Alost, Aorig, z) {
  Anew <- Aorig - sum(Alost)
  Sorig <- nrow(h)
  Sorig * ( 1 - ((Anew + mean(h %*% as.matrix(Alost))) / Aorig) ^ z )
}

Slost(h = h_all, Alost = Alost, Aorig = 1, z = 0.25) 

h_crop <- runif(1000, min=0.4, max=1)
h_pasture <- runif(1000, min=0.6,max=1.1)

Slost(h = cbind(h_crop, h_pasture), Alost = c(0.05, 0.15), Aorig = 1, z = 0.25) # For this set of parameters, ~9.6 species of the 1000 are lost when 20% of the habitat is replaced.
Slost(h = matrix(0, nrow=1000, ncol=2), Alost = c(0.05, 0.15), Aorig = 1, z = 0.25) # If the replaced habitat is completely inhospitable, 54.3 species of the 1000 are lost.
