# read FICRCD to see if columns match

fp <- ifelse(dir.exists('Z:/'), 'Z:', '/nfs/fwe-data')

fic08 <- read.csv(file.path(fp, 'food_consumption/FICRCD/ficrcd_2007_2008.csv'), stringsAsFactors = FALSE)

# do individual vegetables add up to the total vegetables column
with(fic08, table(TotalBrassica >= BroccoliAndCauliflower))
with(fic08, table(TotalBrassica > BroccoliAndCauliflower))

veg_columns <- 54:69
apply(fic08[,veg_columns],1,sum) # some over 100

# check if the total dairy equals the sum of the other dairy columns
# Yes it all matches up.
fic08 %>%
  mutate(dairy_sum = FluidMilkWhl + FluidMilk2pct + FluidMilk1pct + FluidMilkSkim + Butter + Cheese + Yogurt + OtherDairy,
         diff = TotalDairy - dairy_sum) %>%
  pull(diff) %>%
  table

# What about the total meat matching the sum of all the other meat columns
# No it does not since there is no other meat column
fic08 %>%
  mutate(meat_sum = Beef + Pork + Chicken + Turkey + FinAndShellfish,
         diff = TotalMeatPoultryFish - meat_sum) %>%
  pull(diff) %>%
  mean

# Other grain needed
fic08 %>%
  mutate(grain_sum = CornFlour + OatFlour + RiceDried + WheatFlour,
         diff = TotalGrain - grain_sum) %>%
  pull(diff) %>%
  mean

# apples OK
fic08 %>%
  mutate(apple_sum = Apples+ApplesFromJuice,
         diff = TotalApples - apple_sum) %>%
  pull(diff) %>%
  mean

# oranges OK
fic08 %>%
  mutate(orange_sum = Oranges+OrangesFromJuice,
         diff = TotalOranges - orange_sum) %>%
  pull(diff) %>%
  mean

# veg in general
fic08 %>%
  mutate(veg_sum = TotalBrassica + TotalLeafyVeg + TotalRootsAndTubers,
         diff = TotalVegetables - veg_sum) %>%
  pull(diff) %>%
  mean



# There is no "Other" column for Brassica, leafy vegetables, roots and tubers, and vegetables in general.

library(tidyverse)
fic08_modified <- fic08 %>%
  mutate(OtherVegetables = TotalVegetables - TotalBrassica - TotalLeafyVeg - TotalRootsAndTubers,
         OtherBrassica = TotalBrassica - BroccoliAndCauliflower,
         OtherLeafyVeg = TotalLeafyVeg - Lettuce,
         OtherRootsAndTubers = TotalRootsAndTubers - Potatoes,
         OtherPoultry = TotalPoultry - (Chicken + Turkey),
         OtherMeat = TotalMeatPoultryFish - (Beef + Pork + TotalPoultry + FinAndShellfish),
         OtherNuts = TotalNuts - (Peanuts + TreeNuts),
         OtherGrain = TotalGrain - (CornFlour + OatFlour + RiceDried + WheatFlour),
         OtherFruit = TotalFruit - (TotalApples + Bananas + Berries + Grapes + Melons + TotalOranges + OtherCitrusFruits + StoneFruits + TropicalFruits))

# Find issues.
sapply(fic08_modified, min)
# All approx zero except for rounding error.
fic08_modified <- fic08_modified %>%
  mutate_at(vars(TotalDairy:OtherFruit), ~ if_else(.x < 0, 0.0, as.numeric(.x)))

write.csv(fic08_modified, file.path(fp, 'food_consumption/FICRCD/corrected_ficrcd_2007_2008.csv'), row.names = FALSE)
