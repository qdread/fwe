# Average LAFA waste percentages (weighted) by NAICS-ish categories that are in USEEIO.
# QDR / FWE / 05 Dec 2018

# Use the 2012 data for LAFA to get the total retail weight of each of the LAFA groups

source('read_data/read_lafa.r')

us_pop <- 314e6 # US Population on July 1, 2012 according to census.gov

freshfruit2012 <- subset(fruit, Category == 'Fresh fruit' & Year == 2012)
freshfruit2012$Retail_weight_Lbs.year * us_pop # 39B pounds fresh fruit were available in 2012.
# 10% suboptimally packaged
freshfruit2012$Retail_weight_Lbs.year * us_pop * 0.1 # 3.9B pounds fresh fruit were suboptimally packaged
# 5 cents per pound to improve packaging
freshfruit2012$Retail_weight_Lbs.year * us_pop * 0.1 * 0.05 # It would cost $196 million to improve the fresh fruit packaging to optimal.

freshveg2012 <- subset(veg, Category == 'Fresh vegetables' & Year == 2012)
freshveg2012$Retail_weight_Lbs.year * us_pop
freshveg2012$Retail_weight_Lbs.year * us_pop * 0.1
freshveg2012$Retail_weight_Lbs.year * us_pop * 0.1 * 0.05 # It would cost $274 million to improve the fresh veg packaging to optimal.
