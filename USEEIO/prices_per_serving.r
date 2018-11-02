# Use the 1999 HomeScan numbers from Reed et al to get the ratio in cost of fresh to canned/frozen produce
# in dollars per serving
pps <- read.csv('~/Dropbox/projects/foodwaste/Data/prices_per_serving.csv')
fresh_fruit_price <- pps$price_per_serving[pps$food=='fruit' & pps$method=='fresh']
fresh_veg_price <- pps$price_per_serving[pps$food=='vegetable' & pps$method=='fresh']

nonfresh_fruit_price <- weighted.mean(x = pps$price_per_serving[c(3,2)],
                                      w = c(frozen_fruit$Calories_available_daily_Number, canned_fruit$Calories_available_daily_Number))
nonfresh_veg_price <- weighted.mean(x = pps$price_per_serving[c(6,5)],
                                    w = c(frozen_veg$Calories_available_daily_Number, canned_veg$Calories_available_daily_Number))

# It turns out that on average, processed fruit and vegetables cost 1.67 times more than fresh equivalent. The ratio is the same for both.