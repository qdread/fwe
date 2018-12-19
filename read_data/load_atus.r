library(atus)

data(atusact)
data(atuscps)
data(atusresp)

# Codes related to food
household_food_codes <- c(food_prep = "020201",
                          food_presentation = "020202",
                          food_cleanup = "020203",
                          food_notspecified = "020299")
food_shopping_codes <- c(grocery_shopping = "070101",
                         nongrocery_food_shopping = "070103",
                         shopping_waiting = "070105",
                         shopping_notspecified = "070199")
eating_drinking_codes <- c(eating_drinking = "110101",
                           eating_drinking_nonspecified = "110199",
                           eating_drinking_waiting = "110281",
                           eating_drinking_waiting_nonspecified = "110289",
                           eating_drinking_nonspecifiedtwice = "119999")
socialservice_food_codes <- c(socialservice_food_prep = "150201")
travel_food_codes <- c(grocery_shopping_travel = "180701",
                       eating_drinking_travel = "181101",
                       eating_drinking_travel_nonspecified = "181199")