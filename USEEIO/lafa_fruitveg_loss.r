# Script to get fresh and nonfresh vegetable and fruit loss numbers.

source('read_data/read_lafa.r')

fresh_veg <- veg %>% filter(Category == 'Fresh vegetables', Year == 2015)
canned_veg <- veg %>% filter(Category == 'Canned vegetables', Year == 2015)
frozen_veg <- veg %>% filter(Category == 'Frozen vegetables', Year == 2015)
fresh_fruit <- fruit %>% filter(Category == 'Fresh fruit', Year == 2015)
canned_fruit <- fruit %>% filter(Category == 'Canned fruit', Year == 2015)
frozen_fruit <- fruit %>% filter(Category == 'Frozen fruit', Year == 2015)

# Relative loss of canned+frozen fruit compared to fresh fruit
# Weighted average by calories per day
frozen_fruit$Total_loss__all_levels_Percent
canned_fruit$Total_loss__all_levels_Percent
nonfresh_fruit_loss <- weighted.mean(x = c(frozen_fruit$Total_loss__all_levels_Percent, canned_fruit$Total_loss__all_levels_Percent),
                                     w = c(frozen_fruit$Calories_available_daily_Number, canned_fruit$Calories_available_daily_Number))
fresh_fruit_loss <- fresh_fruit$Total_loss__all_levels_Percent

nonfresh_veg_loss <- weighted.mean(x = c(frozen_veg$Total_loss__all_levels_Percent, canned_veg$Total_loss__all_levels_Percent),
                                   w = c(frozen_veg$Calories_available_daily_Number, canned_veg$Calories_available_daily_Number))
fresh_veg_loss <- fresh_veg$Total_loss__all_levels_Percent

nonfresh_fruitandveg_loss <- weighted.mean(x = c(nonfresh_fruit_loss, nonfresh_veg_loss),
                                           w = orig_demand[1:2])
fresh_fruitandveg_loss <- weighted.mean(x = c(fresh_fruit_loss, fresh_veg_loss),
                                        w = orig_demand[1:2])

loss_data = data.frame(food_type = c('freshfruit', 'freshveg', 'cannedfruitveg'),
                       loss = 0.01 * c(fresh_fruit_loss, fresh_veg_loss, nonfresh_fruitandveg_loss))