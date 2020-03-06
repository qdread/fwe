# LAFA rates for fruit and vegetables

source(file.path(fp_github, 'fwe/read_data/read_lafa.r'))
lafa <- list(fruit, meat, veg)

# find the single year closest to 2012.
lafa_df <- lafa %>%
  map2_dfr(c('fruit','meat','veg'), ~ select(.x, Category, Year, Retail_weight_Lbs.year, Loss_from_retail__institutional_to_consumer_level_Percent, Consumer_weight_Lbs.year,
                                       Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent) %>%
             mutate(Group = .y)) %>%
  setNames(c('Food','Year','retail_weight','retail_loss','consumer_weight','avoidable_consumer_loss','Group')) %>%
  filter(!is.na(avoidable_consumer_loss)) %>%
  group_by(Group, Food) %>%
  filter(abs(Year - 2012) == min(abs(Year - 2012))) %>%
  ungroup %>%
  left_join(lafa_struct)

fruit_veg_total_waste <- lafa_df %>% 
  filter(Group %in% c('fruit','veg')) %>%
  mutate(retail_waste = retail_weight * retail_loss/100,
         avoidable_consumer_waste = consumer_weight * avoidable_consumer_loss/100,
         total_waste = retail_waste + avoidable_consumer_waste) %>%
  group_by(subgroup2) %>%
  summarize_at(vars(contains("waste")), sum)

# Normalize
fruit_veg_total_waste %>%
  mutate_if(is.numeric, ~ ./sum(.))

# ~ 1/3 of total retail/consumer fruit+veg waste is fresh fruit, and Refed states that 1/9 of total retail/consumer fruit+veg waste is packaged fresh fruit
# So we can assume based on that, that 1/3 of fresh fruit is packaged. We can assume different values for this.