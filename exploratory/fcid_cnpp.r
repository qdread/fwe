length(intersect(cnpp09$foodcode,fcidfoodsCW$Food_Code))

fcid_cnpp <- fcidfoodsCW %>% 
  rename(foodcode = Food_Code) %>%
  left_join(cnpp09)

# Get recipes
recipes <- read.csv(file.path(fp, 'food_consumption/FCID/Recipes_WWEIA_FCID_0510.csv'))


# Reshape long to wide recipes
recipes_wide <- recipes %>%
  filter(Mod_Code == 0) %>%
  rename(foodcode = Food_Code) %>%
  group_by(foodcode, FCID_Code) %>% ### this part is to correct error for a few duplicated ingredient names in same recipe
  summarize(Commodity_Weight = sum(Commodity_Weight)) %>%
  ungroup %>%
  select(foodcode, FCID_Code, Commodity_Weight) %>%
  spread(FCID_Code, Commodity_Weight)

# Ingredients
fcidingredientsCW <- fcidingredientsCW %>%
  select(-cgn, -CG_Subgroup, -NAICS_retail, -LAFA_category, -exact_match) %>%
  mutate_at(vars(contains('NAICS')), ~ if_else(.x == '', as.character(NA), as.character(.x)))

fcidingredientsCW %>%
  filter(!is.na(NAICS_production2) | !is.na(NAICS_processing2))
