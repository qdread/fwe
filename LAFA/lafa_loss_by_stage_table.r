# Create empty CSV file in which to fill the LAFA preparation status information for each commodity
# At each of 4 levels: arriving at processor, arriving at retailer, arriving at consumer, and final


# Load LAFA and get final year --------------------------------------------


source('read_data/read_lafa.r')

# Function to get the most recent year of data for each category.
most_recent <- function(dat) {
  dat %>% 
    group_by(Category) %>%
    filter(Year == if_else(any(!is.na(Loss_from_primary_to_retail_weight_Percent)), max(Year[!is.na(Loss_from_primary_to_retail_weight_Percent)]), max(Year))) %>%
    ungroup
}

dairy <- most_recent(dairy)
fat <- most_recent(fat)
fruit <- most_recent(fruit)
grain <- most_recent(grain)
meat <- most_recent(meat)
sugar <- most_recent(sugar)
veg <- most_recent(veg)

# Categorize food prep methods --------------------------------------------

fruit <- fruit %>%
  mutate(Prep_type = case_when(
    grepl('Fresh', Category) ~ 'fresh',
    grepl('Canned', Category) ~ 'canned',
    grepl('Frozen', Category) ~ 'frozen',
    grepl('Dried|Raisins', Category) ~ 'dried',
    grepl('Juice', Category, ignore.case = TRUE) ~ 'juice',
    TRUE ~ 'all'
  ),
  Food_type = case_when(
    Prep_type == 'fresh' ~ gsub('Fresh ', '', Category),
    Prep_type == 'canned' ~ gsub('Canned ', '', Category),
    Prep_type == 'frozen' ~ gsub('Frozen ', '', Category),
    Prep_type == 'dried' ~ gsub('Dried ', '', Category),
    Prep_type == 'juice' ~ gsub(' juice', '', Category),
    TRUE ~ 'all'
  )
  )

fruit$Food_type[fruit$Category == 'Raisins'] <- 'grapes'
fruit$Food_type <- tolower(fruit$Food_type)
fruit$Food_type[fruit$Category %in% c('Juice', 'Frozen fruit', 'Canned fruit', 'Fresh fruit', 'Dried fruit')] <- 'all'
fruit$Food_type[fruit$Category == 'Prune juice'] <- 'plums'

# Pluralize the singular fruit that needs
singular_fruit <- c('orange', 'lime', 'lemon', 'apple', 'cranberry', 'grape')
plural_fruit <- c('oranges', 'limes', 'lemons', 'apples', 'cranberries', 'grapes')

for (i in 1:length(singular_fruit)) fruit$Food_type[fruit$Food_type == singular_fruit[i]] <- plural_fruit[i]

veg <- veg %>%
  mutate(Prep_type = case_when(
    grepl('Fresh', Category) ~ 'fresh',
    grepl('Canned', Category) ~ 'canned',
    grepl('Frozen', Category) ~ 'frozen',
    grepl('Dry|Dehydrated', Category) ~ 'dried',
    grepl('Potato chips', Category) ~ 'chips',
    TRUE ~ 'all'
  ),
  Food_type = case_when(
    Prep_type == 'fresh' ~ gsub('Fresh ', '', Category),
    Prep_type == 'canned' ~ gsub('Canned ', '', Category),
    Prep_type == 'frozen' ~ gsub('Frozen ', '', Category),
    Prep_type == 'dried' ~ gsub('Dry ', '', gsub('Dehydrated ', '', Category)),
    Prep_type == 'chips' ~ 'potatoes',
    TRUE ~ 'all'
  )
  )

veg$Food_type[veg$Category %in% c('Fresh vegetables', 'Frozen vegetables', 'Canned vegetables')] <- 'all'
veg$Prep_type[veg$Category == 'Other canned vegetables'] <- 'canned'
veg$Prep_type[veg$Category == 'Misc frozen vegetables'] <- 'frozen'
veg$Food_type[veg$Category == 'Other dry beans'] <- 'other beans'
veg$Food_type[veg$Category == 'Legumes'] <- 'legumes'
veg$Prep_type[veg$Category %in% c('Other dry beans', 'Legumes')] <- 'dried'

# Identify which ones are individual and which ones are groups
veg_group_names <- c('all', 'legumes', 'other beans', 'edible beans')

veg <- veg %>%
  mutate(is_group = Food_type %in% veg_group_names)

# Create groups
the_nuts <- c('Almonds', 'Coconut', 'Hazelnuts', 'Macadamia', 'Peanuts', 'Pecans', 'Pistachios', 'Walnuts', 'Other tree nuts', 'Total tree nuts')
the_meats <- c('Beef', 'Chicken', 'Eggs', 'Lamb', 'Pork', 'Poultry', 'Red meat', 'Turkey', 'Veal')
the_seafood <- c('Canned fish and shellfish', 'Canned Salmon', 'Canned Sardines', 'Canned shellfish', 'Canned Tuna', 'Cured fish', 'Fresh and frozen fish', 'Fresh and frozen shellfish', 'Other canned fish', 'Total fish and shellfish', 'Total Fresh and Frozen Fish')

meat <- meat %>%
  mutate(Prep_type = case_when(
    grepl('fresh and frozen', tolower(Category)) ~ 'fresh/frozen',
    grepl('cured', tolower(Category)) ~ 'cured',
    grepl('fresh', tolower(Category)) ~ 'fresh',
    grepl('canned', tolower(Category)) ~ 'canned',
    grepl('frozen', tolower(Category)) ~ 'frozen',
    TRUE ~ 'fresh'
  ),
  Food_type = case_when(
    Category %in% the_nuts ~ 'nuts',
    Category %in% the_meats ~ 'meats',
    Category %in% the_seafood ~ 'seafood',
    TRUE ~ 'all'
  )
  )

protein_groups <- c('Red meat', 'Poultry', 'Total Fresh and Frozen Fish', 'Canned fish and shellfish', 'Total fish and shellfish', 'Total tree nuts', 'Total meat fish eggs nuts group')

meat$Prep_type[meat$Category %in% c('Total fish and shellfish', 'Total meat fish eggs nuts group')] <- 'all'

meat$is_group <- meat$Category %in% protein_groups

dairy <- dairy %>%
  mutate(Prep_type = case_when(
    grepl('evap|Evap', Category) ~ 'evaporated',
    grepl('dry|Dry', Category) ~ 'dry',
    grepl('Ice milk', Category) ~ 'other',
    grepl('cheese', Category) ~ 'cheese',
    grepl('milk', Category) ~ 'milk',
    
    TRUE ~ 'other'
  )
  )

dairy_grp_names <- c('All plain milk', 'All flavored milk', 'All beverage milks', 'Total fluid milk', 'All American cheese', 'All Italian cheese', 'Total miscellaneous cheeses', 'Total cheese', 'Total cottage cheese', 'Frozen dairy products', 'All evaporated condensed milk', 'Dry milk products', 'Total dairy products', 'Half and half and eggnog')

dairy <- dairy %>% mutate(is_group = Category %in% dairy_grp_names)


names_use <- c(names(dairy)[c(1,4,6,8,9,3,5,7,11)],'Food_type','Prep_type')

# Correct the few NA values
all_loss <- map_dfr(c('dairy', 'fat', 'fruit', 'grain', 'meat', 'sugar', 'veg'), ~ data.frame(Food_Group = .x, get(.x) %>% select(names(.)[names(.) %in% names_use]))) %>%
  setNames(c('Food_Group', 'Commodity', 'Primary_weight', 'Primary', 'Retail_weight', 'Retail', 'Consumer_weight', 'Consumer_nonedible', 'Consumer_cooking_uneaten', 'Final_weight', 'Prep_type', 'Food_type')) %>%
  filter(!is.na(Primary) | !is.na(Retail) | !is.na(Consumer_nonedible) | !is.na(Consumer_cooking_uneaten)) %>%
  mutate(Primary = if_else(is.na(Primary), 100 * (1 - Retail_weight/Primary_weight), Primary)) %>%
  select(Food_Group, Food_type, Prep_type, Commodity, everything())

write.csv(all_loss, file = '/nfs/qread-data/crossreference_tables/lafa_weights_by_stage.csv', row.names = FALSE)

all_loss_longform <- all_loss %>%
  select(Food_Group, Category, Primary, Retail, Consumer_nonedible, Consumer_cooking_uneaten) %>%
  gather(Stage, Loss, -Food_Group, -Category) %>%
  arrange(Food_Group, Category)

write.csv(all_loss_longform, file = '/nfs/qread-data/crossreference_tables/lafa_loss_by_stage.csv', row.names = FALSE)



