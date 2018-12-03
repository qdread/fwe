# Simple graphs of food loss of different types of fruit and vegetables (LAFA)

source('read_data/read_lafa.r')

# Function to get the most recent year of data for each category.
most_recent <- function(dat) {
  dat %>% 
    group_by(Category) %>%
    filter(Year == if_else(any(!is.na(Loss_from_primary_to_retail_weight_Percent)), max(Year[!is.na(Loss_from_primary_to_retail_weight_Percent)]), max(Year))) %>%
    ungroup
}

# Manipulate and plot fruit data ------------------------------------------



# Visualization of fruit loss types.

library(ggplot2)
library(reshape2)

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

# Identify which ones are individual and which ones are groups
group_names <- c('citrus', 'noncitrus', 'all', 'berries')

fruit <- fruit %>%
  mutate(is_group = Food_type %in% group_names)

fruit2015 <- most_recent(fruit)

# Do calculations to get the proportions.
fruit2015 <- fruit2015 %>%
  mutate(retail_percent_remaining = (1 - Loss_from_primary_to_retail_weight_Percent/100),
         consumer_percent_remaining = retail_percent_remaining * (1 - Loss_from_retail__institutional_to_consumer_level_Percent/100),
         after_consumer_loss_percent_remaining = consumer_percent_remaining * (1 - Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100),
         nonedible_portion = consumer_percent_remaining * (Loss_at_consumer_level_Nonedible_share_Percent/100))

# Correct the columns to combine the "loss" and "gain" from primary to retail columns
fruit2015 <- fruit2015 %>%
  mutate(Loss_from_primary_to_retail_weight_Percent = pmin(Loss_from_primary_to_retail_weight_Percent, Gain_from_primary_to_retail_weight_Percent, na.rm = TRUE))

# Use stacking bars.
fruit_plotdata <- fruit2015 %>%
  mutate(processor_loss = Loss_from_primary_to_retail_weight_Percent/100,
         retailer_loss = (1 - processor_loss) * Loss_from_retail__institutional_to_consumer_level_Percent/100,
         consumer_loss = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100,
         nonedible = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Nonedible_share_Percent/100,
         eaten = 1 - (processor_loss + retailer_loss + consumer_loss + nonedible)) %>%
  select(Category, Prep_type, Food_type, is_group, processor_loss, retailer_loss, consumer_loss, nonedible, eaten)

# Create fill scale to set off the lost proportion from the available portion
cols_bar <- c('#EFFD5F','#DAA520','#F8E473','darkolivegreen3', 'darkgreen')

# Theme for plot
th_bar <- theme_bw() + theme(strip.background = element_rect(fill = 'transparent'))

# Make data to draw rectangle around the available portion.
fruit_avail <- fruit_plotdata %>%
  filter(!Prep_type %in% 'all', !is_group) %>%
  filter(complete.cases(.)) %>%
  mutate(available = nonedible + eaten)


fruit_plotdata %>%
  filter(!Prep_type %in% 'all', !is_group) %>%
  filter(complete.cases(.)) %>%
  mutate(Prep_type = factor(Prep_type, levels = c('fresh', 'canned', 'frozen', 'juice', 'dried'))) %>%
  melt(id.vars = 1:4, value.name = 'proportion') %>%
  ggplot(aes(x = Prep_type, y = proportion, fill = variable)) +
    facet_wrap(~ Food_type) +
    geom_col() +
    geom_col(data = fruit_avail, aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
    th_bar +
    scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
    ggtitle('Fruit FLW by weight (LAFA)', 'Black box is proportion available to consumer')

# Show only a small subset.
the_fruits <- c('apples', 'peaches', 'pineapple', 'plums')

fruit_colplot <- fruit_plotdata %>%
  filter(!Prep_type %in% 'all', !is_group, Food_type %in% the_fruits) %>%
  filter(complete.cases(.)) %>%
  mutate(Prep_type = factor(Prep_type, levels = c('fresh', 'canned', 'frozen', 'juice', 'dried'))) %>%
  melt(id.vars = 1:4, value.name = 'proportion') %>%
  ggplot(aes(x = Prep_type, y = proportion, fill = variable)) +
  facet_wrap(~ Food_type) +
  geom_col() +
  geom_col(data = fruit_avail %>% filter(Food_type %in% the_fruits), aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
  ggtitle('Fruit FLW by weight (LAFA)', 'Black box comprises the eaten portion plus "unavoidable" waste')

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_fruit_flw.pdf', fruit_colplot, height = 5, width = 6)


# Manipulate and plot veg data --------------------------------------------

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

veg2015 <- most_recent(veg)

# Calculations for heights of stacking bars
veg_plotdata <- veg2015 %>%
  mutate(processor_loss = Loss_from_primary_to_retail_weight_Percent/100,
         retailer_loss = (1 - processor_loss) * Loss_from_retail__institutional_to_consumer_level_Percent/100,
         consumer_loss = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100,
         nonedible = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Nonedible_share_Percent/100,
         eaten = 1 - (processor_loss + retailer_loss + consumer_loss + nonedible)) %>%
  select(Category, Prep_type, Food_type, is_group, processor_loss, retailer_loss, consumer_loss, nonedible, eaten)

# Make data to draw rectangle around the available portion.
veg_avail <- veg_plotdata %>%
  filter(!Prep_type %in% 'all', !is_group) %>%
  filter(complete.cases(.)) %>%
  mutate(available = nonedible + eaten)

veg_plotdata %>%
  filter(!Prep_type %in% 'all', !is_group) %>%
  filter(complete.cases(.)) %>%
  mutate(Prep_type = factor(Prep_type, levels = c('fresh', 'canned', 'frozen', 'dried', 'chips'))) %>%
  melt(id.vars = 1:4, value.name = 'proportion') %>%
  ggplot(aes(x = Prep_type, y = proportion, fill = variable)) +
  facet_wrap(~ Food_type) +
  geom_col() +
  geom_col(data = veg_avail, aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
  ggtitle('Vegetable FLW by weight (LAFA)', 'Black box is proportion available to consumer')

# Plot only a small subset
the_veggies <- c('sweet corn', 'potatoes', 'broccoli', 'snap beans', 'cabbage', 'spinach')

veg_colplot <- veg_plotdata %>%
  filter(!Prep_type %in% 'all', !is_group, Food_type %in% the_veggies) %>%
  filter(complete.cases(.)) %>%
  mutate(Prep_type = factor(Prep_type, levels = c('fresh', 'canned', 'frozen', 'dried', 'chips'))) %>%
  melt(id.vars = 1:4, value.name = 'proportion') %>%
  ggplot(aes(x = Prep_type, y = proportion, fill = variable)) +
  facet_wrap(~ Food_type) +
  geom_col() +
  geom_col(data = veg_avail %>% filter(Food_type %in% the_veggies), aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
  ggtitle('Vegetable FLW by weight (LAFA)', 'Black box comprises the eaten portion plus "unavoidable" waste')

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_veg_flw.pdf', veg_colplot, height = 5, width = 7.5)


# Manipulate and plot meat data -------------------------------------------

# Create groups
the_nuts <- c('Almonds', 'Coconut', 'Hazelnuts', 'Macadamia', 'Peanuts', 'Pecans', 'Pistachios', 'Walnuts', 'Other tree nuts', 'Total tree nuts')
the_meats <- c('Beef', 'Chicken', 'Eggs', 'Lamb', 'Pork', 'Poultry', 'Red meat', 'Turkey', 'Veal')
the_seafood <- c('Canned fish and shellfish', 'Canned Salmon', 'Canned Sardines', 'Canned shellfish', 'Canned Tuna', 'Cured fish', 'Fresh and frozen fish', 'Fresh and frozen shellfish', 'Other canned fish', 'Total fish and shellfish', 'Total Fresh and Frozen Fish')

meat <- meat %>%
  mutate(Prep_type = case_when(
    grepl('fresh and frozen', tolower(Category)) ~ 'fresh/frozen',
    grepl('cured', tolower(Category)) ~ 'cured'
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

meat2015 <- most_recent(meat)

# Calculations for heights of stacking bars
meat_plotdata <- meat2015 %>%
  mutate(processor_loss = Loss_from_primary_to_retail_weight_Percent/100,
         retailer_loss = (1 - processor_loss) * Loss_from_retail__institutional_to_consumer_level_Percent/100,
         consumer_loss = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100,
         nonedible = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Nonedible_share_Percent/100,
         eaten = 1 - (processor_loss + retailer_loss + consumer_loss + nonedible)) %>%
  select(Category, Prep_type, Food_type, is_group, processor_loss, retailer_loss, consumer_loss, nonedible, eaten)

# Make data to draw rectangle around the available portion.
meat_avail <- meat_plotdata %>%
  filter(!is_group) %>%
  filter(complete.cases(.)) %>%
  mutate(available = nonedible + eaten)

meat_colplot <- meat_plotdata %>%
  filter(Food_type %in% 'meats') %>%
  filter(complete.cases(.)) %>%
  melt(id.vars = 1:4, value.name = 'proportion') %>%
  ggplot(aes(x = Category, y = proportion, fill = variable)) +
  geom_col() +
  geom_col(data = meat_avail %>% filter(Food_type %in% 'meats'), aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
  ggtitle('Meat FLW by weight (LAFA)', 'Black box comprises the eaten portion plus "unavoidable" waste')

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_meat_flw.pdf', meat_colplot, height = 5, width = 6)

seafood_colplot <- meat_plotdata %>%
  filter(Food_type %in% 'seafood') %>%
  mutate(Category = Hmisc::capitalize(tolower(Category))) %>%
  filter(complete.cases(.)) %>%
  melt(id.vars = 1:4, value.name = 'proportion') %>%
  ggplot(aes(x = Category, y = proportion, fill = variable)) +
  geom_col() +
  geom_col(data = meat_avail %>% filter(Food_type %in% 'seafood') %>% mutate(Category = Hmisc::capitalize(tolower(Category))), aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
  ggtitle('Seafood FLW by weight (LAFA)', 'Black box comprises the eaten portion plus "unavoidable" waste')

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_seafood_flw.pdf', seafood_colplot, height = 5, width = 8)

nuts_colplot <- meat_plotdata %>%
  filter(Food_type %in% 'nuts') %>%
  filter(complete.cases(.)) %>%
  melt(id.vars = 1:4, value.name = 'proportion') %>%
  ggplot(aes(x = Category, y = proportion, fill = variable)) +
  geom_col() +
  geom_col(data = meat_avail %>% filter(Food_type %in% 'nuts'), aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 10, simplify = FALSE), paste, collapse="\n")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
  ggtitle('Nuts FLW by weight (LAFA)', 'Black box comprises the eaten portion plus "unavoidable" waste')

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_nuts_flw.pdf', nuts_colplot, height = 5, width = 8)


# Manipulate and plot sugars ----------------------------------------------

# easier, just has a few categories and no difference in prep methods.

sugar_grps <- c('Total honey and syrup', 'Corn sweeteners', 'Caloric sweeteners')

sugar <- sugar %>% mutate(is_group = Category %in% sugar_grps)

sugar2015 <- most_recent(sugar)

sugar_plotdata <- sugar2015 %>%
  mutate(processor_loss = Loss_from_primary_to_retail_weight_Percent/100,
         retailer_loss = (1 - processor_loss) * Loss_from_retail__institutional_to_consumer_level_Percent/100,
         consumer_loss = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100,
         nonedible = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Nonedible_share_Percent/100,
         eaten = 1 - (processor_loss + retailer_loss + consumer_loss + nonedible)) %>%
  select(Category, is_group, processor_loss, retailer_loss, consumer_loss, nonedible, eaten)

sugar_colplot <- sugar_plotdata %>%
  select(-nonedible) %>%
  filter(complete.cases(.)) %>%
  melt(id.vars = 1:2, value.name = 'proportion') %>%
  ggplot(aes(x = Category, y = proportion, fill = variable)) +
  geom_col() +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 12, simplify = FALSE), paste, collapse="\n")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar[-4], labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Eaten')) +
  ggtitle('Sugars FLW by weight (LAFA)', 'Not considered to have nonedible portion at consumer level')

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_sugars_flw.pdf', sugar_colplot, height = 5, width = 7)


# Manipulate and plot dairy data ------------------------------------------

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

dairy2015 <- most_recent(dairy)

dairy_plotdata <- dairy2015 %>%
  mutate(processor_loss = Loss_from_primary_to_retail_weight_Percent/100,
         retailer_loss = (1 - processor_loss) * Loss_from_retail__institutional_to_consumer_level_Percent/100,
         consumer_loss = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100,
         nonedible = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Nonedible_share_Percent/100,
         eaten = 1 - (processor_loss + retailer_loss + consumer_loss + nonedible)) %>%
  select(Category, Prep_type, is_group, processor_loss, retailer_loss, consumer_loss, nonedible, eaten)



# Reduce labeling
dairy_plotdata$Category[dairy_plotdata$Prep_type == 'milk'] <- gsub(' milk', '', dairy_plotdata$Category[dairy_plotdata$Prep_type == 'milk'])
dairy_plotdata$Category[dairy_plotdata$Prep_type == 'cheese'] <- gsub(' cheese', '', dairy_plotdata$Category[dairy_plotdata$Prep_type == 'cheese'])

# Make data to draw rectangle around the available portion.
dairy_avail <- dairy_plotdata %>%
  filter(!is_group) %>%
  filter(complete.cases(.)) %>%
  mutate(available = nonedible + eaten)


dairy_colplot <- dairy_plotdata %>%
  filter(!is_group) %>%
  filter(complete.cases(.)) %>%
  melt(id.vars = 1:3, value.name = 'proportion') %>%
  ggplot(aes(x = Category, y = proportion, fill = variable)) +
  facet_wrap(~ Prep_type, scales = 'free_x') +
  geom_col() +
  geom_col(data = dairy_avail, aes(y = available), fill = 'transparent', color = 'black', size = 1.5) +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 12, simplify = FALSE), paste, collapse="\n")) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar, labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Nonedible', 'Eaten')) +
  ggtitle('Dairy FLW by weight (LAFA)', 'Black box comprises the eaten portion plus "unavoidable" waste') +
  theme(legend.position = c(0.8, 0.2))

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_dairy_flw.pdf', dairy_colplot, height = 10, width = 12)


# Manipulate and plot fat data --------------------------------------------

fat2015 <- most_recent(fat)

fat_plotdata <- fat2015 %>%
  mutate(processor_loss = Loss_from_primary_to_retail_weight_Percent/100,
         retailer_loss = (1 - processor_loss) * Loss_from_retail__institutional_to_consumer_level_Percent/100,
         consumer_loss = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100,
         nonedible = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Nonedible_share_Percent/100,
         eaten = 1 - (processor_loss + retailer_loss + consumer_loss + nonedible)) %>%
  select(Category, processor_loss, retailer_loss, consumer_loss, nonedible, eaten)

fat_colplot <- fat_plotdata %>%
  select(-nonedible) %>%
  filter(complete.cases(.)) %>%
  melt(id.vars = 1, value.name = 'proportion') %>%
  ggplot(aes(x = Category, y = proportion, fill = variable)) +
  geom_col() +
  scale_x_discrete(labels = function(x) lapply(strwrap(x, width = 12, simplify = FALSE), paste, collapse="\n")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1.02), expand = c(0,0)) +
  th_bar +
  scale_fill_manual(name = 'Proportion', values = cols_bar[-4], labels = c('Processor loss', 'Retailer loss', 'Consumer loss\n(avoidable)', 'Eaten')) +
  ggtitle('Fats FLW by weight (LAFA)', 'Not considered to have nonedible portion at consumer level')

ggsave('~/Dropbox/projects/foodwaste/Results/lafa_fats_flw.pdf', fat_colplot, height = 5, width = 8)
