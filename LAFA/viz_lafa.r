# Simple graphs of food loss of different types of fruit and vegetables (LAFA)

source('read_data/read_lafa.r')


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

fruit2015 <- fruit %>%
  filter(Year == 2015)

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
  ggtitle('Fruit FLW by weight (LAFA)', 'Black box is proportion available to consumer')

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

veg2015 <- veg %>%
  filter(Year == 2015)

# Calculations for heights of stacking bars
veg_plotdata <- veg2015 %>%
  mutate(processor_loss = Loss_from_primary_to_retail_weight_Percent/100,
         retailer_loss = (1 - processor_loss) * Loss_from_retail__institutional_to_consumer_level_Percent/100,
         consumer_loss = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Other__cooking_loss_and_uneaten_food__Percent/100,
         nonedible = (1 - processor_loss) * (1 - retailer_loss) * Loss_at_consumer_level_Nonedible_share_Percent/100,
         eaten = 1 - (processor_loss + retailer_loss + consumer_loss + nonedible)) %>%
  select(Category, Prep_type, Food_type, is_group, processor_loss, retailer_loss, consumer_loss, nonedible, eaten)
