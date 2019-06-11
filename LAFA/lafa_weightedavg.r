# Calculate weighted averages of LAFA loss rates by food group

library(tidyverse)

fp <- ifelse(dir.exists('Q:/'), 'Q:/crossreference_tables', '/nfs/qread-data/crossreference_tables')
lafa <- read.csv(file.path(fp, 'lafa_weights_by_stage.csv'), stringsAsFactors = FALSE)

# Get the total weights of each food group
lafa_byfoodgroup <- lafa %>% 
  group_by(Food_Group) %>%
  summarize_at(vars(contains('weight')), ~ sum(.)) %>%
  mutate(primary_loss = 1 - Retail_weight/Primary_weight,
         retail_loss = 1 - Consumer_weight/Retail_weight,
         consumer_loss = 1 - Final_weight/Consumer_weight)
