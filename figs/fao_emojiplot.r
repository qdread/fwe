# FAO percentages emoji plots!!!

# Update 25 Apr: add direct labels; use alternative formulation of data

library(tidyverse)
library(XLConnect)

fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
faopct <- readWorksheetFromFile(file.path(fp_crosswalks, 'fao_percentages.xlsx'), sheet = 1)
faopct <- readWorksheetFromFile(file.path(fp_crosswalks, 'fao_percentages_alternative.xlsx'), sheet = 1) # Switch processing to production group.

# Make a figure or two of the FAO percentages so that it can be put in the PPTX
library(directlabels) # Replace this with pictures later on
library(emoGG)

faopct %>%
  mutate(weight1 = 1, weight2 = 1 - L1, weight3 = 1 - L1L2, weight4 = 1 - L1L2L3) %>%
  select(category, weight1, weight2, weight3, weight4) %>%
  gather(stage, weight, -category) %>%
  ggplot(aes(x = stage, y = weight, group = category)) +
    geom_line() +
    #geom_dl(aes(label = category), method = 'last.points') +
    scale_y_continuous(limits = c(0.3, 1), expand = c(0, 0), name = 'Mass remaining', labels = scales::percent) +
    scale_x_discrete(labels = c('Processor', 'Retailer', 'Consumer', 'Final'), expand = c(0, 0)) +
    expand_limits(x = 5) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank())

# Emoji mapping
# bread, potato, French fries, seedling, peach, canned food, meat on bone, fish, sushi, cheese, egg
emoji_mapping <- c('1f35e', '1f954', '1f35f', '1f331', '1f351', '1f96b', '1f357', '1f41f', '1f363', '1f9c0', '1f95a')

faoplotdat <- faopct %>%
  mutate(weight1 = 1, weight2 = 1 - L1, weight3 = 1 - L1L2, weight4 = 1 - L1L2L3, emojicode = emoji_mapping) %>%
  select(category, emojicode, weight1, weight2, weight3, weight4) %>%
  gather(stage, weight, -category, -emojicode)

# Sick emoji plot!!!
emojiplot <- ggplot(faoplotdat, aes(x = stage, y = weight, group = category)) +
  geom_line() +
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f35e'), emoji='1f35e') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f954'), emoji='1f954') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f35f'), emoji='1f35f') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f331'), emoji='1f331') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f351'), emoji='1f351') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f96b'), emoji='1f96b') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f357'), emoji='1f357') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f41f'), emoji='1f41f') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f363'), emoji='1f363') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f9c0'), emoji='1f9c0') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f95a'), emoji='1f95a') + 
  scale_y_continuous(limits = c(0.3, 1.05), expand = c(0, 0), breaks = c(0.4, 0.6, 0.8, 1), name = 'Mass remaining', labels = scales::percent) +
  scale_x_discrete(labels = c('Producer', 'Retailer', 'Consumer', 'Final'), name = 'Supply chain stage') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())
ggsave('/nfs/qread-data/figures/FAO_FLW_emojis.png', emojiplot, height = 5, width = 5, dpi = 400)

# Emojiplot on black and white
emojiplot <- ggplot(faoplotdat, aes(x = stage, y = weight, group = category)) +
  geom_line(color = 'white') +
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f35e'), emoji='1f35e') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f954'), emoji='1f954') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f35f'), emoji='1f35f') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f331'), emoji='1f331') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f351'), emoji='1f351') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f96b'), emoji='1f96b') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f357'), emoji='1f357') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f41f'), emoji='1f41f') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f363'), emoji='1f363') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f9c0'), emoji='1f9c0') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f95a'), emoji='1f95a') + 
  scale_y_continuous(limits = c(0.3, 1.05), expand = c(0, 0), breaks = c(0.4, 0.6, 0.8, 1), name = 'Mass remaining', labels = scales::percent) +
  scale_x_discrete(labels = c('Producer', 'Retailer', 'Consumer', 'Final'), name = 'Supply chain stage') +
  theme_black() +
  theme(panel.grid.major.x = element_blank())
ggsave('Q:/figures/FAO_FLW_emojis_BW.png', emojiplot, height = 5, width = 5, dpi = 400)

# Key for emoji plot (legends aren't supported by emoGG)
keydat <- data.frame(category = faopct$category, emojicode = emoji_mapping, x = 1, y = 1:11, stringsAsFactors = FALSE)
keydat$category[10] <- 'dairy'
emojikey <- ggplot(keydat, aes(x,y)) +
  geom_text(aes(label = category), hjust = 0) +
  theme_void() +
  geom_emoji(emoji = '1f35e', x = 0.9, y = 1) +
  geom_emoji(emoji = '1f954', x = 0.9, y = 2) +
  geom_emoji(emoji = '1f35f', x = 0.9, y = 3) +
  geom_emoji(emoji = '1f331', x = 0.9, y = 4) +
  geom_emoji(emoji = '1f351', x = 0.9, y = 5) +
  geom_emoji(emoji = '1f96b', x = 0.9, y = 6) +
  geom_emoji(emoji = '1f357', x = 0.9, y = 7) +
  geom_emoji(emoji = '1f41f', x = 0.9, y = 8) +
  geom_emoji(emoji = '1f363', x = 0.9, y = 9) +
  geom_emoji(emoji = '1f9c0', x = 0.9, y = 10) +
  geom_emoji(emoji = '1f95a', x = 0.9, y = 11) +
  scale_x_continuous(limits=c(0,2))
ggsave('/nfs/qread-data/figures/FAO_FLW_emojis_key.png', emojikey, height = 5, width = 5, dpi = 400)

emojikey <- ggplot(keydat, aes(x,y)) +
  geom_text(aes(label = category), hjust = 0, color = 'white') +
  theme_void() +
  theme(panel.background = element_rect(fill = 'black'),
        plot.background = element_rect(fill = 'black')) +
  geom_emoji(emoji = '1f35e', x = 0.9, y = 1) +
  geom_emoji(emoji = '1f954', x = 0.9, y = 2) +
  geom_emoji(emoji = '1f35f', x = 0.9, y = 3) +
  geom_emoji(emoji = '1f331', x = 0.9, y = 4) +
  geom_emoji(emoji = '1f351', x = 0.9, y = 5) +
  geom_emoji(emoji = '1f96b', x = 0.9, y = 6) +
  geom_emoji(emoji = '1f357', x = 0.9, y = 7) +
  geom_emoji(emoji = '1f41f', x = 0.9, y = 8) +
  geom_emoji(emoji = '1f363', x = 0.9, y = 9) +
  geom_emoji(emoji = '1f9c0', x = 0.9, y = 10) +
  geom_emoji(emoji = '1f95a', x = 0.9, y = 11) +
  scale_x_continuous(limits=c(0,2))
ggsave('Q:/figures/FAO_FLW_emojis_key_BW.png', emojikey, height = 5, width = 5, dpi = 400)


# Emoji plot with labels --------------------------------------------------

# manually jitter the points: all are on top of each other at w1
# wt2: eggs and milk; fresh fish, processed fish, and oilseeds; fresh fruit and processed fruit; fresh roots and processed roots
# wt3: eggs and milk; fresh fish and processed fruit
# wt4: eggs, milk, and oilseeds

faoplotdat$xpos <- rep(1:4, each = 11)
faoplotdat$xpos[faoplotdat$category == 'eggs' & faoplotdat$stage == 'weight2'] <- 1.94
faoplotdat$xpos[faoplotdat$category == 'milk' & faoplotdat$stage == 'weight2'] <- 2.06
faoplotdat$xpos[faoplotdat$category == 'fish and seafood, fresh' & faoplotdat$stage == 'weight2'] <- 1.9
faoplotdat$xpos[faoplotdat$category == 'oilseeds and pulses' & faoplotdat$stage == 'weight2'] <- 2.1
faoplotdat$xpos[faoplotdat$category == 'roots and tubers, fresh' & faoplotdat$stage == 'weight2'] <- 1.94
faoplotdat$xpos[faoplotdat$category == 'roots and tubers, processed' & faoplotdat$stage == 'weight2'] <- 2.06
faoplotdat$xpos[faoplotdat$category == 'fruit and vegetable, fresh' & faoplotdat$stage == 'weight2'] <- 1.94
faoplotdat$xpos[faoplotdat$category == 'fruit and vegetable, processed' & faoplotdat$stage == 'weight2'] <- 2.06
faoplotdat$xpos[faoplotdat$category == 'eggs' & faoplotdat$stage == 'weight3'] <- 2.94
faoplotdat$xpos[faoplotdat$category == 'milk' & faoplotdat$stage == 'weight3'] <- 3.06
faoplotdat$xpos[faoplotdat$category == 'fish and seafood, fresh' & faoplotdat$stage == 'weight3'] <- 2.94
faoplotdat$xpos[faoplotdat$category == 'fruit and vegetable, processed' & faoplotdat$stage == 'weight3'] <- 3.06
faoplotdat$xpos[faoplotdat$category == 'eggs' & faoplotdat$stage == 'weight4'] <- 3.9
faoplotdat$xpos[faoplotdat$category == 'oilseeds and pulses' & faoplotdat$stage == 'weight4'] <- 4.1

keydat <- data.frame(category = faopct$category, emojicode = emoji_mapping, x = 1, y = 1:11, stringsAsFactors = FALSE)
keydat$x <- 4.16
keydat <- left_join(keydat, faoplotdat %>% filter(stage == 'weight4'))
keydat$category[10] <- 'dairy'
keydat$weight[4] <- 0.784
keydat$weight[7] <- 0.76
keydat$weight[10] <- 0.812
keydat$weight[11] <- 0.83
keydat$x[11] <- 3.9

emojiplot_jittered <- ggplot(faoplotdat, aes(x = xpos, y = weight, group = category)) +
  geom_line(size = 0.6) +
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f35e'), emoji='1f35e') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f954'), emoji='1f954') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f35f'), emoji='1f35f') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f351'), emoji='1f351') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f96b'), emoji='1f96b') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f357'), emoji='1f357') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f41f'), emoji='1f41f') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f363'), emoji='1f363') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f9c0'), emoji='1f9c0') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f95a'), emoji='1f95a') + 
  geom_emoji(data = faoplotdat %>% filter(emojicode=='1f331'), emoji='1f331') + 
  scale_y_continuous(limits = c(0.3, 1.05), expand = c(0, 0), breaks = c(0.4, 0.6, 0.8, 1), name = 'Mass remaining', labels = scales::percent) +
  scale_x_continuous(breaks = 1:4, labels = c('Producer', 'Retailer', 'Consumer', 'Final'), name = 'Supply chain stage', limits = c(1, 5.5)) +
  geom_text(data = keydat, aes(x = x, y = weight, label = category), hjust = 0, size = 2.5) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggsave('Q:/figures/FAO_FLW_emojis_jitterplot.png', emojiplot_jittered, height = 5, width = 5, dpi = 400)
