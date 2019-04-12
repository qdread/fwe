# FAO percentages emoji plots!!!

fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
faopct <- readWorksheetFromFile(file.path(fp_crosswalks, 'fao_percentages.xlsx'), sheet = 1)

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
  scale_x_discrete(labels = c('Processor', 'Retailer', 'Consumer', 'Final'), name = 'Supply chain stage') +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())
ggsave('/nfs/qread-data/figures/FAO_FLW_emojis.png', emojiplot, height = 5, width = 5, dpi = 400)

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
