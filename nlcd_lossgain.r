# Join the NLCD LUC sums by BBS with the transition table.

luc0611 <- read.csv('Q:/BBS/bbs_nlcd_change0611_1kmbuffer.csv')
table0611 <- read.csv('Q:/NLCD/nlcd_change0611_levels.csv')
table11 <- read.csv('Q:/NLCD/nlcd_2011_levels.csv')

library(dplyr)
library(reshape2)  

luc_props <- luc0611 %>%
  group_by(category) %>%
  summarize(med_prop = median(proportion)) %>%
  arrange(-med_prop) %>%
  left_join(table0611 %>% select(ID, X2006.2011.Class), by = c('category' = 'ID'))

# classify the table by loss of natural habitat or gain of natural habitat.
# First say which codes are natural and which are not.
unique_codes <- table11$ID[match(unique(table11$Land.Cover.Class), table11$Land.Cover.Class)]
names(unique_codes) <- unique(table11$Land.Cover.Class)

ag_codes <- c(81, 82)
devel_codes <- c(21, 22, 23, 24)
terr_natural_codes <- c(31, 41, 42, 43, 52, 71, 90, 95)

natural_loss_rows <- table0611[table0611$X2006.Code %in% terr_natural_codes & table0611$X2011.Code %in% c(ag_codes, devel_codes), ]
natural_gain_rows <- table0611[table0611$X2011.Code %in% terr_natural_codes & table0611$X2006.Code %in% c(ag_codes, devel_codes), ]
no_change_rows <- table0611[table0611$X2011.Code == table0611$X2006.Code, ]

loss_codes <- natural_loss_rows$ID
gain_codes <- natural_gain_rows$ID
no_change_codes <- no_change_rows$ID
other_change_codes <- table0611$ID[!table0611$ID %in% c(loss_codes, gain_codes, no_change_codes)]

luc_lossgain <- luc0611 %>%
  mutate(change_type = case_when(category %in% loss_codes ~ 'loss',
                                 category %in% gain_codes ~ 'gain',
                                 category %in% no_change_codes ~ 'none',
                                 TRUE ~ 'other')) %>%
  group_by(route, change_type) %>%
  summarize(proportion = sum(proportion))

luc_lossgain_wide <- luc_lossgain %>%
  dcast(route ~ change_type, fill = 0) %>%
  mutate(net_gain = gain - loss)

# Express net gain using just the natural codes, not caring what it changed into or out of (can refine later)
natural_2006_codes <- table0611$ID[table0611$X2006.Code %in% terr_natural_codes]
natural_2011_codes <- table0611$ID[table0611$X2011.Code %in% terr_natural_codes]

luc_netnaturalgain <- luc0611 %>%
  mutate(natural_2006 = category %in% natural_2006_codes,
         natural_2011 = category %in% natural_2011_codes,
         lost_natural_0611 = category %in% loss_codes) %>%
  group_by(route) %>%
  summarize(natural_2006 = sum(proportion[natural_2006]),
            natural_2011 = sum(proportion[natural_2011]),
            lost_natural_0611 = sum(proportion[lost_natural_0611]))

# Join with BBS richness --------------------------------------------------

bbs010611 <- read.csv('Q:/BBS/bbs010611.csv')

# Join with FG.
bbs_fg_richness <- bbs010611 %>%
  select(-lon, -lat, -lon_aea, -lat_aea) %>%
  melt(id.vars = c('year', 'rteNo'), variable.name = 'species') %>% 
  mutate(species = gsub('\\.', ' ', species)) %>%
  left_join(birdtrait %>% select(Latin_Name_clean, FG), by = c('species' = 'Latin_Name_clean')) %>%
  group_by(year, rteNo, FG) %>%
  summarize(richness = sum(value > 0))


rich_change <- function(x) {
  vals <- sapply(c(2001, 2006, 2011), function(y) ifelse(any(x$year==y), x$richness[x$year==y], NA))
  data.frame(change0106 = vals[2] - vals[1],
              change0611 = vals[3] - vals[2],
              change0111 = vals[3] - vals[1])
}

# Express richness change as a ratio not a difference
rich_ratio <- function(x) {
  vals <- sapply(c(2001, 2006, 2011), function(y) ifelse(any(x$year==y), x$richness[x$year==y], NA))
  data.frame(change0106 = vals[2] / vals[1],
             change0611 = vals[3] / vals[2],
             change0111 = vals[3] / vals[1])
}


bbs_fg_richchange <- bbs_fg_richness %>%
  mutate(FG = if_else(is.na(FG), 'Other', FG)) %>%
  group_by(rteNo, FG) %>%
  do(rich_ratio(.))

ggplot(bbs_fg_richchange, aes(x = change0611)) +
  geom_histogram() + 
  facet_wrap(~ FG)

bbs_nlcd_change0611 <- bbs_fg_richchange %>%
  left_join(luc_lossgain_wide, by = c('rteNo' = 'route'))

ggplot(bbs_nlcd_change0611, aes(x = net_gain, y = change0611)) +
  geom_point() +
  facet_wrap(~ FG)

ggplot(bbs_nlcd_change0611, aes(x = loss, y = change0611)) +
  geom_point() +
  facet_wrap(~ FG)


# Create maps of bbs and nlcd change --------------------------------------

bbscoord <- bbs010611 %>% select(rteNo, lat, lon) %>% unique()

bbs_nlcd_change0611 <- bbs_nlcd_change0611 %>% left_join(bbscoord)

# Where are birds increasing and decreasing in abundance
bbs_nlcd_change0611 %>%
  filter(!is.na(change0611)) %>%
  mutate(change_factor = case_when(change0611 > 1 ~ 'gain',
                                   change0611 < 1 ~ 'loss',
                                   TRUE ~ 'none')) %>%
ggplot(aes(x=lon, y=lat, color = change_factor)) +
  borders('state') +
  geom_point() +
  scale_color_manual(values = c('gain' = 'slateblue3', 'loss' = 'goldenrod', 'none' = 'gray75')) +
  coord_map(ylim = c(25, 50), xlim = c(-126, -67)) +
  facet_wrap(~ FG, ncol = 2)

luc_lossgain_wide %>%
  left_join(bbscoord, by = c('route' = 'rteNo')) %>%
  mutate(change_factor = case_when(net_gain > 0 ~ 'gain',
                                   net_gain < 0 ~ 'loss',
                                   TRUE ~ 'none')) %>%
ggplot(aes(x=lon, y=lat, color = change_factor)) +
  borders('state') +
  geom_point() +
  scale_color_manual(values = c('gain' = 'slateblue3', 'loss' = 'goldenrod', 'none' = 'gray75')) +
  coord_map(ylim = c(25, 50), xlim = c(-126, -67))


# Mixed model with BCR as random effect -----------------------------------

library(lme4)
library(purrr)
bbs_rtemeta <- read.csv('Q:/BBS/routes.csv')

# Paste together the route number in metadata.
bbs_rtemeta <- bbs_rtemeta %>%
  mutate(rteNo = as.numeric(paste0(statenum, map_chr(3 - nchar(Route), ~ strrep("0", .x)), Route)))

bbs_nlcd_change0611 <- bbs_nlcd_change0611 %>%
  left_join(bbs_rtemeta[,c('rteNo','BCR')])

mm_changebyloss <- lmer(change0611 ~ loss + (1|FG) + (1|BCR), data = bbs_nlcd_change0611 %>% filter(is.finite(change0611)))
summary(mm_changebyloss)
confint(mm_changebyloss)
ranef(mm_changebyloss)

changemodel_by_fg <- bbs_nlcd_change0611 %>%
  filter(is.finite(change0611)) %>%
  group_by(FG) %>%
  do(mod = lmer(change0611 ~ loss + (1|BCR), data = .))

map(changemodel_by_fg$mod, summary)


# Get affinity scores by raw loss and net loss ----------------------------

bbs_affinity <- bbs_nlcd_change0611 %>%
  filter(is.finite(change0611)) %>%
  group_by(BCR, FG) %>%
  summarize(h = mean(change0611, na.rm = TRUE) / mean(1 - net_gain, na.rm = TRUE))

table(bbs_affinity$FG, bbs_affinity$h < 1)


# Get affinity scores using net gain expressed as ratio -------------------

bbs_changes <- bbs_fg_richchange %>% 
  left_join(luc_netnaturalgain, by = c('rteNo'='route')) %>%
  left_join(bbs_rtemeta[,c('rteNo','BCR')])

bbs_affinity_ratio <- bbs_changes %>%
  filter(is.finite(change0611)) %>%
  group_by(BCR, FG) %>%
  summarize(h = mean(change0611 / ((natural_2006 - lost_natural_0611)/natural_2006), na.rm = TRUE))
# Half of the taxon-ecoregion combinations increase in abundance when the route loses natural area.
