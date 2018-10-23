# Total net LUC by BCR. Connect this with BBS average richness change by BCR

bcr_nlcd_0611 <- read.csv('Q:/NLCD/bcr_nlcdchange0611_corrected.csv')
bcr_nlcd_0106 <- read.csv('Q:/NLCD/bcr_nlcdchange0106.csv')

# Get change codes and reshape data.
table0611 <- read.csv('Q:/NLCD/nlcd_change0611_levels.csv')

library(dplyr)
library(reshape2)
library(purrr)

# These two BCRs barely enter into the continental USA so get rid
exclude_bcrs <- c(6, 39)

bcr_nlcd_0611_long <- bcr_nlcd_0611 %>% 
  filter(!BCR %in% exclude_bcrs) %>%
  melt(id.vars = 1:2, value.name = 'count', variable.name = 'class') %>%
  mutate(class = as.numeric(gsub('V', '', class)) - 1,
         area = count * .03^2) %>%
  group_by(BCR, BCRNAME) %>%
  mutate(proportion = area/sum(area)) %>%
  ungroup

bcr_nlcd_0611_long <- bcr_nlcd_0611 %>% 
  rename(class = Var1) %>%
  filter(!BCR %in% exclude_bcrs) %>%
  mutate(area = Freq * .03^2) %>%
  group_by(BCR, BCRNAME) %>%
  mutate(proportion = area/sum(area)) %>%
  ungroup

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

natural_2006_codes <- table0611$ID[table0611$X2006.Code %in% terr_natural_codes]
natural_2011_codes <- table0611$ID[table0611$X2011.Code %in% terr_natural_codes]
unnatural_2006_codes <- table0611$ID[table0611$X2006.Code %in% c(ag_codes, devel_codes)]
unnatural_2011_codes <- table0611$ID[table0611$X2011.Code %in% c(ag_codes, devel_codes)]

bcr_0611_lostnatural <- bcr_nlcd_0611_long %>%
  mutate(natural_2006 = class %in% natural_2006_codes,
         natural_2011 = class %in% natural_2011_codes,
         lost_natural_0611 = class %in% loss_codes) %>%
  group_by(BCR, BCRNAME) %>%
  summarize(natural_2006 = sum(proportion[natural_2006]),
            natural_2011 = sum(proportion[natural_2011]),
            lost_natural_0611 = sum(proportion[lost_natural_0611]))

bcr_0611_lossgain <- bcr_nlcd_0611_long %>%
  mutate(change_type = case_when(class %in% loss_codes ~ 'loss',
                                 class %in% gain_codes ~ 'gain',
                                 class %in% no_change_codes ~ 'none',
                                 TRUE ~ 'other')) %>%
  group_by(BCR, BCRNAME, change_type) %>%
  summarize(proportion = sum(proportion))

bcr_0611_nat_unnat <- bcr_nlcd_0611_long %>%
  mutate(natural_2006 = class %in% natural_2006_codes,
         natural_2011 = class %in% natural_2011_codes,
         unnatural_2006 = class %in% unnatural_2006_codes,
         unnatural_2011 = class %in% unnatural_2011_codes) %>%
  group_by(BCR, BCRNAME) %>%
  summarize(natural_2006 = sum(area[natural_2006]),
            natural_2011 = sum(area[natural_2011]),
            unnatural_2006 = sum(area[unnatural_2006]),
            unnatural_2011 = sum(area[unnatural_2011]))

# Loss of species by BCR.
bbs010611 <- read.csv('Q:/BBS/bbs010611.csv')

# Join with FG.
birdtrait <- read.csv('Q:/BBS/birdtraitmerged.csv', stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA

# Use 5 category classification. 
# Exclude nocturnal and the misc rare guilds like fruit and nectar eaters.
birdtrait <- birdtrait %>%
  mutate(FG = Diet.5Cat,
         FG = if_else(ForStrat.watbelowsurf > 0 | ForStrat.wataroundsurf > 0 | PelagicSpecialist == 1, 'Waterbird', FG),
         FG = if_else(Nocturnal == 1 | is.na(Nocturnal) | FG == 'FruiNect' | Diet.5Cat == '1', as.character(NA), FG)
  )


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

# Join mean richness change by BCR, with mean loss of natural area by BCR.
bbs_rtemeta <- read.csv('Q:/BBS/routes.csv')

# Paste together the route number in metadata.
bbs_rtemeta <- bbs_rtemeta %>%
  mutate(rteNo = as.numeric(paste0(statenum, map_chr(3 - nchar(Route), ~ strrep("0", .x)), Route)))

bbs_fg_richchange <- bbs_fg_richchange %>%
  left_join(bbs_rtemeta[,c('rteNo','BCR')])

bbs_fg_change_bcr <- bbs_fg_richchange %>%
  group_by(BCR, FG) %>%
  summarize(richness_change = mean(change0611, na.rm = TRUE)) %>%
  mutate(richness_change = if_else(is.finite(richness_change), richness_change, as.numeric(NA)))

bbs_fg_change_bcr <- bbs_fg_change_bcr %>% left_join(bcr_0611_lostnatural)

bbs_affinity_ratio <- bbs_fg_change_bcr %>%
  filter(is.finite(richness_change)) %>%
  mutate(h = richness_change / (natural_2011/natural_2006))

library(ggplot2)
ggplot(bbs_fg_change_bcr, aes(x = natural_2011/natural_2006, y = richness_change)) +
  geom_point() + facet_wrap(~ FG, scales = 'free_y')



####
# Instead of change by BCR just get the values for 2001, 2006, and 2011 by BCR
bcr_fg_richness <- bbs_fg_richness %>%
  left_join(bbs_rtemeta[ ,c('rteNo', 'BCR')]) %>%
  ungroup %>%
  mutate(year = paste0('richness_', year)) %>%
  group_by(BCR, FG, year) %>%
  summarize(richness = mean(richness, na.rm = TRUE)) %>%
  dcast(BCR + FG ~ year)

# Also try to get the total richness per BCR.
bbs_rtemeta %>%
  select(rteNo, BCR)

h <- function(Slost, Sorig, z, Anew, Aorig, Aag) (((1 - Slost/Sorig) ^ (1/z)) - (Anew/Aorig)) * (Aorig/Aag)
h <- function(Slost, Sorig, z, Anew, Aorig, Aag) ( Aorig * (1 - Slost/Sorig)^(1/z) - Anew) * (1/Aag)
  
bcr_fg_land <- bcr_0611_nat_unnat %>% ungroup %>% left_join(bcr_fg_richness) %>%
  mutate(h = h(Slost = richness_2006-richness_2011, Sorig=richness_2006, z=0.25, Anew=natural_2011, Aorig=natural_2006, Aag=natural_2006-natural_2011))
