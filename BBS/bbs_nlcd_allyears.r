# Read all years of BBS and all years of NLCD, to get best estimate of affinities.

bbs <- read.csv('Q:/BBS/bbs_allyears_matrix.csv')
bbs_fgs <- read.csv('Q:/BBS/bbs_fgs.csv')

# Calculate richnesses as 10 year moving window.
# First reshape BBS to long and then join with FG.

library(dplyr)
library(reshape2)
library(forcats)
library(zoo)

# Correct functional group data
table(rowSums(table(bbs_fgs$AOU, bbs_fgs$FG) > 0)) # No species is given more than one fg, some have zero though.
bbs_fgs <- bbs_fgs[match(unique(bbs_fgs$AOU), bbs_fgs$AOU), ]
bbs_fgs$FG <- fct_explicit_na(bbs_fgs$FG, 'Other')

bbs_long <- bbs %>%
  filter(RPID == 101) %>%
  mutate(rteNo = 1000 * statenum + Route) %>%
  select(-countrynum, -statenum, -Route, -RPID) %>%
  melt(id.vars = c('rteNo','Year'), value.name = 'n', variable.name = 'AOU') %>%
  mutate(AOU = as.numeric(gsub('X', '', AOU))) %>%
  left_join(bbs_fgs)

# Quickest way to do rolling average is probably to reshape to wide format by year
# Only include 1975 and later because of the lack of sampling before that time.

# Set first year to use and width of rolling average window
year1 <- 1975
window_size <- 5

bbs_wide_year <- bbs_long %>%
  filter(n > 0, Year >= year1) %>%
  dcast(rteNo + FG + AOU ~ Year, value.var = 'n', fill = 0)

idx1 <- which(names(bbs_wide_year) == as.character(year1 + window_size - 1))
idx2 <- which(names(bbs_wide_year) == '2016')
n_yrs <- length(idx1:idx2)

bbs_rolling <- bbs_wide_year %>%
  select(-(1:3)) %>%
  as.matrix %>%
  apply(1, function(x) {
    n_obs <- as.logical(x)
    n_roll <- logical(n_yrs)
    
    for (y in 1:n_yrs) n_roll[y] <- any(n_obs[y:(y+(window_size - 1))]) 
    n_roll
  })

# Create long format bbs rolling
bbs_rolling <- cbind(bbs_wide_year[,1:3], t(bbs_rolling))
bbs_rolling_long <- melt(bbs_rolling, id.vars = c('rteNo', 'FG', 'AOU'), variable.name = 'Year', value.name = 'present') %>%
  mutate(Year = as.numeric(as.character(Year)) + year1 + (window_size - 2))

bbs_fg_rolling <- bbs_rolling_long %>%
  group_by(rteNo, FG, Year) %>%
  summarize(richness = sum(present))

bbs_fg_rolling_wide <- bbs_fg_rolling %>%
  dcast(rteNo + Year ~ FG, value.var = 'richness') %>%
  mutate(Total = Invertebrate + Omnivore + PlantSeed + VertFishScav + Waterbird + Other)



# Get rid of the ones with zero richness
bbs_fg_rolling_wide <- bbs_fg_rolling_wide %>%
  filter(Total > 0)
 
bbs_fg_rolling_wide %>% group_by(Year) %>% summarize(n()) %>% print(n=50)

# Plot some routes
library(ggplot2)
ggplot(bbs_fg_rolling_wide, aes(x = Year, y = Total, group = Year)) + geom_boxplot() # Fairly stable.

set.seed(11)
rtes <- sample(unique(bbs_fg_rolling_wide$rteNo), 10)
x <- filter(bbs_fg_rolling_wide, rteNo %in% rtes)

ggplot(x, aes(x=Year,y=Invertebrate,group=factor(rteNo),color=factor(rteNo))) + geom_line()


# Connect with NLCD -------------------------------------------------------

nlcdold <- read.csv('Q:/NLCD/corrected_nlcd_bcr_old.csv')
nlcd11 <- read.csv('Q:/NLCD/corrected_nlcd_bcr_11.csv')

# Use rolling average from 1985 and rolling average from 2011
# Calculate natural percentage in old NLCD and in 2011 NLCD

bbs_rtemeta <- read.csv('Q:/BBS/routes.csv')
bbs_rtemeta <- bbs_rtemeta %>%
  mutate(rteNo = 1000 * statenum + Route)
bbs_fg_rolling_wide <- right_join(bbs_rtemeta[,c('rteNo','BCR')], bbs_fg_rolling_wide)

old_dict <- data.frame(l1 = 1:9, type = c('developed', 'ag', 'natural', 'natural', 'water', 'natural', 'natural', 'natural', 'water'))
new_dict <- data.frame(l1 = 1:9, type = c('water', 'developed', 'natural', 'natural', 'natural', NA, 'natural', 'ag', 'natural'))

nlcdold <- nlcdold %>%
  mutate(area = Freq * (30/1000)^2,
         l1 = floor(category/10)) %>%
  left_join(old_dict)

nlcdold_bybcr <- nlcdold %>%
  group_by(BCR, BCRNAME, type) %>%
  summarize(area = sum(area))

nlcdnew <- nlcd11 %>%
  mutate(area = Freq * (30/1000)^2,
         l1 = floor(category/10)) %>%
  left_join(new_dict)

nlcdnew_bybcr <- nlcdnew %>%
  group_by(BCR, BCRNAME, type) %>%
  summarize(area = sum(area))

nlcd_by_bcr <- rbind(data.frame(nlcdold_bybcr, year = 1985),
                     data.frame(nlcdnew_bybcr, year = 2011))

# The numbers below seem wrong. It is possible that categories are not lined up correctly.
nlcd_by_bcr %>%
  mutate(year = paste('area', year, sep = '_')) %>%
  dcast(BCR + BCRNAME + type ~ year, value.var = 'area') %>% 
  filter(type == 'ag')
