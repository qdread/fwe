# Characterize BBS routes as natural or modified. Use BCR as region.

library(dplyr)

bbs_nlcd <- read.csv('Q:/BBS/bbs_nlcd_1kmbuffer.csv')
bbs_eco <- read.csv('Q:/BBS/bbs_ecoregions.csv') # this does not contain all the routes.
bbs_rtemeta <- read.csv('Q:/BBS/routes.csv')

# Paste together the route number in metadata.
bbs_rtemeta <- bbs_rtemeta %>%
  mutate(rteNo = as.numeric(paste0(statenum, map_chr(3 - nchar(Route), ~ strrep("0", .x)), Route)))

table(unique(bbs_nlcd$route) %in% bbs_rtemeta$rteNo) # fine.

# Distinguish the three different non-natural categories.
non_natural <- c('crops', 'developed', 'pasture')

bbs_nat <- bbs_nlcd %>%
  mutate(natural = !category %in% non_natural) %>%
  group_by(route) %>%
  summarize(proportion = sum(proportion[natural]))

summary(bbs_nat$proportion) # Median naturalness is 77%, mean naturalness is 68%.

bcr_nat <- bbs_nat %>%
  left_join(bbs_rtemeta, by = c("route" = "rteNo")) %>%
  group_by(BCR) %>%
  summarize(median_natural = median(proportion))

load('Q:/BBS/bbsworkspace_singleyear.r') # Some routes are missing here but we will get them later.

bbs_richness <- bbscov_oneyear %>%
  cbind(n = apply(bbsmat_byroute_oneyear, 1, sum)) %>%
  left_join(bbs_nat, by = c("rteNo" = "route")) %>%
  left_join(bbs_rtemeta[, c('rteNo', 'BCR')])

library(ggplot2)
ggplot(bbs_richness, aes(x = qlogis(proportion), y = n, group = BCR)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

bbs_richness %>%
  left_join(bcr_nat) %>%
  ggplot(aes(x = proportion > median_natural, y = n)) +
  stat_summary(geom = 'pointrange')

library(lme4)
mm_rich <- lmer(n ~ qlogis(proportion) + (1|BCR), data = bbs_richness) # Does not work because some are 100% natural.

# Find the max that's less than 1
maxless1 <- max(bbs_richness$proportion[bbs_richness$proportion < 0.99999999])
bbs_richness <- bbs_richness %>%
  mutate(proportion_corrected = if_else(proportion < maxless1, proportion, maxless1))
mm_rich <- lmer(n ~ qlogis(proportion_corrected) + (1|BCR), data = bbs_richness) # Does not work because some are 100% natural.
summary(mm_rich) # Breeding birds' richness goes up in modified habitat. Not good comparison.

# Let's stratify by functional guild and see if that does anything.
bbs_traits <- read.csv('Q:/BBS/bbs_traits_imputed.csv') # This does not have the categorization.

birdtrait <- read.csv('Q:/BBS/birdtraitmerged.csv', stringsAsFactors = FALSE)

birdtrait[birdtrait == -999] <- NA

# Use 5 category classification. 
# Exclude nocturnal and the misc rare guilds like fruit and nectar eaters.
birdtrait <- birdtrait %>%
  mutate(FG = Diet.5Cat,
         FG = if_else(ForStrat.watbelowsurf > 0 | ForStrat.wataroundsurf > 0 | PelagicSpecialist == 1, 'Waterbird', FG),
         FG = if_else(Nocturnal == 1 | is.na(Nocturnal) | FG == 'FruiNect' | Diet.5Cat == '1', as.character(NA), FG)
        )

# Use AOUs to get functional group identity for each column, use that to reshape the site by species matrix, and get richness of each FG.

sp_names <- dimnames(bbsmat_byroute_oneyear)[[2]]
table(sp_names %in% birdtrait$Latin_Name_clean) # verified.

library(reshape2)

bbs_fg_richness <- bbsmat_byroute_oneyear %>%
  melt(varnames = c('rteNo', 'species')) %>% 
  mutate(rteNo = bbscov_oneyear$rteNo[rteNo]) %>%
  left_join(birdtrait %>% dplyr::select(Latin_Name_clean, FG), by = c('species' = 'Latin_Name_clean')) %>%
  group_by(rteNo, FG) %>%
  summarize(richness = sum(value))
  
# Now get FG richness by naturalness.  

bbs_fg_richness <- bbs_fg_richness %>%
  left_join(bbs_rtemeta %>% dplyr::select(rteNo, BCR)) %>%
  left_join(bcr_nat) %>%
  left_join(bbs_nat, by = c('rteNo' = 'route')) %>%
  mutate(proportion_corrected = if_else(proportion < maxless1, proportion, maxless1))

ggplot(bbs_fg_richness, aes(x = qlogis(proportion_corrected), y = richness, group = BCR)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~FG)


# Calculate affinity scores by taxon and BCR ------------------------------

affinity_score <- function(dat, z) {
  S_mod <- mean(dat$richness[dat$proportion_corrected < dat$median_natural])   # Richness in modified
  S_nat <- mean(dat$richness[dat$proportion_corrected > dat$median_natural]) # Richness in natural
  CF_loc <- 1 - S_mod / S_nat
  h <- (1 - CF_loc) ^ (1 / z)
  return(data.frame(S_mod = S_mod, S_nat = S_nat, CF_loc = CF_loc, h = h))
}

bbs_fg_affin <- bbs_fg_richness %>%
  group_by(BCR, FG) %>%
  do(affinity_score(., z = 0.27))

ggplot(bbs_fg_affin, aes(group = FG, x = h <= 1)) + geom_bar()
