# Old BBS - create matrix so we can see change in richness with land use over a very long time.
# This also has new BBS in it, going up to 2016, in the same format!
# QDR/FWE/19 Oct 2018

# Modified 11 Dec: the manual correction is now removed, no longer necessary

fp <- '/nfs/qread-data/BBS/States'
state_files <- dir(fp, pattern = '*.csv', full.names = TRUE)
fpnew <- '/nfs/qread-data/BBS/Post1997'

library(purrr)
library(dplyr)
library(reshape2)
oldbbs <- map_dfr(state_files, read.csv) # Read all data, ~350MB, takes a little time.


# "Fix" the bbs data using the same methods as I did for the newer data.
bbsspp <- read.csv('/nfs/qread-data/BBS/bbs_species_lookup_table_modified.csv', stringsAsFactors = FALSE)
oldbbs_aous <- unique(oldbbs$Aou)

table(oldbbs_aous %in% bbsspp$AOU)
oldbbs_aous[!oldbbs_aous %in% bbsspp$AOU] # 4812 and 4813 # These appear to be scrub jays. 
# bbsspp$AOU_list[bbsspp$AOU == 4811] <- '4812 4813' # This connection already done manually

# This time let's do the following:
# (1) Anything that is a hybrid, get rid of. (7) This totals a vanishingly tiny percent of the total. (0.0002%)
# (2) Anything that is a subspecies assign to parent species (15)
# (3) Anything that is an unknown species within a genus, or an unknown genus, get rid of. They total 0.1% of individuals.

unkaou <- bbsspp$AOU[bbsspp$Type %in% c('unknown_genus', 'unknown_sp')]
unktotal <- sum(oldbbs$SpeciesTotal[oldbbs$Aou %in% unkaou])
unktotal/sum(oldbbs$SpeciesTotal)
hybridaou <- bbsspp$AOU[bbsspp$Type %in% c('hybrid')]
sum(oldbbs$SpeciesTotal[oldbbs$Aou %in% hybridaou])/sum(oldbbs$SpeciesTotal)

# Get rid of the ones that need to be gotten rid of.
aous_exclude <- c(unkaou, hybridaou)
oldbbs_cleaned <- oldbbs %>% filter(!Aou %in% aous_exclude)

# Create a mapping of species to their parent taxa. 
sspaou <- bbsspp$AOU[bbsspp$Type %in% 'subspecies']
parentaou <- as.numeric(bbsspp$AOU_list[bbsspp$Type %in% 'subspecies'])

# Must include the scrub jays still as a manual correction.
sspaou <- c(sspaou, 4812, 4813)
parentaou <- c(parentaou, 4811, 4811)

# Replace subspecies AOUs with the one from their parent species
oldbbs_cleaned$Aou[oldbbs_cleaned$Aou %in% sspaou] <- na.omit(parentaou[match(oldbbs_cleaned$Aou, sspaou)])

sppids <- sort(unique(oldbbs_cleaned$Aou)) # 649 final species.

# Ensure that we have a functional guild identified for all the species.
birdtrait <- read.csv('/nfs/qread-data/BBS/birdtraitmerged.csv', stringsAsFactors = FALSE)

table(sppids %in% birdtrait$AOU) # 4 are missing
sppids[!sppids %in% birdtrait$AOU]

# Create a final list of functional guilds using the trait matrix plus adding the couple ones not there.
birdtrait <- birdtrait %>%
  mutate(FG = Diet.5Cat,
         FG = if_else(ForStrat.watbelowsurf > 0 | ForStrat.wataroundsurf > 0 | PelagicSpecialist == 1, 'Waterbird', FG),
         FG = if_else(Nocturnal == 1 | is.na(Nocturnal) | FG == 'FruiNect' | Diet.5Cat == '1', as.character(NA), FG)
  )

bird_fg <- birdtrait[,c('AOU','FG')]
missingAOUs <- c(4172, 16600)
closest_to_missingAOUs <- c(4171, 7180)
bird_fg <- rbind(bird_fg, data.frame(AOU=missingAOUs, FG=bird_fg$FG[match(closest_to_missingAOUs, bird_fg$AOU)]))

bird_fg <- unique(bird_fg)

# Exclude the nocturnal birds.
nocturnalaou <- na.omit(unique(birdtrait$AOU[birdtrait$Nocturnal == 1]))
oldbbs_cleaned <- oldbbs_cleaned %>% filter(!Aou %in% nocturnalaou)

bird_fg <- filter(bird_fg, AOU %in% oldbbs_cleaned$Aou) %>% arrange(AOU)

# Reshape to a presence-only, by-route matrix.
# Must sum totals because a few rows have duplicated AOUs now due to the subspecies.
oldbbs_matrix <- oldbbs_cleaned %>%
  filter(RPID == 101) %>%
  dcast(countrynum + statenum + Route + RPID + Year ~ Aou, 
        value.var = 'SpeciesTotal', 
        fun.aggregate = sum,
        fill = 0)

write.csv(oldbbs_matrix, file = '/nfs/qread-data/BBS/bbs_allyears_matrix.csv', row.names = FALSE)
write.csv(bird_fg, file = '/nfs/qread-data/BBS/bbs_fgs.csv', row.names = FALSE)
