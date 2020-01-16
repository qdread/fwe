# Load BBS presence absence by stop, and BBS NLCD landcover 2016 by stop, to find presence absence of species by land cover type

fp <- ifelse(dir.exists('Q:/'), 'Q:/raw_data', '/nfs/qread-data/raw_data')

bbsnlcd <- read.csv(file.path(fp, 'BBS/bbs_stop_coords_nlcd.csv'), stringsAsFactors = FALSE)
load(file.path(fp, 'BBS/bbsworkspace_bystop_20072016.r'))

bbscov_join <- bbscov_oneyear %>%
  left_join(bbsnlcd %>% select(rteNo, Stop, NLCD))
# After joining, about 15K or 300 routes' worth do not have NLCD pixels.

# For each species across all locations, calculate the total number of NLCD pixels of each group in which it was observed.
table_by_species <- apply(bbsmat_oneyear, 2, function(x) table(bbscov_join$NLCD[x > 0]))

# Instead, calculate its probability of presence in each pixel class
bbslong <- bbsmat_oneyear %>% 
  as.data.frame %>%
  cbind(bbscov_join %>% select(rteNo, Stop, NLCD)) %>%
  gather(species, presence, -rteNo, -Stop, -NLCD)

bbs_presence_absence_NLCD <- bbslong %>%
  group_by(species, NLCD) %>%
  summarize(yes = sum(presence == 1),
            no = sum(presence == 0))

bbs_presence_absence_NLCD <- bbs_presence_absence_NLCD %>%
  mutate(prob = yes/(yes+no)) %>%
  group_by(species) %>%
  mutate(affinity = prob/max(prob))

# What proportions of species are most commonly found in each type of habitat
bbs_specialized <- bbs_presence_absence_NLCD %>%
  ungroup %>%
  filter(affinity == 1) 

table(bbs_specialized$NLCD)

# Which are specialized in highly urbanized areas
bbs_specialized %>% filter(NLCD == 24) %>% pull(species)
