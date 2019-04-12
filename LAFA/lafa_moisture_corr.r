# LAFA moisture corrections

# Fruit:
# 1. All dried fruit includes water loss in primary loss. Multiply all by (1 - %water if wet)/(1 - %water if dry) to get dry weight and renormalize
# 2. All caned, frozen, dried, and juice fruit includes inedible part loss in primary loss. Create primary unavoidable loss for this.

# Veg:
# 1. Dried potato and onion include water loss in primary loss. Do same correction for fruit.
# 2. Canned and frozen vegetables include inedible loss in primary loss. Same as for fruit.

# Meat:
# 1. All meat includes bone loss in primary loss. Create primary unavoidable loss for this.

fp_crosswalks <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'crossreference_tables')
ndbCW <- read.csv(file.path(fp_crosswalks, 'ndb_lafa_crosswalk.csv'), stringsAsFactors = FALSE)
lafaloss <- read.csv(file.path(fp_crosswalks, 'lafa_weights_by_stage.csv'), stringsAsFactors = FALSE)

# Fruit and vegetable water content correction
fresh_fruit_names <- lafaloss %>%
  filter(Food_Group == 'fruit', Prep_type == 'fresh') %>%
  select(Food_type, Prep_type, Commodity)
dried_fruit_names <- lafaloss %>%
  filter(Food_Group == 'fruit', Prep_type == 'dried') %>%
  select(Food_type, Prep_type, Commodity)

ndb_freshfruit <- ndbCW %>% filter(LAFA.equivalent %in% fresh_fruit_names$Commodity, Cooked == 'n') %>%
  group_by(LAFA.equivalent) %>%
  summarize(n = n(), watercontent = mean(Water.g.Per.100.g))

ndb_driedfruit <- ndbCW %>% filter(LAFA.equivalent %in% dried_fruit_names$Commodity, Cooked == 'n') %>%
  filter(!grepl('moisture', Description)) %>% # Get rid of ultra low moisture ones
  group_by(LAFA.equivalent) %>%
  summarize(n = n(), watercontent = mean(Water.g.Per.100.g))

fresh_veg_names <- lafaloss %>%
  filter(Food_Group == 'veg', Prep_type == 'fresh') %>%
  select(Food_type, Prep_type, Commodity)
dried_veg_names <- lafaloss %>%
  filter(Food_Group == 'veg', Prep_type == 'dried') %>%
  select(Food_type, Prep_type, Commodity)

ndb_freshveg <- ndbCW %>% filter(LAFA.equivalent %in% fresh_veg_names$Commodity, Cooked == 'n') %>%
  group_by(LAFA.equivalent) %>%
  summarize(n = n(), watercontent = mean(Water.g.Per.100.g))

ndb_driedveg <- ndbCW %>% filter(LAFA.equivalent %in% dried_veg_names$Commodity, Cooked == 'n') %>%
  group_by(LAFA.equivalent) %>%
  summarize(n = n(), watercontent = mean(Water.g.Per.100.g)) 

fruit_WC <- rbind(ndb_freshfruit, ndb_driedfruit) %>%
  rename(Commodity = LAFA.equivalent) %>%
  right_join(rbind(fresh_fruit_names, dried_fruit_names))

fruit_WC_wide <- fruit_WC %>%
  select(-Commodity, -n) %>%
  group_by(Food_type) %>%
  spread(Prep_type, watercontent) %>%
  filter(!is.na(dried), !is.na(fresh)) %>%
  mutate(correction_factor = (100 - fresh)/(100 - dried))

veg_WC <- rbind(ndb_freshveg, ndb_driedveg) %>%
  rename(Commodity = LAFA.equivalent) %>%
  right_join(rbind(fresh_veg_names, dried_veg_names))

veg_WC_wide <- veg_WC %>%
  select(-Commodity, -n) %>%
  group_by(Food_type) %>%
  spread(Prep_type, watercontent) %>%
  filter(!is.na(dried), !is.na(fresh)) %>%
  mutate(correction_factor = (100 - fresh)/(100 - dried))

dried_corr_factors <- rbind(fruit_WC_wide, veg_WC_wide[-1,]) %>%
  select(Food_type, correction_factor) %>%
  left_join(rbind(dried_fruit_names,dried_veg_names))

lafaloss <- lafaloss %>%
  left_join(dried_corr_factors)

lafatocorrect <- lafaloss %>% filter(!is.na(correction_factor)) %>%
  mutate(Primary_weight_corr = pmax(Retail_weight, Primary_weight * correction_factor),
         Primary_loss_corr = 100 * (1 - Retail_weight/Primary_weight_corr))
