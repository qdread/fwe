# Example with packaged meat
x <- '31161A'
x <- '311230' # cereal
x <- '1111A0'

# Either a dish where the entire thing is packaged meat, or has some ingredients in it.
fcidingrlong <- fcidingredientsCW %>%
  gather(stage, code, -FCID_Code, -FCID_Desc) %>%
  group_by(FCID_Code) %>%
  summarize(pkgmeat = x %in% code)

pkgmeat_codes <- fcidingrlong$FCID_Code[fcidingrlong$pkgmeat]

# recipes containing packaged meat in any capacity

recipes_withpkgmeat <- recipes %>%
  filter(Mod_Code == 0) %>%
  mutate(pkgmeat = FCID_Code %in% pkgmeat_codes) %>%
  group_by(Food_Code) %>%
  mutate(contains_meat = any(pkgmeat)) %>%
  ungroup %>%
  filter(contains_meat) 

pcts_pkgmeat <- recipes_withpkgmeat %>%
  filter(pkgmeat) %>%
  group_by(Food_Code) %>%
  summarize(pct = sum(Commodity_Weight))

hist(pcts_pkgmeat$pct) # A few are above 100 but this can be dealt with later

fcid_cnpp_pkgmeat <- fcid_cnpp %>%
  rename(Food_Code = foodcode) %>%
  left_join(pcts_pkgmeat) %>%
  filter(NAICS_wholedish_ifsoldcold %in% x | NAICS_wholedish_ifsoldhot %in% x | !is.na(pct))
  
# test weighted mean
weighted_mean_complete <- function(x, w) {
  idx <- !is.na(x) & !is.na(w)
  weighted.mean(x[idx], w[idx])
}

# Need to also weight by the total amount consumed for each of these recipes
fcid_intake <- read.csv(file.path(fp, 'food_consumption/FCID/Commodity_Intake_0510.csv'), stringsAsFactors = FALSE)
wweia_driff <- read.csv(file.path(fp, 'food_consumption/WWEIA20052010/DRIFF_0510.csv'), stringsAsFactors = FALSE)
wweia_drtot <- read.csv(file.path(fp, 'food_consumption/WWEIA20052010/DRTOT_0510.csv'), stringsAsFactors = FALSE)

# Sum up the total grams consumed of each food code, ignoring weights for the moment
wweia_sums <- wweia_driff %>%
  group_by(DRIFDCD) %>%
  summarize(g_consumed = sum(DRIGRMS, na.rm = TRUE)) %>%
  rename(Food_Code = DRIFDCD)

fcid_cnpp_pkgmeat <- fcid_cnpp_pkgmeat %>%
  left_join(wweia_sums)

priceperkg <- with(fcid_cnpp_pkgmeat, weighted_mean_complete(price_09, g_consumed * pct / 100)) * 10

1e6/priceperkg # 1 million dollars would represent about 124 tons of packaged meat.


