# Load and inspect USDA ERS ARMS data

library(data.table)
library(tidyverse)
a2017 <- fread('/nfs/fwe-data/ERS/arms_2017.csv')

# Only farm finances survey is included.
# Balance sheet, income statement, structural characteristics of farms.'

farmstruc <- a2017 %>% filter(report %in% 'Structural Characteristics')

table(farmstruc$variable)

a2017 %>% group_by(report) %>% do(vars = unique(.$variable)) %>% pull(vars)

table(a2017$state)


# Farm income and wealth stats --------------------------------------------

library(data.table)
library(tidyverse)

fiws <- fread('/nfs/fwe-data/ERS/FarmIncome_WealthStatisticsData_November2018.csv')

# Look at FIWS data
table(fiws$VariableDescriptionPart1)
table(fiws$VariableDescriptionPart2)
table(fiws$artificialKey)

fiws_vars <- unique(fiws %>% select(contains('Variable')))
View(fiws_vars)


# Tabulate

bycropsum <- fiws %>%
  filter(grepl('Cash receipts value', VariableDescriptionTotal)) %>%
  group_by(Year, State, VariableDescriptionTotal, VariableDescriptionPart1, VariableDescriptionPart2) %>%
  summarize(Amount = sum(Amount))

# Get unique variables from summed data
cropvars <- unique(bycropsum[,3:5])

# New tabulation: if there is an "All" in the group, use its value otherwise sum them.
all_or_sum <- function(variable, amt) {
  all_idx <- variable %in% 'All'
  ifelse(any(all_idx), amt[all_idx], sum(amt))
}

bycropsum <- fiws %>%
  filter(grepl('Cash receipts value', VariableDescriptionTotal), Year < 2018) %>%
  group_by(Year, State, VariableDescriptionPart1) %>%
  summarize(Amount = all_or_sum(VariableDescriptionPart2, Amount))

# Labor expenses only
laborexp <- fiws %>%
  filter(VariableDescriptionPart1 %in% 'Labor expenses') %>%
  select(Year, State, VariableDescriptionTotal, VariableDescriptionPart1, VariableDescriptionPart2, Amount)

# All expenses
expense_vars <- c('Capital consumption', 'Capital expenditures', 'Interest expenses', 'Intermediate product expenses', 'Labor expenses', 'Net rent to landowners', 'Production expenses', 'Property taxes')

allexp <- fiws %>%
  filter(VariableDescriptionPart1 %in% expense_vars) %>%
  select(Year, State, VariableDescriptionTotal, VariableDescriptionPart1, VariableDescriptionPart2, Amount)

table(allexp$Year) # Good data coverage from 1949-2017.
allexp <- allexp %>% filter(between(Year, 1949, 2017))

# Write to CSV
write.csv(bycropsum, '/nfs/fwe-data/ERS/fiws_cash_receipts_by_crop.csv', row.names = FALSE)
write.csv(laborexp, '/nfs/fwe-data/ERS/fiws_labor_expenses_by_state.csv', row.names = FALSE)
write.csv(allexp, '/nfs/fwe-data/ERS/fiws_all_expenses_by_state.csv', row.names = FALSE)

# Create visualizations of income data

library(urbnmapr)

labor2017 <- laborexp %>%
  filter(Year == 2017, !State %in% 'US', VariableDescriptionPart2 %in% 'All') %>%
  mutate(type = if_else(grepl('cash',VariableDescriptionTotal), 'cash','contract_and_hired')) %>%
  select(State, type, Amount) 

statepop <- read.csv('/nfs/fwe-data/Census/co-est2017-alldata.csv') %>%
  filter(SUMLEV==40) %>%
  select(STNAME, POPESTIMATE2017) %>%
  mutate(State = state.abb[match(STNAME, state.name)]) %>%
  mutate(State = if_else(is.na(State), 'DC',State))

labor2017 <- labor2017 %>% 
  left_join(statepop) %>%
  mutate(Per_capita_expense = Amount/POPESTIMATE2017)

urbnmapr::states %>%
  rename(State=state_abbv) %>%
  left_join(labor2017) %>%
  filter(!is.na(type)) %>%
ggplot() + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Amount)) +
  facet_wrap(~ type) +
  scale_fill_continuous(trans = 'log', breaks = c(100e3, 1000e3)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

urbnmapr::states %>%
  rename(State=state_abbv) %>%
  left_join(labor2017) %>%
  filter(!is.na(type)) %>%
  ggplot() + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = Per_capita_expense)) +
  facet_wrap(~ type) +
  scale_fill_viridis_c() +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)

# Categories that are sums of other more refined groups
agg_groups <- c('Crops','All Commodities', 'Animals and products', 'Meat animals', 'All other crops', 'Poultry/Eggs', 'Feed crops', 'Oil crops', 'Vegetables and melons', 'Food grains', 'Greenhouse/Nursery','Fruits/Nuts')

# What are the crops with highest revenue?
bycropgrandtotal <- bycropsum %>%
  filter(Year >= 2000) %>%
  rename(Crop = VariableDescriptionPart1) %>%
  filter(State %in% 'US', !Crop %in% agg_groups) %>%
  group_by(Crop) %>%
  summarize(Amount = mean(Amount))

bycropgrandtotal %>% arrange(-Amount) %>% print(n=nrow(.))
topten <- bycropgrandtotal %>% arrange(-Amount) %>% pull(Crop) %>% `[`(1:10)

# Time series of cash receipts for a number of different crops for different states


bycroptopten <- bycropsum %>%
  rename(Crop = VariableDescriptionPart1) %>%
  filter(Crop %in% topten) %>%
  spread(Crop, Amount) %>%
  gather(Crop, Amount, -Year, -State) %>%
  mutate(Crop = factor(Crop, levels = topten)) %>%
  spread(State, Amount) %>%
  gather(State, Amount, -Year, -Crop)

statetopten <- urbnmapr::states %>%
  rename(State=state_abbv) %>%
  left_join(bycroptopten)

toptentotals <- bycroptopten %>%
  group_by(Year, Crop) %>%
  summarize(Grand_Total = signif(sum(Amount, na.rm = TRUE)*1000, 2)/1e9)

maptheme <- theme_bw() + theme(strip.background = element_rect(fill = NA),
                               axis.text = element_blank(),
                               axis.title = element_blank(),
                               axis.ticks = element_blank())

ggplot(statetopten %>% filter(Year==2017)) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Amount)) +
  geom_text(aes(label = paste0('$',Grand_Total,'B')), x = -120, y = 20, data = toptentotals %>% filter(Year == 2017)) +
  facet_wrap(~ Crop) +
  scale_fill_viridis_c(name = 'Revenue', trans = 'log', breaks = c(1e3, 10e3, 100e3, 1000e3, 10000e3), labels = c('$1M', '$10M', '$100M', '$1B', '$10B')) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  maptheme

plot_years <- seq(from=1927,to=2017,by=5)

map_list <- purrr::map(plot_years, function(x) ggplot(statetopten %>% filter(Year==x)) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Amount)) +
  geom_text(aes(label = paste0('$',Grand_Total,'B')), x = -120, y = 20, data = toptentotals %>% filter(Year == x)) +
  facet_wrap(~ Crop) +
  scale_fill_viridis_c(name = 'Revenue', trans = 'log', breaks = c(1e3, 10e3, 100e3, 1000e3, 10000e3), labels = c('$1M', '$10M', '$100M', '$1B', '$10B'), limits = c(100, 13400e3)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  maptheme +
  ggtitle(x)
)

fp <- '/nfs/qread-data/figures'
walk2(map_list, plot_years, ~ ggsave(file.path(fp, paste0('croprevenue',.y,'.png')), .x, height=5, width=9, dpi=100))

library(magick)
file.path(fp, paste0('croprevenue', plot_years, '.png')) %>% 
  purrr::map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write(file.path(fp, 'croprevenue1927to2017.gif')) # write to current dir
