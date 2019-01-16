# Read NASS data
# Downloaded from query on 14 Jan 2019 (QDR)

# survey -- crops -- all -- county level -- 2018

crop2018 <- read.csv('Q:/USDAcropland/NASSqueries/croptotals2018.csv', stringsAsFactors = FALSE)

# survey -- crops -- all -- county level -- 2017 annual

# Have all survey data for all crops at the county level and state level for all years 2012-2018
files <- dir('Q:/USDAcropland/NASSqueries', full.names = TRUE)

library(dplyr)
library(purrr)
library(tidyr)

countycrop <- map_dfr(grep('county', files, value = TRUE), read.csv, stringsAsFactors = FALSE)
statecrop <- map_dfr(grep('state', files, value = TRUE), read.csv, stringsAsFactors = FALSE)

# To clean data
# Split Data.Item into crop and variable
# Convert Value to numeric, removing commas
# Suppressed values are listed as (D): https://www.nass.usda.gov/Publications/AgCensus/2012/Full_Report/Volume_1,_Chapter_1_US/usappxa.pdf

countycrop <- countycrop %>%
  separate(Data.Item, into = c('Crop','Variable'), sep = ' - ') %>%
  mutate(Value = as.numeric(gsub(',', '', Value)))
statecrop <- statecrop %>%
  separate(Data.Item, into = c('Crop','Variable'), sep = ' - ') %>%
  mutate(Value = as.numeric(gsub(',', '', Value)))

# Separate Variable into variable and unit
countycrop <- countycrop %>%
  separate(Variable, into = c('Variable','Unit'), sep = ', MEASURED IN ') %>%
  mutate(Unit = if_else(grepl('ACRES', Variable), 'ACRES', Unit))
statecrop <- statecrop %>%
  separate(Variable, into = c('Variable','Unit'), sep = ', MEASURED IN ') %>%
  mutate(Unit = if_else(grepl('ACRES', Variable), 'ACRES', Unit))

# Get only acreage variables
county_acreage <- countycrop %>%
  filter(grepl('ACRES', Variable)) %>%
  select(Year, State, Ag.District, County, Commodity, Crop, Variable, Value) %>%
  mutate(Variable = gsub(' ', '_', Variable)) %>%
  group_by(Year, State, Ag.District, County, Commodity, Crop, Variable) %>%
  spread(Variable, Value)
state_acreage <- statecrop %>%
  filter(grepl('ACRES', Variable), Period == 'YEAR', Domain == 'TOTAL') %>%
  select(Year, State, Commodity, Crop, Variable, Value) %>%
  mutate(Variable = gsub(' ', '_', Variable)) %>%
  group_by(Year, State, Commodity, Crop, Variable) %>%
  spread(Variable, Value)

# Look by state at the percentages of acres planted that are not harvested
state_acreage <- state_acreage %>%
  mutate(ACRES_HARVESTED = if_else(ACRES_HARVESTED > ACRES_PLANTED, ACRES_PLANTED, ACRES_HARVESTED),
         ACRES_UNHARVESTED = ACRES_PLANTED - ACRES_HARVESTED,
         percent_unharvested = 1 - ACRES_HARVESTED/ACRES_PLANTED)

# Make some figures
library(ggplot2)

unharvested_bycommodity <- state_acreage %>% 
  group_by(Year, Commodity) %>%
  summarize(total_unharvested = sum(ACRES_UNHARVESTED, na.rm = TRUE),
            total_planted = sum(ACRES_PLANTED, na.rm = TRUE),
            pct_unharvested = total_unharvested/total_planted) %>%
  filter(total_planted > 0)

ggplot(unharvested_bycommodity, aes(x = Year, y = pct_unharvested, group = Commodity)) +
  geom_line() + geom_point()

# Make a table
# Mean percent of field acreage left unharvested by crop, 2012-2018
mean_unharvested <- unharvested_bycommodity %>% group_by(Commodity) %>% summarize(pct = round(mean(pct_unharvested * 100), 1)) %>% arrange(pct)

write.csv(mean_unharvested, file = '~/Dropbox/projects/foodwaste/Results/percent_unharvested.csv', row.names = FALSE)
