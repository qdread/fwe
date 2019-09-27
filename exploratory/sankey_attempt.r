# Create Sankey diagram (test)

library(ggforce)

titanic <- reshape2::melt(Titanic)

titanic <- gather_set_data(titanic, 1:4)

ggplot(titanic, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(aes(fill = Sex), alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')

# try various kinds of Sankey
s1 <- read.csv('~/Documents/R/sankeydat.csv')

ggplot(gather_set_data(s1, 1:2), aes(x, id = id, split = y, value = used)) +
  geom_parallel_sets(alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')

s2 <- read.csv('~/Documents/R/sankeydatv2.csv')
s2gather <- gather_set_data(s2, 1:5) %>% mutate(x = factor(x, levels = c('stage1','stage2','stage3','stage4','destination')))

ggplot(s2gather, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')

s3 <- read.csv('~/Documents/R/sankeydatv3.csv')
s3gather <- gather_set_data(s3, 1:5) %>% mutate(x = factor(x, levels = c('stage1','stage2','stage3','stage4','destination')))

ggplot(s3gather, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')

s4 <- read.csv('~/Documents/R/sankeydatv4.csv')
s4gather <- gather_set_data(s4, 1:2)
ggplot(s4gather, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets()


# Example from SO ---------------------------------------------------------

# https://stackoverflow.com/questions/55083945/sankey-diagram-in-r-how-to-change-the-height-y-of-individual-sections-related


dfs <- structure(list(Hospital = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 
                                      1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 
                                      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 
                                      4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                                      4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L), .Label = c("1", 
                                                                                              "2", "3", "4", "5"), class = "factor"), Paciente = structure(c(1L, 
                                                                                                                                                             1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 
                                                                                                                                                             3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 
                                                                                                                                                             5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 
                                                                                                                                                             6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L
                                                                                              ), .Label = c("21", "22", "23", "24", "25", "26", "27"), class = "factor"), 
               Terapia = structure(c(2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                                     3L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 
                                     1L, 3L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                                     3L, 2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 
                                     1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), .Label = c("Adalimumab", 
                                                                                             "Etanercept", "Infliximab", "Rituximab"), class = "factor"), 
               Unit = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), class = "data.frame", row.names = c(NA, 
                                                                                               -65L))
# transform dataframe into appropriate format
dfs2 <- gather_set_data(dfs, 1:3)

# define axis-width / sep parameters once here, to be used by
# each geom layer in the plot
aw <- 0.1
sp <- 0.1

ggplot(dfs2, 
       aes(x = x, id = id, split = y, value = Unit)) +
  geom_parallel_sets(aes(fill = Hospital), alpha = 0.3, 
                     axis.width = aw, sep = sp) +
  geom_parallel_sets_axes(axis.width = aw, sep = sp) +
  geom_parallel_sets_labels(colour = "white", 
                            angle = 0, size = 3,
                            axis.width = aw, sep = sp) +
  theme_minimal()

# Try including some NAs to see if we can "skip" one of the columns
df_edit <- dfs
df_edit$Paciente[36:37] <- NA
df_edit_set <- gather_set_data(df_edit, 1:3)

library(tidyverse)

ggplot(df_edit_set %>% filter(!is.na(y)), 
       aes(x = x, id = id, split = y, value = Unit)) +
  geom_parallel_sets(aes(fill = Hospital), alpha = 0.3, 
                     axis.width = aw, sep = sp) +
  geom_parallel_sets_axes(axis.width = aw, sep = sp) +
  geom_parallel_sets_labels(colour = "white", 
                            angle = 0, size = 3,
                            axis.width = aw, sep = sp) +
  theme_minimal()


# Use ggalluvial ----------------------------------------------------------

library(ggalluvial)

titanic_wide <- data.frame(Titanic)
head(titanic_wide)
#>   Class    Sex   Age Survived Freq
#> 1   1st   Male Child       No    0
#> 2   2nd   Male Child       No    0
#> 3   3rd   Male Child       No   35
#> 4  Crew   Male Child       No    0
#> 5   1st Female Child       No    0
#> 6   2nd Female Child       No    0
ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")

# Edit titanic_wide to include some unsexed individuals
titanic_withunsexed <- titanic_wide %>%
  add_row(Class = 'Crew', Sex = NA, Age = 'Adult', Survived = 'Yes', Freq = 100, .before = 1)
ggplot(data = titanic_withunsexed,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,
           y = Freq)) +
  scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = Survived)) +
  geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
  theme_minimal() +
  ggtitle("passengers on the maiden voyage of the Titanic",
          "stratified by demographics and survival")

# Load Sankey data for fsc
# Replace NAs
fsc_sankey <- read.csv('~/Documents/R/sankeydat_gg.csv', na.strings = c('NA', ''), stringsAsFactors = FALSE)
ggplot(fsc_sankey, aes(axis1 = stage1, axis2 = stage2, axis3 = stage3, axis4 = stage4, axis5 = destination, y = value)) +
  geom_alluvium(aes(fill = destination)) +
  geom_stratum() +
  geom_text(stat = 'stratum', label.strata = TRUE, angle = -90)

# Use same data with ggforce
fsc_sankey <- structure(list(stage1 = c("production", "production", "production", 
                                        "production", "production", "production", "production", "production", 
                                        "production"), stage2 = c(NA, "processing", "processing", "processing", 
                                                                  "processing", "processing", "processing", "processing", "processing"
                                        ), stage3 = c(NA, NA, "retail", NA, NA, NA, NA, "retail", "retail"
                                        ), stage4 = c(NA, NA, NA, "foodservice", "foodservice", "institutions", 
                                                      "institutions", "households", "households"), destination = c("waste", 
                                                                                                                   "waste", "waste", "consumed", "waste", "consumed", "waste", "consumed", 
                                                                                                                   "waste"), value = c(1L, 1L, 1L, 3L, 1L, 3L, 1L, 3L, 1L)), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                 -9L))

fsc_sankey_set <- gather_set_data(fsc_sankey, 1:5) %>%
  mutate(x = factor(x, levels = c('stage1','stage2','stage3','stage4','destination'))) 

ggplot(fsc_sankey_set, aes(x, id = id, split = y, value = value)) +
  geom_parallel_sets(alpha = 0.3, axis.width = 0.1) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels(colour = 'white')
