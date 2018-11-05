# Radar chart

library(ggradar)
library(scales)

scen_results_longer %>%
  group_by(scenario, impact_category) %>%
  summarize(impact = sum(impact_total)) %>%
  dcast(scenario ~ impact_category) %>%
  rename(group = scenario) %>%
  mutate_if(is.double, funs(./max(.))) ->
  scen_grp

ggradar(scen_grp)
