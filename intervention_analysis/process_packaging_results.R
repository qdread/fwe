# Put results from packaging into a "standardized" form that is similar to the other ones.

# This is by food type and should be kept separate
# eeio_packaging_averted

eeio_packaging_averted_total <- eeio_packaging_averted %>%
  group_by(category) %>%
  summarize_if(is.numeric, sum)

# This includes some irrelevant categories of packaging material
# eeio_packaging_offsetting_impacts

eeio_packaging_result <- eeio_packaging_averted_total %>%
  left_join(packaging_annual_offset %>% ungroup %>% select(category, contains('offset'))) %>%
  mutate(net_averted_lower = averted_lower - offset_upper,
         net_averted_mean = (averted_lower + averted_upper) / 2 - offset_mean,
         net_averted_upper = averted_upper - offset_lower)

# Add costs
eeio_packaging_result <- eeio_packaging_result %>%
  cbind(packaging_total_costs %>% setNames(paste0('total_cost_', names(.)))) %>%
  mutate(cost_per_reduction_lower = total_cost_low / net_averted_upper,
         cost_per_reduction_mean = total_cost_mean / net_averted_mean,
         cost_per_reduction_upper = total_cost_high / net_averted_lower)

write_csv(eeio_packaging_result, file.path(fp, 'scenario_results/interventions/eeio_packaging_all.csv'))
