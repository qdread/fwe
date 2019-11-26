# Using Kansas and Nebraska as examples, see whether the land estimates in USEEIO are larger than the land area of the state.

emp_long <- employees_industry_x_state %>%
  select(BEA_Code, contains('US')) %>%
  pivot_longer(-BEA_Code, names_to = 'state', values_to = 'n')

emp_sum <- emp_long %>%
  group_by(BEA_Code) %>%
  mutate(n_total = sum(n),
         prop = n/n_total)

emp_sum %>% filter(state %in% c('US_KS', 'US_NE'))
