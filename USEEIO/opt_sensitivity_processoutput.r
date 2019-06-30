# Process output of sensitivity analysis of optimization
# QDR / FWE / 27 June 2019

library(tidyverse)
fp_output <- file.path(ifelse(dir.exists('Q:/'), 'Q:', '/nfs/qread-data'), 'scenario_results')
n_tasks <- 4

optim_list_all <- map(1:n_tasks, function(task) {
  load(file.path(fp_output, paste0('optim_sens_list_', task, '.RData')))
  optim_list
})

optim_list_all <- do.call(c, optim_list_all) # Concatenate the 25 length lists into one 100 length list

# Structure: multiply nested list: n_draws > 5 lists, one for each category > 4 lists, one for each value of Ctotal optimized over > lists with different elements for the different outputs

# Extract data from the list.

Ctotal_vec <- c(500, 1000, 2000, 5000)
stage_full_names <- c('production', 'processing', 'retail', 'consumption: food service', 'consumption: institutional', 'consumption: household')
makeoptimdf <- function(o) map2_dfr(o, Ctotal_vec, ~ data.frame(total_cost = .y, stage = factor(stage_full_names, levels = stage_full_names), cost = .x$pars))
makeoptimvaldf <- function(o) map2_dfr(o, Ctotal_vec, ~ data.frame(total_cost = .y, value = .x$values[length(.x$values)]))

optim_pars <- imap_dfr(optim_list_all, function(l, i) {
  cbind(draw_id = i, map2_dfr(c('GHG','land','water','energy','eutrophication'), l, ~ cbind(category = .x, makeoptimdf(.y))))
})

optim_vals <- imap_dfr(optim_list_all, function(l, i) {
  cbind(draw_id = i, map2_dfr(c('GHG','land','water','energy','eutrophication'), l, ~ cbind(category = .x, makeoptimvaldf(.y))))
})

write.csv(optim_pars, file.path(fp_output, 'opt_sensitivity_pars.csv'), row.names = FALSE)
write.csv(optim_vals, file.path(fp_output, 'opt_sensitivity_vals.csv'), row.names = FALSE)
