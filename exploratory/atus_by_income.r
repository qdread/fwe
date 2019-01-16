atus_withcov <- atusact_summ %>%
  left_join(atuscps)

groctravel <- atus_withcov %>%
  filter(food_activity_name == 'grocery_shopping_travel', dur > 0) %>%
  select(famincome, dur)

groctravel <- groctravel %>% mutate(famincome = ordered(famincome, levels = levels(atus_withcov$famincome)))

ggplot(groctravel, aes(x = famincome, y = dur)) +
  geom_boxplot() +
  scale_y_log10()
ggplot(groctravel, aes(x = famincome, y = dur)) +
  stat_summary(geom = 'point', fun.y = mean) +
  scale_y_log10()

lmtravel <- lm(dur ~ famincome, data = groctravel)
summary(lmtravel)
lmtravel_testtrend <- lm(dur ~ as.numeric(famincome), data = groctravel)
summary(lmtravel_testtrend)

grocshop <- atus_withcov %>%
  filter(food_activity_name == 'grocery_shopping', dur > 0) %>%
  select(famincome, dur)

ggplot(grocshop, aes(x = famincome, y = dur)) +
  geom_boxplot() +
  scale_y_log10()

