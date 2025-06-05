library(readr)

temperatura <- temperatura

temperatura <- temperatura %>%
  arrange(Codigo_IBGE, DATA) %>%
  group_by(Codigo_IBGE) %>%
  mutate(
    is_hot = temperatura_maxima > 35,
    hot_run_id = cumsum(c(1, diff(is_hot) != 0)),
    consecutive_hot_days = sequence(rle(is_hot)$lengths),
    hot_spell_3_consecutive_days = ifelse(is_hot & consecutive_hot_days >= 3, TRUE, FALSE)
  ) %>%
  ungroup()
