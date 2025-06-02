rm(list = setdiff(ls(), c("temperaturaConv","temperaturaAuto")))

temperaturaAuto <- temperaturaAuto %>%
  filter(!is.na(Codigo_IBGE))

temperaturaConv <- temperaturaConv %>%
  filter(!is.na(Codigo_IBGE))

temperatura <- rbind(temperaturaAuto, temperaturaConv)

temperatura <- temperatura %>%
  group_by(ESTADO,	Codigo_IBGE,	DATA,	Mes) %>%
  summarise(
    media_temperatura_maxima = mean(temperatura_maxima, na.rm = TRUE),
    media_temperatura_minima = mean(temperatura_minima, na.rm = TRUE),
    media_max = mean(Max, na.rm = TRUE),
    media_min = mean(Min, na.rm = TRUE)
  )

temperatura$diff_max <- temperatura$media_temperatura_maxima - temperatura$media_max
temperatura$diff_min <- temperatura$media_temperatura_minima - temperatura$media_min

temperatura <- temperatura %>%
  mutate(anomalia_max = ifelse(diff_max >= 5, 1, 0)) %>%
  mutate(anomalia_min = ifelse(diff_min <= -5, 1, 0))

temperatura <- temperatura %>%
  filter(!is.na(media_max) | !is.na(media_min))

rm(list = setdiff(ls(), c("temperatura")))