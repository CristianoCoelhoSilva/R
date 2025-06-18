df <- temperatura %>%
  arrange(DC_NOME, DATA)

df_final <- df %>%
  group_by(DC_NOME) %>% # Agrupar por cidade para calcular a média para cada uma
  mutate(temperatura_media_3dias = zoo::rollmeanr(temperatura_maxima, k = 3, fill = NA, align = "right")) %>%
  ungroup()

df_final <- df_final %>%
  group_by(DC_NOME) %>% # Agrupar por cidade para calcular a média para cada uma
  mutate(temperatura_media_30dias = zoo::rollmeanr(temperatura_maxima, k = 30, fill = NA, align = "right")) %>%
  ungroup()