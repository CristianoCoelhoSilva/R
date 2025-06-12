validacao <- temperatura[c('DC_NOME','Ano', 'temperatura_maxima','Temperatura_Normal')] 

validacao <- validacao[validacao$DC_NOME == 'PORTO VELHO',]


library(dplyr)

validacao <- validacao %>%
  group_by(DC_NOME, Ano) %>%
  summarise(
    media_temperatura_maxima = mean(temperatura_maxima, na.rm = TRUE),
    media_temperatura_normal = mean(Temperatura_Normal, na.rm = TRUE)
  )

validacao <- temperatura %>% group_by(anomolia3, anomolia5, anomolia7) %>% count()

validacao <- temperatura %>% group_by(anomolia3, anomolia5, anomolia7) %>%  
  summarise(
    media_temperatura_maxima = mean(EHISIG, na.rm = TRUE),
    media_temperatura_normal = mean(EHIACCL, na.rm = TRUE)
  )


validacao <- base %>%
  group_by(DC_NOME) %>%
  summarise(
    max_temperatura_media_3dias = max(temperatura_media_3dias, na.rm = TRUE)
  )

validacao <- base %>%
  group_by(DC_NOME) %>%
  summarise(
    media_temperatura_media_3dias = mean(temperatura_media_3dias, na.rm = TRUE),
    media_temperatura_maxima = mean(temperatura_maxima, na.rm = TRUE)
  )

validacao <- base %>%
  group_by(DC_NOME, Ano) %>%
  summarise(
    media_temperatura_maxima = mean(temperatura_maxima, na.rm = TRUE)
  )

validacao <- validacao[validacao$DC_NOME %in% c('BRASILIA', 'SAO PAULO', 'RECIFE'), ]
