rm(list = ls())
library(data.table)
library(bit64)
library(tidyverse)
library(stargazer)
library(lfe)
library(lubridate)
library(gridExtra)
library(grid)
library(fixest)

source("./Script/plot_utils.R")

FAST_DEATH_LOADING = TRUE

if(FAST_DEATH_LOADING == FALSE) {
  
  base2002 = fread("./DADOS/SIM2002.csv")
  base2003 = fread("./DADOS/SIM2003.csv")
  base2004 = fread("./DADOS/SIM2004.csv")
  base2005 = fread("./DADOS/SIM2005.csv")
  base2006 = fread("./DADOS/SIM2006.csv")
  base2007 = fread("./DADOS/SIM2007.csv")
  base2008 = fread("./DADOS/SIM2008.csv")
  base2009 = fread("./DADOS/SIM2009.csv")
  base2010 = fread("./DADOS/SIM2010.csv")
  base2011 = fread("./DADOS/SIM2011.csv")
  base2012 = fread("./DADOS/SIM2012.csv")
  base2013 = fread("./DADOS/SIM2013.csv")
  base2014 = fread("./DADOS/SIM2014.csv")
  base2015 = fread("./DADOS/SIM2015.csv") 
  base2016 = fread("./DADOS/SIM2016.csv")
  base2017 = fread("./DADOS/SIM2017.csv")
  base2018 = fread("./DADOS/SIM2018.csv")
  base2019 = fread("./DADOS/SIM2019.csv")

  base = rbindlist(
    l = list(base2002,base2003,base2004,base2005
             ,base2006,base2007,base2008,base2009
             ,base2010,base2011,base2012,base2013
             ,base2014,base2015,base2016,base2017
             ,base2018,base2019),
    use.names = T
  )
  
  rm(base2002,base2003,base2004,base2005
     ,base2006,base2007,base2008,base2009
     ,base2010,base2011,base2012,base2013
     ,base2014,base2015,base2016,base2017
     ,base2018,base2019)
  
  fwrite(base, "./DADOS/SIM2002_2019.csv")
} else {
  base = fread("./DADOS/SIM2002_2019.csv", encoding = "UTF-8")
}

ibge = 
  fread("./DADOS/ibge_2002_2019_comPopulacao.csv") %>% 
  filter(POPULACAO > 0 & PIB > 0) %>%
  mutate(PIB_PER_CAPITA = PIB / POPULACAO) %>% 
  mutate(ibge2 = floor(IBGE / 10)) %>%
  group_by(IBGE) %>%
  arrange(ANO) %>% 
  mutate(lag_PIB = dplyr::lag(PIB, n = 1),
         lag_PIB_PER_CAPITA = dplyr::lag(PIB_PER_CAPITA, n = 1)) %>% 
  ungroup() %>% 
  mutate(growth_pib = log(PIB) - log(lag_PIB),
         growth_pib_per_capita = log(PIB_PER_CAPITA) - log(lag_PIB_PER_CAPITA)) 


geographic_city_info = 
  ibge %>% 
  group_by(IBGE) %>% 
  mutate(last_seen = max(ANO, na.rm = T)) %>% 
  ungroup()  %>% 
  filter(ANO == last_seen) %>% 
  select(IBGE, ESTADO, REGIAO, NOME_MICRORREGIAO, NOME_MESORREGIAO)

tudo = 
  base %>% 
#  mutate(DTOBITO = dmy(DTOBITO)) %>% 
#  mutate(MES_ANO = year(DTOBITO)*1e2 + month(DTOBITO)) %>% 
  mutate(discrete_idade_obito = dplyr::ntile(idade_obito, 10)) %>% 
  group_by(ano_obito, CODMUNRES, discrete_idade_obito, def_sexo, def_raca_cor, def_escol, causabas_capitulo) %>% 
  summarize(number_deaths = n()) %>% 
  ungroup() %>% 
  left_join(ibge %>% 
               select(ano_obito = ANO,
                      CODMUNRES = ibge2,
                      PIB,
                      PIB_PER_CAPITA,
                      POPULACAO,
                      REGIAO,
                      ESTADO,
                      CODIGO_MESORREGIAO,
                      CODIGO_MICRORREGIAO,
                      growth_pib,
                      growth_pib_per_capita
               ))

toRegress = 
  tudo %>% 
  mutate(frac_deaths = 100 * number_deaths / POPULACAO) 


# Regressions (Male vs Female) -------------------------------------------------------------

model_baseline1_sexo = 
  feols(
    data = toRegress %>%
      filter(def_sexo != "Ignorado") %>% 
      mutate(def_sexo = stats::relevel(factor(def_sexo), ref = "Feminino")) 
    ,fml = log(1+number_deaths) ~  
      def_sexo |
      ano_obito^CODMUNRES^causabas_capitulo^discrete_idade_obito^def_raca_cor^def_escol
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_baseline1_sexo, "./DADOS/fastloading/model_baseline1_sexo.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_baseline1_sexo = readRDS("./DADOS/fastloading/model_baseline1_sexo.R")


model_baseline2_sexo = 
  feols(
    data = toRegress %>%
      filter(def_sexo != "Ignorado") %>% 
      mutate(def_sexo = stats::relevel(factor(def_sexo), ref = "Feminino")) 
    ,fml = log(1+number_deaths) ~  
      causabas_capitulo*def_sexo - def_sexo |
      ano_obito^CODMUNRES^discrete_idade_obito^def_raca_cor^def_escol + causabas_capitulo
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_baseline2_sexo, "./DADOS/fastloading/model_baseline2_sexo.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_baseline2_sexo = readRDS("./DADOS/fastloading/model_baseline2_sexo.R")


etable(model_baseline1_sexo, model_baseline2_sexo)

# Joga o output desse commando no Latex/Overleaf e ajeita por ali. Depois, pega a imagem e coloca no word. 
etable(model_baseline1_sexo, model_baseline2_sexo,
       tex = T,
       fixef_sizes = T,
       digits.stats = 4)


coeftable = coeftable(model_baseline2_sexo) 

to_plot = 
  coeftable %>% 
  as.data.table() %>% 
  mutate(coefficient = rownames(coeftable)) %>% 
  janitor::clean_names() %>% 
  mutate(
    formatted_coefficient = str_match(coefficient, "causabas_capitulo.+\\.[ ]*(.*):.*")[, 2]
  ) %>%
  mutate(coefficient = str_replace_all(coefficient, "(causabas_capitulo|:def_sexoMasculino)", "")) %>% 
  arrange(estimate) %>% 
  mutate(rank = row_number()) %>% 
  mutate(
    formatted_coefficient = factor(formatted_coefficient,
                                   ordered = T,
                                   .$formatted_coefficient[.$rank])
  ) %>% 
  mutate(positive_estimate = estimate >= 0)

to_plot %>% 
  ggplot(aes(x = formatted_coefficient, y = estimate)) +
  geom_errorbar(aes(ymin=estimate-1.96*std_error,
                    ymax=estimate+1.96*std_error,
                    color = positive_estimate)
  ) +
  geom_point(aes(color = positive_estimate), size = 2.8) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks())  +
  coord_flip() +
  labs(y = "Diferencial na prob. de morte (relativo à mulher)",
       x = "",
       color = "Dep. variable") + 
  scale_color_brewer(palette = "Set1", direction = -1) +  
  geom_hline(yintercept = 0, linetype = "dashed") + 
  # viridis::scale_color_viridis(discrete = T, option = "B") + 
  ggthemes::theme_clean(base_size = 22)  +
  theme(
    legend.position = "hide",
    plot.background = element_blank()
  )  
  
ggsave(
  path = "./results",
  filename = "dotplot_death_male_vs_female.pdf",
  device = "pdf",
  width = 9,
  height = 5,
  dpi = 320
)

model_sexo_ao_longo_tempo = 
  feols(
    data = 
      toRegress %>%
        filter(causabas_capitulo %in% 
                 (to_plot %>%
                 filter(!str_detect(formatted_coefficient, "(achad anorm|mentais|apófise|olho|cromoss)")) %>% 
                 pull(coefficient))) %>% 
        # filter(str_detect(causabas_capitulo, "Causas externas de morbidade")) %>% 
        filter(def_sexo != "Ignorado") %>% 
        mutate(log_deaths = log(1+number_deaths)) %>% 
        mutate(is_male = as.integer(def_sexo == "Masculino"))
    ,fml = log_deaths ~  
      i(ano_obito, is_male) - is_male |
      ano_obito^CODMUNRES^discrete_idade_obito^def_raca_cor^def_escol
    ,cluster = ~ CODMUNRES
    ,fixef.rm = "both"
    ,split = ~ causabas_capitulo
  )

# Grava o objeto resultado da regressão
#saveRDS(model_sexo_ao_longo_tempo, "./DADOS/fastloading/model_sexo_ao_longo_tempo.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_sexo_ao_longo_tempo = readRDS("./DADOS/fastloading/model_sexo_ao_longo_tempo.R")


plot_coefplot(
  regression_model = model_sexo_ao_longo_tempo,
  coefficientName = "is_male",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == TRUE) %>%
    # filter(!str_detect(formatted_coefficient, "(achad anorm|mentais|apófise|olho|cromoss)")) %>%
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_male_most_likely_to_die.pdf",
  device = "pdf",
  width = 12,
  height = 9.5,
  dpi = 320
)


plot_coefplot(
  regression_model = model_sexo_ao_longo_tempo,
  coefficientName = "is_male",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == FALSE) %>%
    # filter(!str_detect(formatted_coefficient, "(olho)")) %>% 
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_male_least_likely_to_die.pdf",
  device = "pdf",
  width = 12,
  height = 6.5,
  dpi = 320
)

# Regressions (Race) -------------------------------------------------------------

toRegress$def_raca_cor %>% table()

to_regress_raca = 
  toRegress %>%
  filter(def_raca_cor != "Ignorado")  %>% 
  mutate(def_raca_cor = case_when(
    def_raca_cor %in% c("Amarela", "Branca") ~ "Branca",
    TRUE ~ "Não Branca",
  )) %>%
  mutate(is_white = def_raca_cor == "Branca")
  #mutate(def_raca_cor = stats::relevel(factor(def_raca_cor), ref = "Não Branca")) 

model_baseline1_raca = 
  feols(
    data = 
      to_regress_raca 
    ,fml = log(1+number_deaths) ~  
      is_white |
      ano_obito^CODMUNRES^causabas_capitulo^discrete_idade_obito^def_sexo^def_escol
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_baseline1_raca, "./DADOS/fastloading/model_baseline1_raca.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_baseline1_raca = readRDS("./DADOS/fastloading/model_baseline1_raca.R")

model_baseline2_raca = 
  feols(
    data = 
      to_regress_raca
    ,fml = log(1+number_deaths) ~  
      causabas_capitulo*is_white - is_white |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_escol + causabas_capitulo
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_baseline2_raca, "./DADOS/fastloading/model_baseline2_raca.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_baseline2_raca = readRDS("./DADOS/fastloading/model_baseline2_raca.R")


etable(model_baseline1_raca, model_baseline2_raca)

# Joga o output desse commando no Latex/Overleaf e ajeita por ali. Depois, pega a imagem e coloca no word. 
etable(model_baseline1_raca, model_baseline2_raca,
       tex = T,
       fixef_sizes = T,
       digits.stats = 4)


coeftable = coeftable(model_baseline2_raca) 

to_plot = 
  coeftable %>% 
  as.data.table() %>% 
  mutate(coefficient = rownames(coeftable)) %>% 
  janitor::clean_names() %>% 
  mutate(
    formatted_coefficient = str_match(coefficient, "causabas_capitulo.+\\.[ ]*(.*):.*")[, 2]
  ) %>%
  mutate(coefficient = str_replace_all(coefficient, "(causabas_capitulo|:is_whiteTRUE)", "")) %>% 
  arrange(estimate) %>% 
  mutate(rank = row_number()) %>% 
  mutate(
    formatted_coefficient = factor(formatted_coefficient,
                                   ordered = T,
                                   .$formatted_coefficient[.$rank])
  ) %>% 
  mutate(positive_estimate = estimate >= 0)

to_plot %>% 
  ggplot(aes(x = formatted_coefficient, y = estimate)) +
  geom_errorbar(aes(ymin=estimate-1.96*std_error,
                    ymax=estimate+1.96*std_error,
                    color = positive_estimate)
  ) +
  geom_point(aes(color = positive_estimate), size = 2.8) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks())  +
  coord_flip() +
  labs(y = "Diferencial na prob. de morte (relativo a não branco)",
       x = "",
       color = "Dep. variable") + 
  scale_color_brewer(palette = "Set1", direction = -1) +  
  geom_hline(yintercept = 0, linetype = "dashed") + 
  # viridis::scale_color_viridis(discrete = T, option = "B") + 
  ggthemes::theme_clean(base_size = 22)  +
  theme(
    legend.position = "hide",
    plot.background = element_blank()
  )  

ggsave(
  path = "./results",
  filename = "dotplot_death_white_vs_non-white.pdf",
  device = "pdf",
  width = 11,
  height = 7,
  dpi = 320
)

model_raca_ao_longo_tempo = 
  feols(
    data = 
      to_regress_raca
    ,fml = log(1+number_deaths) ~  
      i(ano_obito, is_white) - is_white |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_escol
    ,cluster = ~ CODMUNRES
    # ,fixef.rm = "both"
    ,split = ~ causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_raca_ao_longo_tempo, "./DADOS/fastloading/model_raca_ao_longo_tempo.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_raca_ao_longo_tempo = readRDS("./DADOS/fastloading/model_raca_ao_longo_tempo.R")



plot_coefplot(
  regression_model = model_raca_ao_longo_tempo,
  coefficientName = "is_white",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == TRUE) %>%
    # filter(!str_detect(formatted_coefficient, "(metabóli|transtorn|muscula|puerpér|cromoss|mentais|transt imun|achad)")) %>%
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_white_most_likely_to_die.pdf",
  device = "pdf",
  width = 13,
  height = 10.5,
  dpi = 320
)


plot_coefplot(
  regression_model = model_raca_ao_longo_tempo,
  coefficientName = "is_white",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == FALSE) %>%
    filter(!str_detect(formatted_coefficient, "(olho|mastóide)")) %>%
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_white_least_likely_to_die.pdf",
  device = "pdf",
  width = 12,
  height = 9,
  dpi = 320
)


# Regressions (Escolaridade) -------------------------------------------------------------

table(toRegress$def_escol)

to_regress_escolaridade = 
  toRegress %>%
  filter(def_escol != "Ignorado")  %>% 
  mutate(def_escol = case_when(
    def_escol %in% c("Nenhuma", "1 a 3 anos", "4 a 7 anos") ~ "Ensino Fundamental", #tive de fazer essa gambiarra para balancear os dados. com as populacoes por escolaridade, não há necessidade de fazer isso
    TRUE ~ "Ensino Médio ou Superior",
  )) %>%
  mutate(is_well_educated = def_escol == "Ensino Médio ou Superior")

table(to_regress_escolaridade$is_well_educated)

model_baseline1_escolaridade = 
  feols(
    data = 
      to_regress_escolaridade 
    ,fml = log(1+number_deaths) ~  
      is_well_educated |
      ano_obito^CODMUNRES^causabas_capitulo^discrete_idade_obito^def_sexo^def_raca_cor
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_baseline1_escolaridade, "./DADOS/fastloading/model_baseline1_escolaridade.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_baseline1_escolaridade = readRDS("./DADOS/fastloading/model_baseline1_escolaridade.R")


model_baseline2_escolaridade = 
  feols(
    data = 
      to_regress_escolaridade
    ,fml = log(1+number_deaths) ~  
      causabas_capitulo*is_well_educated - is_well_educated |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_raca_cor + causabas_capitulo
    ,fixef.rm = "both"
    ,cluster = ~ CODMUNRES + causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_baseline2_escolaridade, "./DADOS/fastloading/model_baseline2_escolaridade.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_baseline2_escolaridade = readRDS("./DADOS/fastloading/model_baseline2_escolaridade.R")


etable(model_baseline1_escolaridade, model_baseline2_escolaridade)

# Joga o output desse commando no Latex/Overleaf e ajeita por ali. Depois, pega a imagem e coloca no word. 
etable(model_baseline1_escolaridade, model_baseline2_escolaridade,
       tex = T,
       fixef_sizes = T,
       digits.stats = 4)


coeftable = coeftable(model_baseline2_escolaridade) 

to_plot = 
  coeftable %>% 
  as.data.table() %>% 
  mutate(coefficient = rownames(coeftable)) %>% 
  janitor::clean_names() %>% 
  mutate(
    formatted_coefficient = str_match(coefficient, "causabas_capitulo.+\\.[ ]*(.*):.*")[, 2]
  ) %>%
  mutate(coefficient = str_replace_all(coefficient, "(causabas_capitulo|:is_well_educatedTRUE)", "")) %>% 
  arrange(estimate) %>% 
  mutate(rank = row_number()) %>% 
  mutate(
    formatted_coefficient = factor(formatted_coefficient,
                                   ordered = T,
                                   .$formatted_coefficient[.$rank])
  ) %>% 
  mutate(positive_estimate = estimate >= 0)

to_plot %>% 
  ggplot(aes(x = formatted_coefficient, y = estimate)) +
  geom_errorbar(aes(ymin=estimate-1.96*std_error,
                    ymax=estimate+1.96*std_error,
                    color = positive_estimate)
  ) +
  geom_point(aes(color = positive_estimate), size = 2.8) + 
  scale_y_continuous(label = scales::percent_format(accuracy = 1), 
                     breaks = scales::pretty_breaks())  +
  coord_flip() +
  labs(y = "Diferencial na prob. de morte a bem educado",
       x = "",
       color = "Dep. variable") + 
  scale_color_brewer(palette = "Set1", direction = -1) +  
  geom_hline(yintercept = 0, linetype = "dashed") + 
  # viridis::scale_color_viridis(discrete = T, option = "B") + 
  ggthemes::theme_clean(base_size = 22)  +
  theme(
    legend.position = "hide",
    plot.background = element_blank()
  )  

ggsave(
  path = "./results",
  filename = "dotplot_death_educated_vs_non-educated.pdf",
  device = "pdf",
  width = 11,
  height = 7,
  dpi = 320
)

model_education_ao_longo_tempo = 
  feols(
    data = 
      to_regress_escolaridade
    ,fml = log(1+number_deaths) ~  
      i(ano_obito, is_well_educated) - is_well_educated |
      ano_obito^CODMUNRES^discrete_idade_obito^def_sexo^def_raca_cor
    ,cluster = ~ CODMUNRES
    # ,fixef.rm = "both"
    ,split = ~ causabas_capitulo
  )

# Grava o objeto resultado da regressão
# saveRDS(model_education_ao_longo_tempo, "./DADOS/fastloading/model_education_ao_longo_tempo.R")
# Carrega o objeto resultado da regressão (DESCOMENTAR A LINHA DEBAIXO PARA CARREGAR O MODELO)
# model_education_ao_longo_tempo = readRDS("./DADOS/fastloading/model_education_ao_longo_tempo.R")



plot_coefplot(
  regression_model = model_education_ao_longo_tempo,
  coefficientName = "is_well_educated",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == TRUE) %>%
    # filter(!str_detect(formatted_coefficient, "(metabóli|transtorn|muscula|puerpér|cromoss|mentais|transt imun|achad)")) %>%
    mutate(formatted_coefficient = as.character(formatted_coefficient)) %>% 
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_wellEducated_most_likely_to_die.pdf",
  device = "pdf",
  width = 12,
  height = 9,
  dpi = 320
)


plot_coefplot(
  regression_model = model_education_ao_longo_tempo,
  coefficientName = "is_well_educated",
  curves_to_plot = 
    to_plot %>% 
    filter(positive_estimate == FALSE) %>%
    filter(!str_detect(formatted_coefficient, "(olho|ouvido|peri)")) %>%
    # filter(!str_detect(formatted_coefficient, "(perinat|metabóli|muscula|cromoss|mentais|olho|nervo|transt imun|achad|ouvido)")) %>%
    mutate(formatted_coefficient = as.character(formatted_coefficient)) %>% 
    pull(formatted_coefficient)
)

ggsave(
  path = "./results",
  filename = "coefplot_death_wellEducated_least_likely_to_die.pdf",
  device = "pdf",
  width = 13,
  height = 10.5,
  dpi = 320
)

