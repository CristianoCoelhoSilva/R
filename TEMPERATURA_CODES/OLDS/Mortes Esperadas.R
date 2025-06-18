library(dplyr)
library(lubridate)
set.seed(123)

datas <- seq(as.Date("2000-01-01"), as.Date("2018-12-31"), by = "day")
num_dias <- length(datas)




populacao_anual <- data.frame(
  ano = 2000:2018,
  pop = round(seq(1000000, 1200000, length.out = length(2000:2018)))
)

df_mortes <- data.frame(
  data = datas,
  ano = year(datas),
  mes_dia = format(datas, "%m-%d") # Usado para o Julian day (mês e dia)
)

# Adicionar população ao dataframe de mortes
df_mortes <- left_join(df_mortes, populacao_anual, by = "ano")

# Simular óbitos diários por 100.000 habitantes
# Base: 0.2 mortes por 100.000 (taxa diária)
# Sazonalidade: mais mortes em jan/fev e jul/ago (simulando extremos de inverno/verão)
# Ruído: aleatório
df_mortes$taxa_mortes_diaria <- 0.2 +
  0.05 * sin(2 * pi * (month(df_mortes$data) - 1) / 12) + # Sazonalidade
  rnorm(num_dias, mean = 0, sd = 0.02) # Ruído aleatório

df_mortes$mortes_observadas <- round(df_mortes$taxa_mortes_diaria * df_mortes$pop / 100000)

# Criar um indicador de onda de calor (HW) para alguns dias específicos
# Exemplo: Ondas de calor em 15-20 Jul de 2003, 20-25 Ago de 2010, 05-10 Fev de 2015
df_mortes$is_hw <- FALSE
df_mortes <- df_mortes %>%
  mutate(
    is_hw = case_when(
      (ano == 2003 & mes_dia %in% format(seq(as.Date("2003-07-15"), as.Date("2003-07-20"), by = "day"), "%m-%d")) ~ TRUE,
      (ano == 2010 & mes_dia %in% format(seq(as.Date("2010-08-20"), as.Date("2010-08-25"), by = "day"), "%m-%d")) ~ TRUE,
      (ano == 2015 & mes_dia %in% format(seq(as.Date("2015-02-05"), as.Date("2015-02-10"), by = "day"), "%m-%d")) ~ TRUE,
      .default = FALSE
    )
  )

# Criar um dataframe para armazenar a mortalidade esperada diária
df_mortes_esperadas <- df_mortes %>%
  filter(!is_hw) %>% # Excluir os dias de HW para o cálculo da base
  group_by(mes_dia) %>%
  summarise(
    mortes_esperadas_dia = mean(mortes_observadas / pop * 100000, na.rm = TRUE), # Taxa média
    .groups = 'drop'
  )

# Juntar a taxa de mortalidade esperada de volta ao dataframe principal
df_mortes <- left_join(df_mortes, df_mortes_esperadas, by = "mes_dia")

# Converter a taxa esperada de volta para número de mortes esperadas
df_mortes$mortes_esperadas <- df_mortes$mortes_esperadas_dia * df_mortes$pop / 100000


df_hw_eventos_OE <- df_mortes %>%
  arrange(data) %>% 
  filter(is_hw) %>% 
  mutate(
    group_start = (data - lag(data, default = first(data) - 1)) != 1
  ) %>%
  mutate(
    # 'hw_event_id' é um ID sequencial para cada evento de onda de calor
    hw_event_id = cumsum(group_start)
  ) %>%
  group_by(hw_event_id) %>% # Agora agrupa por cada evento de onda de calor
  summarise(
    data_inicio_hw = min(data),
    data_fim_hw = max(data),
    duracao_dias = n(),
    mortes_observadas_hw = sum(mortes_observadas),
    mortes_esperadas_hw = sum(mortes_esperadas),
    # Calcula a Razão O/E para o evento completo da onda de calor
    razao_OE_hw_evento = mortes_observadas_hw / mortes_esperadas_hw,
    .groups = 'drop' # Remove o agrupamento após o summarise
  )

print("Razão O/E para cada evento de Onda de Calor:")
print(df_hw_eventos_OE)

df_hw_dias_individuais <- df_mortes %>%
  filter(is_hw == TRUE) %>%
  mutate(
    razao_OE = mortes_observadas / mortes_esperadas
  )

print("Razão O/E por dia individual de Onda de Calor:")
print(df_hw_dias_individuais %>% select(data, mortes_observadas, mortes_esperadas, razao_OE))
