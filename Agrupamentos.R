#Script para Agrupamento

#Verifica a distribuição por Sexo
sexo <- data %>%
  group_by(Sexo) %>%
  count() %>%
  ungroup() %>%
  mutate(percentage = round((n / sum(n)) * 100, 2))


#Verifica a distribuição por Sexo
Idade <- data %>%
  group_by(Idade) %>%
  count() %>%
  ungroup() %>%
  mutate(percentage = round((n / sum(n)) * 100, 2))

question1 <- data %>%
  group_by(question1) %>%
  count() %>%
  ungroup() %>%
  mutate(percentage = round((n / sum(n)) * 100, 2))