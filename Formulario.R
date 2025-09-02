data <- read_excel("Vulnerabilidade e Educação Financeira.xlsx")


data <- data %>%
  mutate(
    Dummy_fl1 = ifelse(fl1 == "Mais de $ 102", 1, 0),
    Dummy_fl2 = ifelse(fl2 == "Menos do que hoje", 1, 0),
    Dummy_fl3 = ifelse(fl3 == "Diminuem", 1, 0),
    Dummy_fl4 = ifelse(fl4 == "Verdadeiro", 1, 0),
    Dummy_fl5 = ifelse(fl5 == "Falso", 1, 0),
    Dummy_fl6 = ifelse(fl6 == "Utilizar frequentemente planos de pagamento a prestações de cartões de crédito para adiar o pagamento.", 1, 0),
    Dummy_fl7 = ifelse(fl7 == "Pelo menos 2 anos, mas menos de 5 anos", 1, 0),
    Dummy_fl8 = ifelse(fl8 == "Empresa de classificação de risco", 1, 0)
  )


data <- data %>%
  mutate(
    VF1 = ifelse(`Compra de alimentos` == "Sim", 1, 0),
    VF2 = ifelse(`Compra de roupas essenciais` == "Sim", 1, 0),
    VF3 = ifelse(`Pagamento de contas de gás, eletricidade, telefone, etc.` == "Sim", 1, 0),
    VF4 = ifelse(`Pagamento de taxas de administração do condomínio` == "Sim", 1, 0),
    VF5 = ifelse(`Pagamento do aluguel` == "Sim", 1, 0),
    VF6 = ifelse(`Pagamento de empréstimos para a compra de carros, televisores, computadores, etc.` == "Sim", 1, 0),
    VF7 = ifelse(`Contas de gás, eletricidade, telefone, etc.` == "Sim", 1, 0),
    VF8 = ifelse(`Taxas de administração do condomínio` == "Sim", 1, 0),
    VF9 = ifelse(`Aluguel` == "Sim", 1, 0),
    VF10 = ifelse(`Pagamento de empréstimos para ompra de carros, televisores, computadores` == "Sim", 1, 0),
    VF11 = ifelse(`Outras contas` == "Sim", 1, 0),
    VF12 = ifelse(`Nos últimos doze meses, eu ou algum membro do meu domicílio solicitou um empréstimo a um banco ou empresa financeira, mas esse pedido foi recusado.` == "Sim", 1, 0),
    VF13 = ifelse(`A renda mensal de sua família atualmente permite que você chegue financeiramente bem ao final do mês.` == "Sim", 0, 1),
    VF14 = ifelse(`Sua família seria capaz de lidar com uma despesa inesperada de R$1.500 reais hoje?` == "Sim", 0, 1)
  )

#sim_data <- data[c('Dummy_fl1','Dummy_fl2','Dummy_fl4','Dummy_fl5','Dummy_fl6','Dummy_fl7','Dummy_fl8')]

sim_data <- data[, c('VF1', 'VF2', 'VF3', 'VF4', 'VF5', 'VF6', 'VF7', 'VF8', 'VF9', 'VF10', 'VF11', 'VF12', 'VF13', 'VF14')]
