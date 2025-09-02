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

#sim_data <- data[c('Dummy_fl1','Dummy_fl2','Dummy_fl4','Dummy_fl5','Dummy_fl6','Dummy_fl7','Dummy_fl8')]

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
    VF12 = ifelse(`Nos últimos doze meses, eu ou algum membro do meu domicílio solicitou um empréstimo a um banco ou empresa financeira, mas esse pedido foi recusado.` == "sim", 1, 0),
    VF13 = ifelse(`A renda mensal de sua família atualmente permite que você chegue financeiramente bem ao final do mês.` == "sim", 0, 1),
    VF14 = ifelse(`Sua família seria capaz de lidar com uma despesa inesperada de R$1.500 reais hoje?` == "sim", 0, 1)
  )

#sim_data <- data[, c('VF1', 'VF2', 'VF3', 'VF4', 'VF5', 'VF6', 'VF7', 'VF8', 'VF9', 'VF10', 'VF11', 'VF12', 'VF13', 'VF14')]


data <- data %>%
  mutate(
    `CP1` = case_when(
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Concordo Totalmente" ~ 4,
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Concordo" ~ 3,
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Discordo" ~ 2,
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Discordo Totalmente" ~ 1
    ),
    `gestao` = case_when(
      `Pago minhas contas em dia.` == "Concordo Totalmente" ~ 4,
      `Pago minhas contas em dia.` == "Concordo" ~ 3,
      `Pago minhas contas em dia.` == "Discordo" ~ 2,
      `Pago minhas contas em dia.` == "Discordo Totalmente" ~ 1
    ),
    `herding` = case_when(
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Concordo Totalmente" ~ 4,
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Concordo" ~ 3,
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Discordo" ~ 2,
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Discordo totalmente" ~ 1
    ),
    `CP2` = case_when(
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Concordo Totalmente" ~ 4,
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Concordo" ~ 3,
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Discordo" ~ 2,
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Discordo totalmente" ~ 1
    ),
    `discount` = case_when(
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Concordo Totalmente" ~ 4,
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Concordo" ~ 3,
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Discordo" ~ 2,
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Discordo totalmente" ~ 1
    ),
    `CP3` = case_when(
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Concordo Totalmente" ~ 4,
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Concordo" ~ 3,
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Discordo" ~ 2,
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Discordo totalmente" ~ 1
    ),
    `CP4` = case_when(
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Concordo Totalmente" ~ 4,
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Concordo" ~ 3,
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Discordo" ~ 2,
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Discordo totalmente" ~ 1
    ),
    `CP5` = case_when(
      `Tenho muitas dívidas agora.` == "Concordo Totalmente" ~ 4,
      `Tenho muitas dívidas agora.` == "Concordo" ~ 3,
      `Tenho muitas dívidas agora.` == "Discordo" ~ 2,
      `Tenho muitas dívidas agora.` == "Discordo totalmente" ~ 1
    ),
    loss_aversion = case_when(
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Concordo Totalmente" ~ 4,
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Concordo" ~ 3,
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Discordo" ~ 2,
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Discordo totalmente" ~ 1
    ),
    `myopic` = case_when(
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Concordo Totalmente" ~ 4,
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Concordo" ~ 3,
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Discordo" ~ 2,
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Discordo totalmente" ~ 1
    )
  )

#sim_data <- data[, c('CP1', 'gestao', 'herding', 'CP2', 'discount', 'CP3', 'CP5', 'loss_aversion', 'myopic')]
#sim_data[is.na(sim_data)] <- 0

data <- data %>%
  mutate(
    FF1 = ifelse(`Investiu num produto financeiro/empresa que mais tarde se descobriu ser uma fraude` == "Sim", 1, 0),
    FF2 = ifelse(`Alguém utilizou indevidamente os seus dados bancários (net banking/cartão de crédito/cartão de débito) para comprar produtos ou serviços` == "Sim", 1, 0),
    FF3 = ifelse(`Alguém utilizou indevidamente os seus dados bancários (net banking/cartão de crédito/cartão de débito) para transferir/levantar dinheiro` == "Sim", 1, 0),
    FF4 = ifelse(`Você já perdeu dinheiro devido a fraudes por meio de mensagens ou ligações` == "Sim", 1, 0),
    FF5 = ifelse(`Qual dos seguintes é inadequado como comportamento para evitar se envolver em problemas financeiros? Escolha apenas uma resposta.` == "Confiar e deixar todo o assunto para o prestador de serviço quando for difícil tomar uma decisão", 1, 0),
    FF6 = ifelse(`Qual das seguintes opções é inadequada como uma ação relacionada a transações na Internet? Escolha apenas uma resposta.` == "Fiz uma transferência bancária usando um computador em um cibercafé", 1, 0)
  )

#sim_data <- data[, c('FF1', 'FF2', 'FF3', 'FF4', 'FF5', 'FF6')]


data <- data %>%
  mutate(
    `Crypto1` = case_when(
      `Estou confiante na minha capacidade de compreender os conceitos e princípios fundamentais das criptomoedas.` == "Discordo totalmente" ~ 1,
      `Estou confiante na minha capacidade de compreender os conceitos e princípios fundamentais das criptomoedas.` == "Discordo" ~ 2,
      `Estou confiante na minha capacidade de compreender os conceitos e princípios fundamentais das criptomoedas.` == "Concordo" ~ 3,
      `Estou confiante na minha capacidade de compreender os conceitos e princípios fundamentais das criptomoedas.` == "Concordo totalmente" ~ 4
    ),
    `Crypto2` = case_when(
      `Sou capaz de avaliar corretamente os riscos associados ao investimento em criptomoedas.` == "Discordo totalmente" ~ 1,
      `Sou capaz de avaliar corretamente os riscos associados ao investimento em criptomoedas.` == "Discordo" ~ 2,
      `Sou capaz de avaliar corretamente os riscos associados ao investimento em criptomoedas.` == "Concordo" ~ 3,
      `Sou capaz de avaliar corretamente os riscos associados ao investimento em criptomoedas.` == "Concordo totalmente" ~ 4
    ),
    `Crypto3` = case_when(
      `Conheço os diferentes tipos de criptomoedas e as suas utilizações.` == "Discordo totalmente" ~ 1,
      `Conheço os diferentes tipos de criptomoedas e as suas utilizações.` == "Discordo" ~ 2,
      `Conheço os diferentes tipos de criptomoedas e as suas utilizações.` == "Concordo" ~ 3,
      `Conheço os diferentes tipos de criptomoedas e as suas utilizações.` == "Concordo totalmente" ~ 4
    ),
    `Crypto4` = case_when(
      `Compreendo o impacto das flutuações do mercado nos preços das criptomoedas.` == "Discordo totalmente" ~ 1,
      `Compreendo o impacto das flutuações do mercado nos preços das criptomoedas.` == "Discordo" ~ 2,
      `Compreendo o impacto das flutuações do mercado nos preços das criptomoedas.` == "Concordo" ~ 3,
      `Compreendo o impacto das flutuações do mercado nos preços das criptomoedas.` == "Concordo totalmente" ~ 4
    )
  )

sim_data <- data[, c('Crypto1', 'Crypto2', 'Crypto3', 'Crypto4')]