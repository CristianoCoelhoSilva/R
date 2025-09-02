# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)



# Set the path for the input Excel file and output directory
data <- read_excel("Vulnerabilidade e Educação Financeira.xlsx")
View(input_file)

# Show unique options in the 'Sexo' variable
unique_sex <- unique(data$Sexo)
print(unique_sex)

# Create binary variables for 'Female' and 'Male'
data$Female <- ifelse(data$Sexo == "Feminino", 1, 0)
data$Male <- ifelse(data$Sexo == "Masculino", 1, 0)
data$NonBinary <- ifelse(data$Sexo == "Não binário", 1, 0)



# Show unique options in the 'Idade' variable
unique_idade <- unique(data$Idade)
print(unique_idade)

data <- data %>%
  mutate(
    Age_18_25 = ifelse(Idade == "18-25", 1, 0),
    Age_26_30 = ifelse(Idade == "26-30", 1, 0),
    Age_31_35 = ifelse(Idade == "31-35", 1, 0),
    Age_36_40 = ifelse(Idade == "36-40", 1, 0),
    Age_41_45 = ifelse(Idade == "41-45", 1, 0),
    Age_46_50 = ifelse(Idade == "46-50", 1, 0),
    Age_51_55 = ifelse(Idade == "51-55", 1, 0),
    Age_56_60 = ifelse(Idade == "56-60", 1, 0),
    Age_60_plus = ifelse(Idade == "60+", 1, 0)
  )

# create variable Young = 1 if age 18-25 or 26-30
# create variable old =1 if age 56-60 or 60+

data$Young <- ifelse(data$Age_18_25 == 1 | data$Age_26_30 == 1, 1, 0)
data$Old <- ifelse(data$Age_56_60 == 1 | data$Age_60_plus == 1, 1, 0)


unique_race <- unique(data$race)
print(unique_race)

data <- data %>%
  mutate(
    White = ifelse(race== "Branca", 1, 0),
    Black = ifelse(race == "Preta", 1, 0),
    Brown = ifelse(race == "Parda", 1, 0),
    Yellow = ifelse(race == "Amarela", 1, 0),
    Indigenous = ifelse(race == "Indígena", 1, 0),
    NoAnswer_race = ifelse(race == "Não informado", 1, 0)
  )


unique_escola <- unique(data$Escolaridade)
print(unique_escola)

data <- data %>%
  mutate(
    Elementary = ifelse(Escolaridade == "Ensino Fundamental", 1, 0),
    HighSchool = ifelse(Escolaridade == "Ensino Médio", 1, 0),
    College = ifelse(Escolaridade == "Ensino Superior", 1, 0),
    PostGrad = ifelse(Escolaridade == "Pós-graduação", 1, 0),
    NoAnswer_esc = ifelse(Escolaridade == "Não informado", 1, 0)
  )

 
# CRIA VARIAVEL SCHOOL = 1 SE Ensino Superior ou Pós-graduação

data$School <- ifelse(data$College == 1 | data$PostGrad == 1, 1, 0)




## rename variable data$`Qual a sua renda mensal, aproximadamente?(renda individual)` as data$renda
data <- data %>%
  rename(Income = `Qual a sua renda mensal, aproximadamente?(renda individual)`)




unique_renda <- unique(data$Income)
print(unique_renda)


# Creating dummy variables for each income category
data <- data %>%
  mutate(
    Income_1_3_SM = ifelse(Income == "De 1 a 3 salários mínimos (R$1.320 a R$3.960)", 1, 0),
    Income_Until_1_SM = ifelse(Income == "Até 1 salário mínimo (R$ 1.320)", 1, 0),
    Income_More_20_SM = ifelse(Income == "Mais de 20 salários mínimos ( mais de R$26.400)", 1, 0),
    Income_3_6_SM = ifelse(Income == "De 3 a 6 salários mínimos (R$3.960 a R$7.920)", 1, 0),
    Income_6_9_SM = ifelse(Income == "De 6 a 9 salários mínimos (R$7.920 a R$11.880)", 1, 0),
    Income_10_20_SM = ifelse(Income == "De 10 a 20 salários mínimos  (R$13.200 a R$26.400)", 1, 0)
  )


# cria variavel low_income = 1 se Income = Até 1 salário mínimo (R$ 1.320) ou De 1 a 3 salários mínimos (R$1.320 a R$3.960)
# cria variável high_income =1 se Income = De 10 a 20 salários mínimos  (R$13.200 a R$26.400) ou Mais de 20 salários mínimos ( mais de R$26.400)

data$Low_income <- ifelse(data$Income_Until_1_SM == 1 | data$Income_1_3_SM == 1, 1, 0)

data$High_income <- ifelse(data$Income_10_20_SM == 1 | data$Income_More_20_SM == 1, 1, 0)

data <- data %>%
  mutate(
    CR1 = ifelse(`Um taco e uma bola custam R$1,10 no total. O taco custa R$1,00 a mais do que a bola. Quanto custa a bola?` == "0,05 centavos", 1, 0),
    CR2 = ifelse(`Se 5 máquinas levam 5 minutos para fazer 5 ferramentas, quanto tempo levaria 100 máquinas para fazer 100 ferramentas?` == "5 minutos", 1, 0),
    CR3 = ifelse(`Em um lago, há um conjunto de vitórias régias. Todos os dias, a mancha dobra de tamanho. Se forem necessários 48 dias para que as vitórias régias cubram todo o lago, quanto tempo levaria para que cobrisse metade do lago?` == "47 dias", 1, 0),
    CR4 = ifelse(`Se João pode beber um barril de água em 6 dias e Maria pode beber um barril de água em 12 dias, quanto tempo levaria para beberem um barril de água juntos?` == "4 dias", 1, 0),
    CR5 = ifelse(`Jerry recebeu a 15ª nota mais alta e a 15ª mais baixa da turma. Quantos alunos há na classe?` == "29 alunos", 1, 0),
    CR6 = ifelse(`Um homem compra um porco por R$60, vende-o por R$70, compra-o novamente por R$80 e, finalmente, vende-o por R$90. Quanto ele ganhou?` == "20 reais", 1, 0),
    CR7 = ifelse(`Simon decidiu investir R$8.000 no mercado de ações em um dia no início de 2008. Seis meses após o investimento, em 17 de julho, as ações que ele havia comprado caíram 50%. Felizmente para Simon, de 17 de julho a 17 de outubro, as ações que ele havia comprado subiram 75%. Nesse momento, Simon:` == "Perdeu dinheiro", 1, 0)
  )


data <- data %>%
  mutate(
    Dummy_fl1 = ifelse(fl1 == "Mais de $ 102", 1, 0),
    Dummy_fl2 = ifelse(fl2 == "Menos do que hoje", 1, 0),
    #Dummy_fl3 = ifelse(fl3 == "Diminuem", 1, 0),
    Dummy_fl4 = ifelse(fl4 == "Verdadeiro", 1, 0),
    Dummy_fl5 = ifelse(fl5 == "Falso", 1, 0),
    Dummy_fl6 = ifelse(fl6 == "Utilizar frequentemente planos de pagamento a prestações de cartões de crédito para adiar o pagamento.", 1, 0),
    Dummy_fl7 = ifelse(fl7 == "Pelo menos 2 anos, mas menos de 5 anos", 1, 0),
    Dummy_fl8 = ifelse(fl8 == "Empresa de classificação de risco", 1, 0)
  )


# cria variavel FL = (Dummy_fl1 + Dummy_fl2 + Dummy_fl3 + Dummy_fl4 + Dummy_fl5 + Dummy_fl7 + Dummy_fl8)/8

#data$FL <- (data$Dummy_fl1 + data$Dummy_fl2 + data$Dummy_fl3 + data$Dummy_fl4 + data$Dummy_fl5 + data$Dummy_fl7 + data$Dummy_fl8) / 8


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


# criando a variavel Crypto que é a soma de (Crypto1+Crypto2+Crypto3+Crypto4)/16

data$Crypto <- (data$Crypto1 + data$Crypto2 + data$Crypto3 + data$Crypto4) / 16

data <- data %>%
  mutate(
    FF1 = ifelse(`Investiu num produto financeiro/empresa que mais tarde se descobriu ser uma fraude` == "Sim", 1, 0),
    FF2 = ifelse(`Alguém utilizou indevidamente os seus dados bancários (net banking/cartão de crédito/cartão de débito) para comprar produtos ou serviços` == "Sim", 1, 0),
    FF3 = ifelse(`Alguém utilizou indevidamente os seus dados bancários (net banking/cartão de crédito/cartão de débito) para transferir/levantar dinheiro` == "Sim", 1, 0),
    FF4 = ifelse(`Você já perdeu dinheiro devido a fraudes por meio de mensagens ou ligações` == "Sim", 1, 0),
    FF5 = ifelse(`Qual dos seguintes é inadequado como comportamento para evitar se envolver em problemas financeiros? Escolha apenas uma resposta.` == "Confiar e deixar todo o assunto para o prestador de serviço quando for difícil tomar uma decisão", 1, 0),
    FF6 = ifelse(`Qual das seguintes opções é inadequada como uma ação relacionada a transações na Internet? Escolha apenas uma resposta.` == "Fiz uma transferência bancária usando um computador em um cibercafé", 1, 0)
  )

# cria a variavel FF = (FF1+FF2+FF3+FF4+FF5+FF6)/6

data$FF <- (data$FF1 + data$FF2 + data$FF3 + data$FF4 + data$FF5 + data$FF6) / 6

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
# cria a variavel VF = (VF1+VF2+VF3+VF4+VF5+VF6+VF7+VF8+VF9+VF10+VF11+VF12+VF13+VF14)/14

data$VF <- (data$VF1 + data$VF2 + data$VF3 + data$VF4 + data$VF5 + data$VF6 + data$VF7 + data$VF8 + data$VF9 + data$VF10 + data$VF11 + data$VF12 + data$VF13 + data$VF14) / 14


data <- data %>%
  mutate(
    `CP1` = case_when(
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Concordo totalmente" ~ 4,
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Concordo" ~ 3,
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Discordo" ~ 2,
      `Antes de comprar algo, considero cuidadosamente se posso pagar.` == "Discordo totalmente" ~ 1
    ),
    `gestao` = case_when(
      `Pago minhas contas em dia.` == "Concordo totalmente" ~ 4,
      `Pago minhas contas em dia.` == "Concordo" ~ 3,
      `Pago minhas contas em dia.` == "Discordo" ~ 2,
      `Pago minhas contas em dia.` == "Discordo totalmente" ~ 1
    ),
    `herding` = case_when(
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Concordo totalmente" ~ 4,
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Concordo" ~ 3,
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Discordo" ~ 2,
      `Quando há vários produtos similares, costumo comprar o que é recomendado como o produto mais vendido, em vez do que eu realmente acho que é um bom produto.` == "Discordo totalmente" ~ 1
    ),
    `CP2` = case_when(
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Concordo totalmente" ~ 4,
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Concordo" ~ 3,
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Discordo" ~ 2,
      `Estabeleço metas financeiras de longo prazo e me esforço para alcançá-las.` == "Discordo totalmente" ~ 1
    ),
    `discount` = case_when(
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Concordo totalmente" ~ 4,
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Concordo" ~ 3,
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Discordo" ~ 2,
      `Acho mais satisfatório gastar dinheiro do que guardá-lo para o longo prazo.` == "Discordo totalmente" ~ 1
    ),
    `CP3` = case_when(
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Concordo totalmente" ~ 4,
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Concordo" ~ 3,
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Discordo" ~ 2,
      `Costumo viver para o hoje e deixar o amanhã cuidar de si mesmo.` == "Discordo totalmente" ~ 1
    ),
    `CP4` = case_when(
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Concordo totalmente" ~ 4,
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Concordo" ~ 3,
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Discordo" ~ 2,
      `Eu mantenho uma vigilância pessoal sobre meus assuntos financeiros.` == "Discordo totalmente" ~ 1
    ),
    `CP5` = case_when(
      `Tenho muitas dívidas agora.` == "Concordo totalmente" ~ 4,
      `Tenho muitas dívidas agora.` == "Concordo" ~ 3,
      `Tenho muitas dívidas agora.` == "Discordo" ~ 2,
      `Tenho muitas dívidas agora.` == "Discordo totalmente" ~ 1
    ),
    loss_aversion = case_when(
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Concordo totalmente" ~ 4,
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Concordo" ~ 3,
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Discordo" ~ 2,
      `Estou preparado para arriscar parte do meu próprio dinheiro ao poupar ou fazer um investimento.` == "Discordo totalmente" ~ 1
    ),
    `myopic` = case_when(
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Concordo totalmente" ~ 4,
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Concordo" ~ 3,
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Discordo" ~ 2,
      `Se eu tivesse a escolha de (1) receber 10.000 reais agora ou (2) receber 12.000 reais em um ano, eu escolheria (1), desde que eu possa definitivamente receber o dinheiro.` == "Discordo totalmente" ~ 1
    )
  )

# criando a variavel CP2 = (CP1+ CP2 + CP3 + CP4 + CP5) /20

data$CP2 <- (data$CP1 + data$CP2 + data$CP3 + data$CP4 + data$CP5) / 20

# cria variavel CP1_b = 1 se escolheu CP Concordo totalmente ou Concordo
# Cria variavel CP2_b=1 se escolheu Concordo totalmente ou Concordo


data$CP1_b <- ifelse(data$CP1 %in% c(3, 4), 1, 0)

data$CP2_b <- ifelse(data$CP2 %in% c(3, 4), 1, 0)

data$CP3_b <- ifelse(data$CP3 %in% c(1, 2), 1, 0)

data$CP4_b <- ifelse(data$CP4 %in% c(3, 4), 1, 0)

data$CP5_b <- ifelse(data$CP5 %in% c(1, 2), 1, 0)

# cria a variavel CP=(CP1_b+CP2_b+CP3_b+CP4_b+CP5_b)/5

data$CP <- (data$CP1_b + data$CP2_b + data$CP3_b + data$CP4_b + data$CP5_b) / 5

# criando a variavel myopic = 1 se escolheu discordo ou discordo totalmente

data$myopic <- ifelse(data$myopic %in% c(1, 2), 1, 0)

# criando a variavel herding = 1 se escolheu concordo ou concordo totalmente

data$herding <- ifelse(data$herding %in% c(3, 4), 1, 0)

# criando a variavel discount = 1 se escolheu concordo ou concordo totalmente

data$discount <- ifelse(data$discount %in% c(3, 4), 1, 0)

# criando a variavel loss_aversion = 1 se escolheu discordo ou discordo totalmente

data$loss_aversion <- ifelse(data$loss_aversion %in% c(1, 2), 1, 0)

# criando a variavel gestao = 1 se escolheu concordo ou concordo totalmente

data$gestao <- ifelse(data$gestao %in% c(3, 4), 1, 0)


###########################################################################
# otimizado

# Load necessary library
library(mirt)

# Select relevant variables and convert to numeric
fin_literacy <- data[, grep("Dummy_fl", names(data))]
fin_literacy[] <- lapply(fin_literacy, as.numeric)
fin_literacy <- na.omit(fin_literacy)

# Guarda os índices antes de remover NA
valid_rows <- complete.cases(fin_literacy)

# Aplica na.omit
fin_literacy <- fin_literacy[valid_rows, ]

# Check the variation in each item and remove non-varying items
var_counts <- apply(fin_literacy, 2, function(x) length(unique(x)))
fin_literacy <- fin_literacy[, var_counts > 1]

# Update the model definition based on the remaining items
num_items <- sum(var_counts > 1)
model <- sprintf('FL = 1-%d', num_items)



# Fit the 2PL model using mirt
fit <- mirt(fin_literacy, model=model, itemtype="2PL")

summary(fit)

coef(fit, simplify = TRUE)$items

# Extract and print item parameters
# item_params <- coef(fit, simplify = TRUE)

fl_scores <- fscores(fit)

data$FL <- NA

# Preenche apenas as linhas válidas (sem NA nas variáveis de literacia)
data$FL[valid_rows] <- fl_scores[, 1]

# Plot item characteristic curves for each item
plot(fit, type='trace', items=1:num_items)

# Print model fit indices
cat("AIC: ", AIC(fit), "\n")
cat("BIC: ", BIC(fit), "\n")

# Analyze and interpret item parameters for decision making
summary(fit)

fliteracy <- data[, grep("Dummy_fl", names(data))]
fliteracy$FL <- data$FL
fliteracy$Ind <- data$Indigenous

print(fliteracy)

# create a new variable 
# FL = Dummy_FL1 + Dummy_FL2 + Dummy_FL3 + Dummy_FL4 + Dummy_FL5 + Dummy_FL7 + Dummy_FL8

#data$FL <- rowSums(data[, grep("Dummy_fl", names(data))])

# create a new variable which is the sum of 
# CR = CR1+ CR2 + CR3 + CR4 + CR5 + CR6 + CR7

data$CR <- rowSums(data[, grep("CR", names(data))])

data <- data %>%
  mutate(
    # Create a new variable based on fl_school
    Dummy_fl_school = ifelse(fl_school == "Sim, e participei da educação financeira", 1, 0),
    
    # Create a new variable based on fl_knowledge
    Dummy_fl_knowledge = ifelse(fl_knowledge %in% c("Concordo Totalmente", "Concordo Moderadamente"), 1, 0)
  )

data_sumary <- data[c('FL','VF','Crypto','FF','CR','CP')]

summary(data_sumary)




###################################################################################

#data <- data[c("FL", "CR","CP","Female","NonBinary","Black","Yellow","Brown","Indigenous","Young","Old","Low_income","High_income","loss_aversion","gestao","myopic","discount","herding")]

#write.csv(data, "base_machine_learning.csv", row.names = FALSE)

fit1 <- lm(FL ~ CR, data = data)

# Print the summary of the regression model

summary(fit1)

data <- data %>%
  mutate(
    # Create a new variable based on fl_school
    Dummy_fl_school = ifelse(fl_school == "Sim, e participei da educação financeira", 1, 0),
    
    # Create a new variable based on fl_knowledge
    Dummy_fl_knowledge = ifelse(fl_knowledge %in% c("Concordo Totalmente", "Concordo Moderadamente"), 1, 0)
  )


# include in the regression Dummy_fl_school and Dummy_fl_knowledge as independent variables

fit2 <- lm(FL ~ CR + Dummy_fl_school + Dummy_fl_knowledge, data = data)

# Print the summary of the regression model

summary(fit2)

# include Female and Non_Binary as independent variables

fit3 <- lm(FL ~ CR + Dummy_fl_school + Dummy_fl_knowledge +Female + NonBinary, data = data)

# Print the summary of the regression model

summary(fit3)

# include black, brown, yellow, indigenous as independent variables

fit4 <- lm(FL ~ CR + Dummy_fl_school + Dummy_fl_knowledge 
           +Female + NonBinary + Black + Yellow + Brown + Indigenous, data = data)

# Print the summary of the regression model

summary(fit4)

fit_FL <- lm(FL ~ CR + Dummy_fl_school + Dummy_fl_knowledge  +Female + NonBinary + Black + Yellow + Brown + Indigenous
             + Income_1_3_SM + Income_10_20_SM  +Crypto + loss_aversion + gestao+
               myopic + discount +herding, data = data)

# Print the summary of the regression model

summary(fit_FL)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_FL)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_FL_hc <- coeftest(fit_FL, vcov = vcovHC)

# use stargazer to present the results fit5 with and without correction for
# heteroskedastic standard errors


##############################################################################



fit_VF <- lm(VF ~ FL+FF+CR + Dummy_fl_school + Dummy_fl_knowledge  +Female + NonBinary + Black + Yellow + Brown + Indigenous
             + Income_1_3_SM + Income_10_20_SM  +Crypto + loss_aversion + gestao+
               myopic + discount +herding, data = data)

# Print the summary of the regression model

summary(fit_VF)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_VF)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_VF_hc <- coeftest(fit_VF, vcov = vcovHC)

# use stargazer to present the results fit5 with and without correction for
# heteroskedastic standard errors


##################################################################################


fit_FF <- lm(FF ~ FL+CR + Dummy_fl_school + Dummy_fl_knowledge  +Female + NonBinary + Black + Yellow + Brown + Indigenous
             + Income_1_3_SM + Income_10_20_SM  +Crypto + loss_aversion + gestao+
               myopic + discount +herding, data = data)

# Print the summary of the regression model

summary(fit_FF)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_FF)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_FF_hc <- coeftest(fit_FF, vcov = vcovHC)

# use stargazer to present the results fit5 with and without correction for
# heteroskedastic standard errors

#################################################################################
library(stargazer)

# write what are the columns of the table 

stargazer(fit_VF, fit_VF_hc, fit_FF, fit_FF_hc,type = "text")


#####################################################333333
fit_VF <- lm(VF ~ FL+CR +CP +Female + NonBinary + Black + Yellow + Brown + Indigenous
             + Young +Old  + Low_income +High_income  + loss_aversion + gestao+
               myopic + discount +herding, data = data)

# Print the summary of the regression model

summary(fit_VF)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_VF)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_VF_hc <- coeftest(fit_VF, vcov = vcovHC)


fit_FF <- lm(FF ~ FL+CR +CP +Female + NonBinary + Black + Yellow + Brown + Indigenous
             + Young +Old  + Low_income +High_income  + loss_aversion + gestao+
               myopic + discount +herding, data = data)

# Print the summary of the regression model

summary(fit_FF)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_FF)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_FF_hc <- coeftest(fit_FF, vcov = vcovHC)

fit_Crypto <- lm(Crypto ~ FL+CR +CP +Female + NonBinary + Black + Yellow + Brown + Indigenous
                 + Young +Old + Low_income +High_income + loss_aversion + gestao+
                   myopic + discount +herding, data = data)

# Print the summary of the regression model

summary(fit_Crypto)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_Crypto)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_Crypto_hc <- coeftest(fit_Crypto, vcov = vcovHC)

fit_Crypto <- lm(Crypto ~ FL+CR +CP +Female + NonBinary + Black + Yellow + Brown + Indigenous
                 + Young +Old  + Low_income +High_income  + loss_aversion + gestao+
                   myopic + discount +herding, data = data)

# Print the summary of the regression model

summary(fit_Crypto)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_Crypto)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_Crypto_hc <- coeftest(fit_Crypto, vcov = vcovHC)

fit_FL <- lm( FL~ CR +CP +Female + NonBinary + Black + Yellow + Brown + Indigenous
              + Young +Old + Low_income +High_income + loss_aversion + gestao+
                myopic + discount +herding, data = data)




# Print the summary of the regression model

summary(fit_FL)

# is there multiconlinearity?
# load package for vif

library(car)
vif(fit_FL)


# interpretation

# The VIF values are all below 10, indicating that multicollinearity is not a significant issue in the model.

# test the fit5 with heteroskedastic standard errors
# load the package for heteroskedastic standard errors

library(lmtest)
library(sandwich)

fit_FL_hc <- coeftest(fit_FL, vcov = vcovHC)


library(stargazer)

# write what are the columns of the table 

#stargazer(fit_FL, fit_FL_hc, fit_VF, fit_VF_hc, fit_FF, fit_FF_hc, fit_Crypto, fit_Crypto_hc,type = "text")

#stargazer(fit_FL, fit_FL_hc, fit_VF, fit_VF_hc, fit_FF, fit_FF_hc,type = "text")

# save the stargazer in text in C:/MODELO_VUL_FINAN/Modelo_vul_finan.txt

stargazer(fit_FL, fit_FL_hc, fit_VF, fit_VF_hc, fit_FF, fit_FF_hc, type = "text", out = "Modelo_vul_finan.txt")

stargazer(fit_Crypto, fit_Crypto_hc, type = "text", out = "crypto.txt")

