library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(openxlsx)
data <- read_excel("NEWS PROJETOS/NEWS PROJETOS/DIOGO/Diogo Perfis de Racionalidade.xlsx", .name_repair = "minimal")

names(data)[names(data) == ""] <- paste0("unnamed_", seq_len(sum(names(data) == "")))


corrigir_cabecalho <- function(df) {
  df[] <- lapply(df, as.character)
  nomes_atuais <- names(df)
  primeira_linha <- as.character(unlist(df[1, ]))
  novos_nomes <- ifelse(primeira_linha == "Response", nomes_atuais, primeira_linha)
  names(df) <- novos_nomes
  df <- df[-1, ]
  rownames(df) <- NULL
  return(df)
}

data <- corrigir_cabecalho(data)

data <- data %>%
  select(11:85)

data <- data %>%
  mutate (
    Gender_male = ifelse(`Sexo` == 'Masculino', 1, 0),                                                                                                              
    Gender_female = ifelse(`Sexo` == 'Feminino', 1, 0),                                                                                                             
    Gender_Other = ifelse(`Sexo` == 'Não binário', 1, 0), 
    Gender_Other = ifelse(`Sexo` == 'Prefiro não declarar', 1, 0),
    
    Menor_18 = ifelse(`Idade` == 'Menos de 18', 1, 0),

    Primary_ed_incomplet        = ifelse(`Escolaridade` == 'Fundamental incompleto', 1, 0),
    Primary_education           = ifelse(`Escolaridade` == 'Fundamental completo', 1, 0),
    High_school                 = ifelse(`Escolaridade` == 'Ensino Médio Completo', 1, 0),
    High_school_incomplet       = ifelse(`Escolaridade` == 'Ensino Médio Incompleto', 1, 0),
    Higher_education_incomplet  = ifelse(`Escolaridade` == 'Superior Incompleto', 1, 0),                                                                   
    Other_Education             = ifelse(`Escolaridade` == 'Não escolarizado', 1, 0),

    Vive_sozinho = ifelse(`Você vive sozinho?` == 'Sim', 1, 0),
    Vitima = ifelse(`Você já foi vítima de violência ou crime?` == 'Sim', 1, 0),
    Medicamento = ifelse(`Você faz uso frequente de algum medicamento?` == 'Sim', 1, 0),
    Pcd = ifelse(`Você é uma pessoa com deﬁciência (PCD)?` == 'Sim', 1, 0),
    
    Seguranca =  case_when (
      `Como você avalia a segurança na sua comunidade?` == 'Muito ruim' ~ 2,                                                                                                                                                                                                                        
      `Como você avalia a segurança na sua comunidade?` == 'Ruim' ~ 1,
      `Como você avalia a segurança na sua comunidade?` == 'Boa' ~ 3,                                                                                                                                                                                                                               
      `Como você avalia a segurança na sua comunidade?` == 'Muito boa' ~ 4
    ),
    
    Particular = ifelse(`Utiliza serviços de saúde do SUS ou de redes particulares?(O SUS (Sistema Único de Saúde) é o sistema público de saúde brasileiro que garante acesso universal, integral e gratuito à saúde para toda a população)` == 'Redes Particulares', 1, 0),                                               
    Sus = ifelse(`Utiliza serviços de saúde do SUS ou de redes particulares?(O SUS (Sistema Único de Saúde) é o sistema público de saúde brasileiro que garante acesso universal, integral e gratuito à saúde para toda a população)` == 'SUS', 1, 0),                                                              
    Sus_Particular = ifelse(`Utiliza serviços de saúde do SUS ou de redes particulares?(O SUS (Sistema Único de Saúde) é o sistema público de saúde brasileiro que garante acesso universal, integral e gratuito à saúde para toda a população)` == 'SUS e Redes Particulares', 1, 0),                                         

    Hipertensao_Diabetes = ifelse(!is.na(`Hipertensão/pressão alta Diabetes`), 1, 0),                                                                                                                                                                                                          
    Depressao = ifelse(!is.na(`Depressão`), 1, 0),                                                                                                                                                                                                                                                                
    Cardiovasculares = ifelse(!is.na(`Doenças cardiovasculares como infarto, derrame ou insuﬁciência cardíaca`), 1, 0),
    Nenhuma_condicao = ifelse(!is.na(`Nenhuma das condição`), 1, 0),                                                                                                                                                                                   
    Outra_Doenca = ifelse(!is.na(`Outro doença (especifique)`), 1, 0),
    
    Familia = ifelse(`Quando você quer saber mais sobre saúde e bem-estar, onde você prefere buscar informações?` == 'Família', 1, 0),
    Outro = ifelse(`Quando você quer saber mais sobre saúde e bem-estar, onde você prefere buscar informações?` == 'Outro (especifique)', 1, 0),
    Profissionais_Saude = ifelse(`Quando você quer saber mais sobre saúde e bem-estar, onde você prefere buscar informações?` == 'Profissionais de saúde pessoalmente', 1, 0),
    Redes_Sociais = ifelse(`Quando você quer saber mais sobre saúde e bem-estar, onde você prefere buscar informações?` == 'Redes sociais', 1, 0),
    Site_Busca = ifelse(`Quando você quer saber mais sobre saúde e bem-estar, onde você prefere buscar informações?` == 'Sites de busca', 1, 0),
    Site_Medicos = ifelse(`Quando você quer saber mais sobre saúde e bem-estar, onde você prefere buscar informações?` == 'Sites médicos', 1, 0)

)

#Peguntas sobre consultas médicos e informações
data <- data %>%
  mutate (
    Saude1 = case_when(
      `Avaliar quando você precisa de uma segunda opinião de outro médico?` == 'Não sei' ~ 1,
      `Avaliar quando você precisa de uma segunda opinião de outro médico?` == 'Muito fácil' ~ 2,
      `Avaliar quando você precisa de uma segunda opinião de outro médico?` == 'Fácil' ~ 3,
      `Avaliar quando você precisa de uma segunda opinião de outro médico?` == 'Difícil' ~ 4,
      `Avaliar quando você precisa de uma segunda opinião de outro médico?` == 'Muito difícil' ~ 5
    ),
    
    Saude2 = case_when(
      `Utilizar as orientações do seu médico para tomar decisões sobre sua condição de saúde?` == 'Não sei' ~ 1,
      `Utilizar as orientações do seu médico para tomar decisões sobre sua condição de saúde?` == 'Muito fácil' ~ 2,
      `Utilizar as orientações do seu médico para tomar decisões sobre sua condição de saúde?` == 'Fácil' ~ 3,
      `Utilizar as orientações do seu médico para tomar decisões sobre sua condição de saúde?` == 'Difícil' ~ 4,
      `Utilizar as orientações do seu médico para tomar decisões sobre sua condição de saúde?` == 'Muito difícil' ~ 5
    ),
    
    Saude3 = case_when(
      `Encontrar informações sobre como lidar com problemas de saúde mental, como o estresse ou depressão?` == 'Não sei' ~ 1,
      `Encontrar informações sobre como lidar com problemas de saúde mental, como o estresse ou depressão?` == 'Muito fácil' ~ 2,
      `Encontrar informações sobre como lidar com problemas de saúde mental, como o estresse ou depressão?` == 'Fácil' ~ 3,
      `Encontrar informações sobre como lidar com problemas de saúde mental, como o estresse ou depressão?` == 'Difícil' ~ 4,
      `Encontrar informações sobre como lidar com problemas de saúde mental, como o estresse ou depressão?` == 'Muito difícil' ~ 5
    ),
    
    Saude4 = case_when(
      `Avaliar se as informações sobre os riscos à saúde disponíveis nos meios de comunicação são confiáveis? (por ex. TV, internet ou outros meios de comunicação)` == 'Não sei' ~ 1,
      `Avaliar se as informações sobre os riscos à saúde disponíveis nos meios de comunicação são confiáveis? (por ex. TV, internet ou outros meios de comunicação)` == 'Muito fácil' ~ 2,
      `Avaliar se as informações sobre os riscos à saúde disponíveis nos meios de comunicação são confiáveis? (por ex. TV, internet ou outros meios de comunicação)` == 'Fácil' ~ 3,
      `Avaliar se as informações sobre os riscos à saúde disponíveis nos meios de comunicação são confiáveis? (por ex. TV, internet ou outros meios de comunicação)` == 'Difícil' ~ 4,
      `Avaliar se as informações sobre os riscos à saúde disponíveis nos meios de comunicação são confiáveis? (por ex. TV, internet ou outros meios de comunicação)` == 'Muito difícil' ~ 5
    ),
    
    Saude5 = case_when(
      `Encontrar informações sobre as atividades que são boas para o seu bem-estar mental? (por ex. meditação, exercício, caminhada, pilates etc.)` == 'Não sei' ~ 1,
      `Encontrar informações sobre as atividades que são boas para o seu bem-estar mental? (por ex. meditação, exercício, caminhada, pilates etc.)` == 'Muito fácil' ~ 2,
      `Encontrar informações sobre as atividades que são boas para o seu bem-estar mental? (por ex. meditação, exercício, caminhada, pilates etc.)` == 'Fácil' ~ 3,
      `Encontrar informações sobre as atividades que são boas para o seu bem-estar mental? (por ex. meditação, exercício, caminhada, pilates etc.)` == 'Difícil' ~ 4,
      `Encontrar informações sobre as atividades que são boas para o seu bem-estar mental? (por ex. meditação, exercício, caminhada, pilates etc.)` == 'Muito difícil' ~ 5
    ),
    
    Saude6 = case_when(
      `Entender as informações disponíveis nos meios de comunicação sobre como ficar mais saudável? (por ex. internet, jornais, revistas)` == 'Não sei' ~ 1,
      `Entender as informações disponíveis nos meios de comunicação sobre como ficar mais saudável? (por ex. internet, jornais, revistas)` == 'Muito fácil' ~ 2,
      `Entender as informações disponíveis nos meios de comunicação sobre como ficar mais saudável? (por ex. internet, jornais, revistas)` == 'Fácil' ~ 3,
      `Entender as informações disponíveis nos meios de comunicação sobre como ficar mais saudável? (por ex. internet, jornais, revistas)` == 'Difícil' ~ 4,
      `Entender as informações disponíveis nos meios de comunicação sobre como ficar mais saudável? (por ex. internet, jornais, revistas)` == 'Muito difícil' ~ 5
    ),
    
    Saude7 = case_when(
      `Quando se trata de saúde, eu sei quais dados/informações/recursos estão disponíveis na internet.` == 'Não tenho certeza' ~ 1,
      `Quando se trata de saúde, eu sei quais dados/informações/recursos estão disponíveis na internet.` == 'Discordo totalmente' ~ 2,
      `Quando se trata de saúde, eu sei quais dados/informações/recursos estão disponíveis na internet.` == 'Discordo em parte' ~ 3,
      `Quando se trata de saúde, eu sei quais dados/informações/recursos estão disponíveis na internet.` == 'Concordo em parte' ~ 4,
      `Quando se trata de saúde, eu sei quais dados/informações/recursos estão disponíveis na internet.` == 'Concordo totalmente' ~ 5
    ),
    
    Saude8 = case_when(
      `Eu sei onde encontrar recursos de saúde úteis na internet.` == 'Não tenho certeza' ~ 1,
      `Eu sei onde encontrar recursos de saúde úteis na internet.` == 'Discordo totalmente' ~ 2,
      `Eu sei onde encontrar recursos de saúde úteis na internet.` == 'Discordo em parte' ~ 3,
      `Eu sei onde encontrar recursos de saúde úteis na internet.` == 'Concordo em parte' ~ 4,
      `Eu sei onde encontrar recursos de saúde úteis na internet.` == 'Concordo totalmente' ~ 5
    ),
    
    Saude9 = case_when(
      `Eu sei como encontrar recursos de saúde úteis na internet.` == 'Não tenho certeza' ~ 1,
      `Eu sei como encontrar recursos de saúde úteis na internet.` == 'Discordo totalmente' ~ 2,
      `Eu sei como encontrar recursos de saúde úteis na internet.` == 'Discordo em parte' ~ 3,
      `Eu sei como encontrar recursos de saúde úteis na internet.` == 'Concordo em parte' ~ 4,
      `Eu sei como encontrar recursos de saúde úteis na internet.` == 'Concordo totalmente' ~ 5
    ),
    
    Saude10 = case_when(
      `Eu sei como usar a internet para esclarecer minhas dúvidas sobre saúde.` == 'Não tenho certeza' ~ 1,
      `Eu sei como usar a internet para esclarecer minhas dúvidas sobre saúde.` == 'Discordo totalmente' ~ 2,
      `Eu sei como usar a internet para esclarecer minhas dúvidas sobre saúde.` == 'Discordo em parte' ~ 3,
      `Eu sei como usar a internet para esclarecer minhas dúvidas sobre saúde.` == 'Concordo em parte' ~ 4,
      `Eu sei como usar a internet para esclarecer minhas dúvidas sobre saúde.` == 'Concordo totalmente' ~ 5
    ),
    
    Saude11 = case_when(
      `Eu sei como usar as informações sobre saúde que encontro na internet para me ajudar.` == 'Não tenho certeza' ~ 1,
      `Eu sei como usar as informações sobre saúde que encontro na internet para me ajudar.` == 'Discordo totalmente' ~ 2,
      `Eu sei como usar as informações sobre saúde que encontro na internet para me ajudar.` == 'Discordo em parte' ~ 3,
      `Eu sei como usar as informações sobre saúde que encontro na internet para me ajudar.` == 'Concordo em parte' ~ 4,
      `Eu sei como usar as informações sobre saúde que encontro na internet para me ajudar.` == 'Concordo totalmente' ~ 5
    ),
    
    Saude12 = case_when(
      `Eu domino todas as habilidades necessárias para avaliar dados/informações/recursos de saúde que encontrei na internet.` == 'Não tenho certeza' ~ 1,
      `Eu domino todas as habilidades necessárias para avaliar dados/informações/recursos de saúde que encontrei na internet.` == 'Discordo totalmente' ~ 2,
      `Eu domino todas as habilidades necessárias para avaliar dados/informações/recursos de saúde que encontrei na internet.` == 'Discordo em parte' ~ 3,
      `Eu domino todas as habilidades necessárias para avaliar dados/informações/recursos de saúde que encontrei na internet.` == 'Concordo em parte' ~ 4,
      `Eu domino todas as habilidades necessárias para avaliar dados/informações/recursos de saúde que encontrei na internet.` == 'Concordo totalmente' ~ 5
    ),
    
    Saude13 = case_when(
      `Eu consigo diferenciar os recursos de saúde que são de alta qualidade dos que são de baixa qualidade na internet.` == 'Não tenho certeza' ~ 1,
      `Eu consigo diferenciar os recursos de saúde que são de alta qualidade dos que são de baixa qualidade na internet.` == 'Discordo totalmente' ~ 2,
      `Eu consigo diferenciar os recursos de saúde que são de alta qualidade dos que são de baixa qualidade na internet.` == 'Discordo em parte' ~ 3,
      `Eu consigo diferenciar os recursos de saúde que são de alta qualidade dos que são de baixa qualidade na internet.` == 'Concordo em parte' ~ 4,
      `Eu consigo diferenciar os recursos de saúde que são de alta qualidade dos que são de baixa qualidade na internet.` == 'Concordo totalmente' ~ 5
    ),
    
    Saude14 = case_when(
      `Eu me sinto seguro ao usar informações da internet para tomar decisões relacionadas à saúde.` == 'Não tenho certeza' ~ 1,
      `Eu me sinto seguro ao usar informações da internet para tomar decisões relacionadas à saúde.` == 'Discordo totalmente' ~ 2,
      `Eu me sinto seguro ao usar informações da internet para tomar decisões relacionadas à saúde.` == 'Discordo em parte' ~ 3,
      `Eu me sinto seguro ao usar informações da internet para tomar decisões relacionadas à saúde.` == 'Concordo em parte' ~ 4,
      `Eu me sinto seguro ao usar informações da internet para tomar decisões relacionadas à saúde.` == 'Concordo totalmente' ~ 5
    )
)
