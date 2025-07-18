data <- read_excel("Grupo2.xlsx")

data <- data %>%
  mutate (
    
    Gender_male = ifelse(`Qual é o seu gênero?` == 'Homem', 1, 0),                                                                                                              
    Gender_female = ifelse(`Qual é o seu gênero?` == 'Mulher', 1, 0),                                                                                                             
    Gender_Other = ifelse(`Qual é o seu gênero?` == 'Outro', 1, 0),  
    
    Age_18_24 = ifelse(`Eu tenho entre 18 e 24 anos` == 'Sim', 1, 0),
    
    employment_employee = ifelse(`Qual é a sua situação empregatícia?` == 'Empregado(a)', 1, 0),                                                                                        
    employment_studant = ifelse(`Qual é a sua situação empregatícia?` == 'Estudando ou em treinamento', 1, 0),                                                                         
    employment_no_employee = ifelse(`Qual é a sua situação empregatícia?` == 'Não estou empregado(a), nem estudando ou em treinamento', 1, 0),
    
    primary_education     = ifelse(`Qual é o nível de escolaridade da sua mãe?` == 'Ensino Fundamental', 1, 0),
    High_school           = ifelse(`Qual é o nível de escolaridade da sua mãe?` == 'Ensino Médio Completo', 1, 0),                                                                        
    High_school_incomplet = ifelse(`Qual é o nível de escolaridade da sua mãe?` == 'Ensino Médio Incompleto', 1, 0),                                                                      
    higher_education      = ifelse(`Qual é o nível de escolaridade da sua mãe?` == 'Ensino Superior Completo', 1, 0),                                                                     
    higher_education_incomplet    = ifelse(`Qual é o nível de escolaridade da sua mãe?` == 'Ensino Superior Incompleto', 1, 0),                                                                   
    other_Education       = ifelse(`Qual é o nível de escolaridade da sua mãe?` == 'Nenhuma das anteriores', 1, 0),                                                                       
    Postgraduate          = ifelse(`Qual é o nível de escolaridade da sua mãe?` == 'Pós-graduação Completa', 1, 0),  
    
    Residence_Rural = ifelse(`Qual é a sua área de residência?` == 'Rural', 1, 0),                                                                                                  
    Residence_Urban = ifelse(`Qual é a sua área de residência?` == 'Urbana', 1, 0),  
    
    Race_Yellow = ifelse(`Qual é a sua raça/etnia?` == 'Amarelo', 1, 0),                                                                                                        
    Race_White = ifelse(`Qual é a sua raça/etnia?` == 'Branco', 1, 0),                                                                                                         
    Race_Indigenous = ifelse(`Qual é a sua raça/etnia?` == 'Indígena', 1, 0),                                                                                                       
    Race_Brown = ifelse(`Qual é a sua raça/etnia?` == 'Pardo', 1, 0),                                                                                                          
    Race_Black = ifelse(`Qual é a sua raça/etnia?` == 'Preto', 1, 0)
    
  )

#Comun question to all Groups
data <- data %>%
  mutate(
    volunteer_work = ifelse(`Você já fez trabalho voluntário para uma organização ambiental?` == 'Sim', 1, 0),
    eat_meat = ifelse(`Você come carne (ou seja, bovina, suína ou de frango)?` == 'Sim', 1, 0),
    eat_fish = ifelse(`Você come peixe?` == 'Sim', 1, 0),
    eat_dairy_eggs = ifelse(`Você come ovos ou laticínios (por exemplo, leite, queijo, iogurte)?` == 'Sim', 1, 0),
    environmental_activism = ifelse(`Você já se envolveu em ativismo ambiental? (Por exemplo, Todos Pela Amazônia, se recusa a comprar produtos específicos, protestos, etc.)` == 'Sim', 1, 0),
    
    transport_walk_bike = ifelse(`Como você se locomove no dia a dia? (por exemplo, para ir à escola/faculdade/trabalho, ir ao supermercado, visitar amigos)` == 'Na maioria das vezes, ando a pé ou de bicicleta', 1, 0),
    transport_car_alone = ifelse(`Como você se locomove no dia a dia? (por exemplo, para ir à escola/faculdade/trabalho, ir ao supermercado, visitar amigos)` == 'Na maioria das vezes, pego carona ou dirijo sozinho', 1, 0),
    transport_public = ifelse(`Como você se locomove no dia a dia? (por exemplo, para ir à escola/faculdade/trabalho, ir ao supermercado, visitar amigos)` == 'Na maioria das vezes, uso o transporte público', 1, 0),
    
    buy_some_secondhand = ifelse(`Qual das opções a seguir melhor descreve como você compra coisas novas (por exemplo, roupas)?` == 'Compro (ou outra pessoa compra para mim) algumas de minhas coisas novas, mas às vezes compro de segunda mão ou reutilizo coisas antigas', 1, 0),
    buy_most_new = ifelse(`Qual das opções a seguir melhor descreve como você compra coisas novas (por exemplo, roupas)?` == 'Eu compro (ou outra pessoa compra para mim) a maioria ou todas as minhas coisas novas', 1, 0),
    buy_rarely = ifelse(`Qual das opções a seguir melhor descreve como você compra coisas novas (por exemplo, roupas)?` == 'Eu compro (ou outra pessoa compra para mim) coisas novas muito raramente', 1, 0),
    
    recycling_bin_no = ifelse(`Sua família/domicílio usa uma lixeira separada para reciclagem? Se você mora em mais de uma residência, pense em onde você passa a maior parte do tempo.` == 'Não', 1, 0),
    recycling_bin_dont_know = ifelse(`Sua família/domicílio usa uma lixeira separada para reciclagem? Se você mora em mais de uma residência, pense em onde você passa a maior parte do tempo.` == 'Não sei', 1, 0),
    recycling_bin_yes = ifelse(`Sua família/domicílio usa uma lixeira separada para reciclagem? Se você mora em mais de uma residência, pense em onde você passa a maior parte do tempo.` == 'Sim', 1, 0),
    
    food_waste_bin_dont_know = ifelse(`Sua família/domicílio usa uma lixeira separada para resíduos de alimentos (por exemplo, sobras, cascas de frutas, cascas de ovos)? Se você mora em mais de uma residência, pense em onde você passa a maior parte do tempo.` == 'Não sei', 1, 0),
    food_waste_bin_no = ifelse(`Sua família/domicílio usa uma lixeira separada para resíduos de alimentos (por exemplo, sobras, cascas de frutas, cascas de ovos)? Se você mora em mais de uma residência, pense em onde você passa a maior parte do tempo.` == 'Não, os resíduos de alimentos são colocados junto com o lixo geral', 1, 0),
    food_waste_bin_yes = ifelse(`Sua família/domicílio usa uma lixeira separada para resíduos de alimentos (por exemplo, sobras, cascas de frutas, cascas de ovos)? Se você mora em mais de uma residência, pense em onde você passa a maior parte do tempo.` == 'Sim, há uma lixeira separada para resíduos de alimentos ou uma lixeira de compostagem', 1, 0),
    
    nature_reason_not_important = ifelse(`Gostaríamos de saber qual você acha que é o motivo mais importante para proteger a natureza. Por favor, escolha um motivo da lista a seguir:` == 'Não acho que seja importante proteger a natureza', 1, 0),
    nature_reason_other = ifelse(`Gostaríamos de saber qual você acha que é o motivo mais importante para proteger a natureza. Por favor, escolha um motivo da lista a seguir:` == 'Outro motivo', 1, 0),
    nature_reason_disasters = ifelse(`Gostaríamos de saber qual você acha que é o motivo mais importante para proteger a natureza. Por favor, escolha um motivo da lista a seguir:` == 'Para evitar desastres naturais (por exemplo, enchentes)', 1, 0),
    nature_reason_future_generations = ifelse(`Gostaríamos de saber qual você acha que é o motivo mais importante para proteger a natureza. Por favor, escolha um motivo da lista a seguir:` == 'Para o bem das gerações futuras', 1, 0),
    nature_reason_animals_plants = ifelse(`Gostaríamos de saber qual você acha que é o motivo mais importante para proteger a natureza. Por favor, escolha um motivo da lista a seguir:` == 'Para proteger animais e plantas (não humanos) (por exemplo, impedir a extinção de animais)', 1, 0),
    nature_reason_beauty = ifelse(`Gostaríamos de saber qual você acha que é o motivo mais importante para proteger a natureza. Por favor, escolha um motivo da lista a seguir:` == 'Para que possamos continuar a desfrutar da beleza da natureza', 1, 0),
    nature_reason_clean_area = ifelse(`Gostaríamos de saber qual você acha que é o motivo mais importante para proteger a natureza. Por favor, escolha um motivo da lista a seguir:` == 'Para ter uma área local limpa (por exemplo, ter ar e rios limpos)', 1, 0)
  )

#Quais das seguintes opções de áreas ao ar livre existem na sua área local? (Selecione todas que se aplicam)
data <- data %>%
  mutate(
    Park_or_similar = ifelse(!is.na(`Parque/área de lazer ou similar`), 1, 0),
    Beach = ifelse(!is.na(`Praia`), 1, 0),
    Path_along_water = ifelse(!is.na(`Caminho/ciclofaixa ao longo de um lago ou rio`), 1, 0),
    Mountain_trail = ifelse(!is.na(`Caminho em montanha ou colina`), 1, 0),
    Forest_trail = ifelse(!is.na(`Caminho em bosque/floresta`), 1, 0),
    Other_place = ifelse(!is.na(`Outra coisa`), 1, 0),
    No_idea = ifelse(!is.na(`Não consigo pensar em nenhuma`), 1, 0)
  )

#Escalas
data <- data %>%
  mutate (
    Climate_Change1 = case_when (
      `Leia atentamente as informações a seguir, pois haverá perguntas sobre elas na próxima seção.A próxima parte do estudo é especificamente sobre mudanças climáticas. Mudanças climáticas referem-se a alterações de longo prazo no clima de uma região (por exemplo, um país). Isso pode significar mudanças duradouras (décadas ou mais) na temperatura (dias mais quentes ou mais frios), na quantidade de chuva ou na intensidade dos ventos.As mudanças climáticas são causadas pelas decisões e comportamentos das pessoas. Quase todos os cientistas concordam que as mudanças climáticas estão ocorrendo e que os efeitos já podem ser observados ao redor do mundo. Alguns dos piores efeitos incluem eventos climáticos extremos, como tempestades, secas e inundações.Pessoas em todo o mundo começaram a levantar a questão sobre as mudanças climáticas e a exigir que os tomadores de decisão comecem a agir de forma real e urgente.Nesta seção, estamos interessados em seus pensamentos e opiniões.Quão importante você acha que é proteger o meio ambiente?` == '1 (Nada importante)' ~ 1,       
      `Leia atentamente as informações a seguir, pois haverá perguntas sobre elas na próxima seção.A próxima parte do estudo é especificamente sobre mudanças climáticas. Mudanças climáticas referem-se a alterações de longo prazo no clima de uma região (por exemplo, um país). Isso pode significar mudanças duradouras (décadas ou mais) na temperatura (dias mais quentes ou mais frios), na quantidade de chuva ou na intensidade dos ventos.As mudanças climáticas são causadas pelas decisões e comportamentos das pessoas. Quase todos os cientistas concordam que as mudanças climáticas estão ocorrendo e que os efeitos já podem ser observados ao redor do mundo. Alguns dos piores efeitos incluem eventos climáticos extremos, como tempestades, secas e inundações.Pessoas em todo o mundo começaram a levantar a questão sobre as mudanças climáticas e a exigir que os tomadores de decisão comecem a agir de forma real e urgente.Nesta seção, estamos interessados em seus pensamentos e opiniões.Quão importante você acha que é proteger o meio ambiente?` == '2' ~ 2,
      `Leia atentamente as informações a seguir, pois haverá perguntas sobre elas na próxima seção.A próxima parte do estudo é especificamente sobre mudanças climáticas. Mudanças climáticas referem-se a alterações de longo prazo no clima de uma região (por exemplo, um país). Isso pode significar mudanças duradouras (décadas ou mais) na temperatura (dias mais quentes ou mais frios), na quantidade de chuva ou na intensidade dos ventos.As mudanças climáticas são causadas pelas decisões e comportamentos das pessoas. Quase todos os cientistas concordam que as mudanças climáticas estão ocorrendo e que os efeitos já podem ser observados ao redor do mundo. Alguns dos piores efeitos incluem eventos climáticos extremos, como tempestades, secas e inundações.Pessoas em todo o mundo começaram a levantar a questão sobre as mudanças climáticas e a exigir que os tomadores de decisão comecem a agir de forma real e urgente.Nesta seção, estamos interessados em seus pensamentos e opiniões.Quão importante você acha que é proteger o meio ambiente?` == '3' ~ 3,
      `Leia atentamente as informações a seguir, pois haverá perguntas sobre elas na próxima seção.A próxima parte do estudo é especificamente sobre mudanças climáticas. Mudanças climáticas referem-se a alterações de longo prazo no clima de uma região (por exemplo, um país). Isso pode significar mudanças duradouras (décadas ou mais) na temperatura (dias mais quentes ou mais frios), na quantidade de chuva ou na intensidade dos ventos.As mudanças climáticas são causadas pelas decisões e comportamentos das pessoas. Quase todos os cientistas concordam que as mudanças climáticas estão ocorrendo e que os efeitos já podem ser observados ao redor do mundo. Alguns dos piores efeitos incluem eventos climáticos extremos, como tempestades, secas e inundações.Pessoas em todo o mundo começaram a levantar a questão sobre as mudanças climáticas e a exigir que os tomadores de decisão comecem a agir de forma real e urgente.Nesta seção, estamos interessados em seus pensamentos e opiniões.Quão importante você acha que é proteger o meio ambiente?` == '4' ~ 4,                         
      `Leia atentamente as informações a seguir, pois haverá perguntas sobre elas na próxima seção.A próxima parte do estudo é especificamente sobre mudanças climáticas. Mudanças climáticas referem-se a alterações de longo prazo no clima de uma região (por exemplo, um país). Isso pode significar mudanças duradouras (décadas ou mais) na temperatura (dias mais quentes ou mais frios), na quantidade de chuva ou na intensidade dos ventos.As mudanças climáticas são causadas pelas decisões e comportamentos das pessoas. Quase todos os cientistas concordam que as mudanças climáticas estão ocorrendo e que os efeitos já podem ser observados ao redor do mundo. Alguns dos piores efeitos incluem eventos climáticos extremos, como tempestades, secas e inundações.Pessoas em todo o mundo começaram a levantar a questão sobre as mudanças climáticas e a exigir que os tomadores de decisão comecem a agir de forma real e urgente.Nesta seção, estamos interessados em seus pensamentos e opiniões.Quão importante você acha que é proteger o meio ambiente?` == '5' ~ 5,                         
      `Leia atentamente as informações a seguir, pois haverá perguntas sobre elas na próxima seção.A próxima parte do estudo é especificamente sobre mudanças climáticas. Mudanças climáticas referem-se a alterações de longo prazo no clima de uma região (por exemplo, um país). Isso pode significar mudanças duradouras (décadas ou mais) na temperatura (dias mais quentes ou mais frios), na quantidade de chuva ou na intensidade dos ventos.As mudanças climáticas são causadas pelas decisões e comportamentos das pessoas. Quase todos os cientistas concordam que as mudanças climáticas estão ocorrendo e que os efeitos já podem ser observados ao redor do mundo. Alguns dos piores efeitos incluem eventos climáticos extremos, como tempestades, secas e inundações.Pessoas em todo o mundo começaram a levantar a questão sobre as mudanças climáticas e a exigir que os tomadores de decisão comecem a agir de forma real e urgente.Nesta seção, estamos interessados em seus pensamentos e opiniões.Quão importante você acha que é proteger o meio ambiente?` == '6' ~ 6,                         
      `Leia atentamente as informações a seguir, pois haverá perguntas sobre elas na próxima seção.A próxima parte do estudo é especificamente sobre mudanças climáticas. Mudanças climáticas referem-se a alterações de longo prazo no clima de uma região (por exemplo, um país). Isso pode significar mudanças duradouras (décadas ou mais) na temperatura (dias mais quentes ou mais frios), na quantidade de chuva ou na intensidade dos ventos.As mudanças climáticas são causadas pelas decisões e comportamentos das pessoas. Quase todos os cientistas concordam que as mudanças climáticas estão ocorrendo e que os efeitos já podem ser observados ao redor do mundo. Alguns dos piores efeitos incluem eventos climáticos extremos, como tempestades, secas e inundações.Pessoas em todo o mundo começaram a levantar a questão sobre as mudanças climáticas e a exigir que os tomadores de decisão comecem a agir de forma real e urgente.Nesta seção, estamos interessados em seus pensamentos e opiniões.Quão importante você acha que é proteger o meio ambiente?` == '7 (Extremamente importante)' ~ 7
    ),
    Climate_Change2 =  case_when (
      `De modo geral, quão preocupado(a) você está com as mudanças climáticas?` == '1 (Nada preocupado)' ~ 1,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      `De modo geral, quão preocupado(a) você está com as mudanças climáticas?` == '2' ~ 2,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      `De modo geral, quão preocupado(a) você está com as mudanças climáticas?` == '3' ~ 3,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      `De modo geral, quão preocupado(a) você está com as mudanças climáticas?` == '4' ~ 4,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
      `De modo geral, quão preocupado(a) você está com as mudanças climáticas?` == '5' ~ 5,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `De modo geral, quão preocupado(a) você está com as mudanças climáticas?` == '6' ~ 6,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `De modo geral, quão preocupado(a) você está com as mudanças climáticas?` == '7 (Extremamente preocupado)' ~ 7
    ),
    Climate_Change3 =  case_when (	
      `Quão preocupadas você acha que estão as pessoas no Brasil com 40 anos ou mais em relação às mudanças climáticas?` == '1 (Nada preocupadas)' ~ 1,                                                                                                                                                                                                                              
      `Quão preocupadas você acha que estão as pessoas no Brasil com 40 anos ou mais em relação às mudanças climáticas?` == '2' ~ 2,                                                                                                                                                                                                                                                 
      `Quão preocupadas você acha que estão as pessoas no Brasil com 40 anos ou mais em relação às mudanças climáticas?` == '3' ~ 3,                                                                                                                                                                                                                                                 
      `Quão preocupadas você acha que estão as pessoas no Brasil com 40 anos ou mais em relação às mudanças climáticas?` == '4' ~ 4,                                                                                                                                                                                                                                                 
      `Quão preocupadas você acha que estão as pessoas no Brasil com 40 anos ou mais em relação às mudanças climáticas?` == '5' ~ 5,                                                                                                                                                                                                                                                 
      `Quão preocupadas você acha que estão as pessoas no Brasil com 40 anos ou mais em relação às mudanças climáticas?` == '6' ~ 6,                                                                                                                                                                                                                                                 
      `Quão preocupadas você acha que estão as pessoas no Brasil com 40 anos ou mais em relação às mudanças climáticas?` == '7 (Extremamente preocupadas)' ~ 7 
    ),
    Climate_Change4 =  case_when (																																																							 
      `Também gostaríamos de saber o que você acha que seus colegas (ou seja, pessoas da sua idade) pensam sobre as mudanças climáticas.O quanto você acha que pessoas da sua idade em geral no Brasil estão preocupados com as mudanças climáticas?` == '1 (Nada preocupados)' ~ 1,                                                                                                               
      `Também gostaríamos de saber o que você acha que seus colegas (ou seja, pessoas da sua idade) pensam sobre as mudanças climáticas.O quanto você acha que pessoas da sua idade em geral no Brasil estão preocupados com as mudanças climáticas?` == '2' ~ 2,                                                                                                                                  
      `Também gostaríamos de saber o que você acha que seus colegas (ou seja, pessoas da sua idade) pensam sobre as mudanças climáticas.O quanto você acha que pessoas da sua idade em geral no Brasil estão preocupados com as mudanças climáticas?` == '3' ~ 3,                                                                                                                                  
      `Também gostaríamos de saber o que você acha que seus colegas (ou seja, pessoas da sua idade) pensam sobre as mudanças climáticas.O quanto você acha que pessoas da sua idade em geral no Brasil estão preocupados com as mudanças climáticas?` == '4' ~ 4,                                                                                                                                  
      `Também gostaríamos de saber o que você acha que seus colegas (ou seja, pessoas da sua idade) pensam sobre as mudanças climáticas.O quanto você acha que pessoas da sua idade em geral no Brasil estão preocupados com as mudanças climáticas?` == '5' ~ 5,                                                                                                                                  
      `Também gostaríamos de saber o que você acha que seus colegas (ou seja, pessoas da sua idade) pensam sobre as mudanças climáticas.O quanto você acha que pessoas da sua idade em geral no Brasil estão preocupados com as mudanças climáticas?` == '6' ~ 6,                                                                                                                                  
      `Também gostaríamos de saber o que você acha que seus colegas (ou seja, pessoas da sua idade) pensam sobre as mudanças climáticas.O quanto você acha que pessoas da sua idade em geral no Brasil estão preocupados com as mudanças climáticas?` == '7 (Extremamente preocupados)' ~ 7                                                                                                      
      
    ),
    Climate_Change5 =  case_when (	
      `Quão preocupados você acha que estão seus amigos próximos em relação às mudanças climáticas?` == '1 (Nada preocupados)' ~ 1,                                                                                                                                                                                                                                                  
      `Quão preocupados você acha que estão seus amigos próximos em relação às mudanças climáticas?` == '2' ~ 2,                                                                                                                                                                                                                                                                     
      `Quão preocupados você acha que estão seus amigos próximos em relação às mudanças climáticas?` == '3' ~ 3,                                                                                                                                                                                                                                                                     
      `Quão preocupados você acha que estão seus amigos próximos em relação às mudanças climáticas?` == '4' ~ 4,                                                                                                                                                                                                                                                                     
      `Quão preocupados você acha que estão seus amigos próximos em relação às mudanças climáticas?` == '5' ~ 5,                                                                                                                                                                                                                                                                     
      `Quão preocupados você acha que estão seus amigos próximos em relação às mudanças climáticas?` == '6' ~ 6,                                                                                                                                                                                                                                                                     
      `Quão preocupados você acha que estão seus amigos próximos em relação às mudanças climáticas?` == '7 (Extremamente preocupados)' ~ 7                                                                                                                                                                                                                                          
    ),
    Climate_Change6 =  case_when (	
      `Um estudo demostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão surpreso(a) você está com esses resultados?` == '1 (Nada surpreso)' ~ 1,        
      `Um estudo demostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão surpreso(a) você está com esses resultados?` == '2' ~ 2,                        
      `Um estudo demostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão surpreso(a) você está com esses resultados?` == '3' ~ 3,                        
      `Um estudo demostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão surpreso(a) você está com esses resultados?` == '4' ~ 4,                        
      `Um estudo demostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão surpreso(a) você está com esses resultados?` == '5' ~ 5,                        
      `Um estudo demostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão surpreso(a) você está com esses resultados?` == '6' ~ 6,                        
      `Um estudo demostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão surpreso(a) você está com esses resultados?` == '7 (Extremamente surpreso)' ~ 7
    ),
    Climate_Change7 =  case_when (
      `Considerando o que você pensa sobre o quão preocupadas as outras pessoas estão com as mudanças climáticas, o quanto você discorda ou concorda com o seguinte?É provável que a maioria das pessoas no Brasil faça sua parte no esforço conjunto para combater as mudanças climáticas` == '1 (Discordo completamente)' ~ 1,                                                                                                                                                                                                                  
      `Considerando o que você pensa sobre o quão preocupadas as outras pessoas estão com as mudanças climáticas, o quanto você discorda ou concorda com o seguinte?É provável que a maioria das pessoas no Brasil faça sua parte no esforço conjunto para combater as mudanças climáticas` == '2' ~ 2,                                                                                                                                                                                                                                           
      `Considerando o que você pensa sobre o quão preocupadas as outras pessoas estão com as mudanças climáticas, o quanto você discorda ou concorda com o seguinte?É provável que a maioria das pessoas no Brasil faça sua parte no esforço conjunto para combater as mudanças climáticas` == '3' ~ 3,                                                                                                                                                                                                                                           
      `Considerando o que você pensa sobre o quão preocupadas as outras pessoas estão com as mudanças climáticas, o quanto você discorda ou concorda com o seguinte?É provável que a maioria das pessoas no Brasil faça sua parte no esforço conjunto para combater as mudanças climáticas` == '4' ~ 4,                                                                                                                                                                                                                                           
      `Considerando o que você pensa sobre o quão preocupadas as outras pessoas estão com as mudanças climáticas, o quanto você discorda ou concorda com o seguinte?É provável que a maioria das pessoas no Brasil faça sua parte no esforço conjunto para combater as mudanças climáticas` == '5' ~ 5,                                                                                                                                                                                                                                           
      `Considerando o que você pensa sobre o quão preocupadas as outras pessoas estão com as mudanças climáticas, o quanto você discorda ou concorda com o seguinte?É provável que a maioria das pessoas no Brasil faça sua parte no esforço conjunto para combater as mudanças climáticas` == '6' ~ 6,                                                                                                                                                                                                                                           
      `Considerando o que você pensa sobre o quão preocupadas as outras pessoas estão com as mudanças climáticas, o quanto você discorda ou concorda com o seguinte?É provável que a maioria das pessoas no Brasil faça sua parte no esforço conjunto para combater as mudanças climáticas` == '7 (Concordo completamente)' ~ 7
    ),
    Climate_Change8 =  case_when (	
      `É provável que os jovens no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '1 (Discordo completamente)' ~ 1,                                                                                                                                                                                                                            
      `É provável que os jovens no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '2' ~ 2,                                                                                                                                                                                                                                                     
      `É provável que os jovens no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '3' ~ 3,                                                                                                                                                                                                                                                     
      `É provável que os jovens no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '4' ~ 4,                                                                                                                                                                                                                                                     
      `É provável que os jovens no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '5' ~ 5,                                                                                                                                                                                                                                                     
      `É provável que os jovens no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '6' ~ 6,                                                                                                                                                                                                                                                     
      `É provável que os jovens no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '7 (Concordo completamente)' ~ 7                                                                                                                                                                                                                            
    ),
    Climate_Change9 =  case_when (	
      `É provável que as pessoas mais velhas no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '1 (Discordo completamente)' ~ 1,                                                                                                                                                                                                               
      `É provável que as pessoas mais velhas no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '2' ~ 2,                                                                                                                                                                                                                                        
      `É provável que as pessoas mais velhas no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '3' ~ 3,                                                                                                                                                                                                                                        
      `É provável que as pessoas mais velhas no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '4' ~ 4,                                                                                                                                                                                                                                        
      `É provável que as pessoas mais velhas no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '5' ~ 5,                                                                                                                                                                                                                                        
      `É provável que as pessoas mais velhas no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '6' ~ 6,                                                                                                                                                                                                                                        
      `É provável que as pessoas mais velhas no Brasil façam sua parte no esforço conjunto para combater as mudanças climáticas.` == '7 (Concordo completamente)' ~ 7
    ),
    Climate_Change10 = case_when (
      `Se as pessoas no Brasil fizerem a sua parte, juntas podem ajudar a combater as mudanças climáticas.` == '1 (Discordo completamente)' ~ 1,                                                                                                                                                                                                                                                                                                                                                                                                                                                             
      `Se as pessoas no Brasil fizerem a sua parte, juntas podem ajudar a combater as mudanças climáticas.` == '2' ~ 2,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      `Se as pessoas no Brasil fizerem a sua parte, juntas podem ajudar a combater as mudanças climáticas.` == '3' ~ 3,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      `Se as pessoas no Brasil fizerem a sua parte, juntas podem ajudar a combater as mudanças climáticas.` == '4' ~ 4,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      `Se as pessoas no Brasil fizerem a sua parte, juntas podem ajudar a combater as mudanças climáticas.` == '5' ~ 5,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      `Se as pessoas no Brasil fizerem a sua parte, juntas podem ajudar a combater as mudanças climáticas.` == '6' ~ 6,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
      `Se as pessoas no Brasil fizerem a sua parte, juntas podem ajudar a combater as mudanças climáticas.` == '7 (Concordo completamente)' ~ 7     
    ),
    Climate_Change11 = case_when (                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que eu posso fazer na minha rotina diária para ajudar a combater as mudanças climáticas.` == '1 (Discordo completamente)' ~ 1,                                                                                                                                                                                                                                                                                                                                                                                                                                                  
      `Há coisas que eu posso fazer na minha rotina diária para ajudar a combater as mudanças climáticas.` == '2' ~ 2,                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
      `Há coisas que eu posso fazer na minha rotina diária para ajudar a combater as mudanças climáticas.` == '3' ~ 3,                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
      `Há coisas que eu posso fazer na minha rotina diária para ajudar a combater as mudanças climáticas.` == '4' ~ 4,                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
      `Há coisas que eu posso fazer na minha rotina diária para ajudar a combater as mudanças climáticas.` == '5' ~ 5,                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
      `Há coisas que eu posso fazer na minha rotina diária para ajudar a combater as mudanças climáticas.` == '6' ~ 6,                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
      `Há coisas que eu posso fazer na minha rotina diária para ajudar a combater as mudanças climáticas.` == '7 (Concordo completamente)' ~ 7   
    ),
    Climate_Change12 = case_when (                                                                                                                                                                                                                                                                                                                                                                                                                                              
      `Há coisas que pessoas mais velhas podem fazer na sua rotina diária para ajudar a combater as mudanças climáticas.` == '1 (Discordo completamente)' ~ 1,                                                                                                                                                                                                                                                                                                                                                                                                                                               
      `Há coisas que pessoas mais velhas podem fazer na sua rotina diária para ajudar a combater as mudanças climáticas.` == '2' ~ 2,                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que pessoas mais velhas podem fazer na sua rotina diária para ajudar a combater as mudanças climáticas.` == '3' ~ 3,                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que pessoas mais velhas podem fazer na sua rotina diária para ajudar a combater as mudanças climáticas.` == '4' ~ 4,                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que pessoas mais velhas podem fazer na sua rotina diária para ajudar a combater as mudanças climáticas.` == '5' ~ 5,                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que pessoas mais velhas podem fazer na sua rotina diária para ajudar a combater as mudanças climáticas.` == '6' ~ 6,                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que pessoas mais velhas podem fazer na sua rotina diária para ajudar a combater as mudanças climáticas.` == '7 (Concordo completamente)' ~ 7
    ),
    Climate_Change13 = case_when (                                                                                                                                                                                                                                                                                                                                                                                                                                              
      `Há coisas que o governo pode fazer para ajudar a combater as mudanças climáticas.` == '1 (Discordo completamente)' ~ 1,                                                                                                                                                                                                                                                                                                                                                     
      `Há coisas que o governo pode fazer para ajudar a combater as mudanças climáticas.` == '2' ~ 2,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que o governo pode fazer para ajudar a combater as mudanças climáticas.` == '3' ~ 3,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que o governo pode fazer para ajudar a combater as mudanças climáticas.` == '4' ~ 4,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que o governo pode fazer para ajudar a combater as mudanças climáticas.` == '5' ~ 5,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `Há coisas que o governo pode fazer para ajudar a combater as mudanças climáticas.` == '6' ~ 6,                                                                                                                                                                                                                                                                                                                                                                                              
      `Há coisas que o governo pode fazer para ajudar a combater as mudanças climáticas.` == '7 (Concordo completamente)' ~ 7  
    ),
    Climate_Change14 = case_when (                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
      `No mês passado, com que frequência você passou tempo nas áreas ao ar livre em sua região?` == 'Algumas vezes por mês' ~ 3,                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
      `No mês passado, com que frequência você passou tempo nas áreas ao ar livre em sua região?` == 'Algumas vezes por semana' ~ 4,                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
      `No mês passado, com que frequência você passou tempo nas áreas ao ar livre em sua região?` == 'Nunca' ~ 1,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
      `No mês passado, com que frequência você passou tempo nas áreas ao ar livre em sua região?` == 'Raramente' ~ 2,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
      `No mês passado, com que frequência você passou tempo nas áreas ao ar livre em sua região?` == 'Todos os dias' ~ 5 
    ),
    Climate_Change15 = case_when (                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
      `[Um estudo realizado em 2021]Mais cedo na pesquisa, você viu resultados de um estudo que foi publicado na Folha de São Paulo mostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão precisamente você acha que este resultado da pesquisa reflete o quão preocupadas as pessoas mais velhas no Brasil estão com as mudanças climáticas?` == '1 (Nada preciso)' ~ 1,       
      `[Um estudo realizado em 2021]Mais cedo na pesquisa, você viu resultados de um estudo que foi publicado na Folha de São Paulo mostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão precisamente você acha que este resultado da pesquisa reflete o quão preocupadas as pessoas mais velhas no Brasil estão com as mudanças climáticas?` == '2' ~ 2,                      
      `[Um estudo realizado em 2021]Mais cedo na pesquisa, você viu resultados de um estudo que foi publicado na Folha de São Paulo mostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão precisamente você acha que este resultado da pesquisa reflete o quão preocupadas as pessoas mais velhas no Brasil estão com as mudanças climáticas?` == '3' ~ 3,                      
      `[Um estudo realizado em 2021]Mais cedo na pesquisa, você viu resultados de um estudo que foi publicado na Folha de São Paulo mostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão precisamente você acha que este resultado da pesquisa reflete o quão preocupadas as pessoas mais velhas no Brasil estão com as mudanças climáticas?` == '4' ~ 4,                      
      `[Um estudo realizado em 2021]Mais cedo na pesquisa, você viu resultados de um estudo que foi publicado na Folha de São Paulo mostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão precisamente você acha que este resultado da pesquisa reflete o quão preocupadas as pessoas mais velhas no Brasil estão com as mudanças climáticas?` == '5' ~ 5,                      
      `[Um estudo realizado em 2021]Mais cedo na pesquisa, você viu resultados de um estudo que foi publicado na Folha de São Paulo mostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão precisamente você acha que este resultado da pesquisa reflete o quão preocupadas as pessoas mais velhas no Brasil estão com as mudanças climáticas?` == '6' ~ 6,                      
      `[Um estudo realizado em 2021]Mais cedo na pesquisa, você viu resultados de um estudo que foi publicado na Folha de São Paulo mostra que a maioria dos brasileiros (64%) com mais de 60 anos não acredita que cientistas e ambientalistas exageram ao tratar da crise do clima, isto é, pessoas mais velhas acreditam nos alertas de cientistas, demonstrando preocupação em relação às mudanças climáticas.Quão precisamente você acha que este resultado da pesquisa reflete o quão preocupadas as pessoas mais velhas no Brasil estão com as mudanças climáticas?` == '7 (Extremamente preciso)' ~ 7
    )
  )


#Para cada ação, por favor escolha se você acha que ela tem um grande efeito, médio efeito ou pequeno efeito. Pense no impacto de cada ação ao longo de um período de um ano, a menos que seja indicado de outra forma.
data <- data %>%
  mutate (
    Effect1 = case_when (
      `Evitar um voo de longa distância (ou seja, 6 horas ou mais)` == 'Grande efeito' ~ 3,        
      `Evitar um voo de longa distância (ou seja, 6 horas ou mais)` == 'Médio efeito' ~ 2,         
      `Evitar um voo de longa distância (ou seja, 6 horas ou mais)` == 'Pequeno efeito' ~ 1
    ),
    Effect2 = case_when (
      `Comprar coisas de maior durabilidade (ex.: roupas)` == 'Grande efeito' ~ 3,                 
      `Comprar coisas de maior durabilidade (ex.: roupas)` == 'Médio efeito' ~ 2,                  
      `Comprar coisas de maior durabilidade (ex.: roupas)` == 'Pequeno efeito' ~ 1                
    ),
    Effect3 = case_when (
      `Comprar apenas alimentos locais` == 'Grande efeito' ~ 3,                                    
      `Comprar apenas alimentos locais` == 'Médio efeito' ~ 2,                                     
      `Comprar apenas alimentos locais` == 'Pequeno efeito' ~ 1                                   
    ),
    Effect4 = case_when (
      `Comprar apenas alimentos orgânicos` == 'Grande efeito' ~ 3,                                 
      `Comprar apenas alimentos orgânicos` == 'Médio efeito' ~ 2,                                  
      `Comprar apenas alimentos orgânicos` == 'Pequeno efeito' ~ 1                                
    ),
    Effect5 = case_when (
      `Comprar apenas alimentos sem embalagem` == 'Grande efeito' ~ 3,                             
      `Comprar apenas alimentos sem embalagem` == 'Médio efeito' ~ 2,                              
      `Comprar apenas alimentos sem embalagem` == 'Pequeno efeito' ~ 1                            
    ),
    Effect6 = case_when (
      `Seguir uma dieta baseada em plantas (vegana)` == 'Grande efeito' ~ 3,                       
      `Seguir uma dieta baseada em plantas (vegana)` == 'Médio efeito' ~ 2,                        
      `Seguir uma dieta baseada em plantas (vegana)` == 'Pequeno efeito' ~ 1                      
    ),
    Effect7 = case_when (
      `Secar roupas apenas no varal` == 'Grande efeito' ~ 3,                                       
      `Secar roupas apenas no varal` == 'Médio efeito' ~ 2,                                        
      `Secar roupas apenas no varal` == 'Pequeno efeito' ~ 1                                      
    ),
    Effect8 = case_when (
      `Minimizar o desperdício de alimentos` == 'Grande efeito' ~ 3,                               
      `Minimizar o desperdício de alimentos` == 'Médio efeito' ~ 2,                                
      `Minimizar o desperdício de alimentos` == 'Pequeno efeito' ~ 1                              
    ),
    Effect9 = case_when (
      `Não jogar lixo no chão` == 'Grande efeito' ~ 3,                                             
      `Não jogar lixo no chão` == 'Médio efeito' ~ 2,                                              
      `Não jogar lixo no chão` == 'Pequeno efeito' ~ 1                                            
    ),
    Effect10 = case_when (
      `Reciclar o máximo possível` == 'Grande efeito' ~ 3,                                         
      `Reciclar o máximo possível` == 'Médio efeito' ~ 2,                                          
      `Reciclar o máximo possível` == 'Pequeno efeito' ~ 1                                        
    ),
    Effect11 = case_when (
      `Reutilizar coisas antigas em vez de comprar novas` == 'Grande efeito' ~ 3,                  
      `Reutilizar coisas antigas em vez de comprar novas` == 'Médio efeito' ~ 2,                   
      `Reutilizar coisas antigas em vez de comprar novas` == 'Pequeno efeito' ~ 1                 
    ),
    Effect12 = case_when (
      `Usar transporte público, bicicleta ou caminhar em vez de usar carro` == 'Grande efeito' ~ 3,
      `Usar transporte público, bicicleta ou caminhar em vez de usar carro` == 'Médio efeito' ~ 2, 
      `Usar transporte público, bicicleta ou caminhar em vez de usar carro` == 'Pequeno efeito' ~ 3
    ),
    Effect13 = case_when (
      `Usar sacolas reutilizáveis para compras` == 'Grande efeito' ~ 3,                            
      `Usar sacolas reutilizáveis para compras` == 'Médio efeito' ~ 2,                             
      `Usar sacolas reutilizáveis para compras` == 'Pequeno efeito' ~ 1 
    )
  )

#Como jovem, quão responsáveis você acha que cada um dos seguintes é quando se trata de combater as mudanças climáticas?
data <- data %>%
  mutate (
    Young = case_when (
      `Jovens` == '1 (Nada responsável)' ~ 1,                    
      `Jovens` == '2' ~ 2,                                      
      `Jovens` == '3' ~ 3,                                       
      `Jovens` == '4' ~ 4,                                      
      `Jovens` == '5' ~ 5,                                       
      `Jovens` == '6' ~ 6,                                      
      `Jovens` == '7 (Extremamente responsável)' ~ 7
    ),
    Old = case_when (
      `Pessoas mais velhas` == '1 (Nada responsável)' ~ 1,      
      `Pessoas mais velhas` == '2' ~ 2,                          
      `Pessoas mais velhas` == '3' ~ 3,                         
      `Pessoas mais velhas` == '4' ~ 4,                          
      `Pessoas mais velhas` == '5' ~ 5,                         
      `Pessoas mais velhas` == '6' ~ 6,                          
      `Pessoas mais velhas` == '7 (Extremamente responsável)' ~ 7
    ),
    government = case_when (
      `O governo` == '1 (Nada responsável)' ~ 1,                 
      `O governo` == '2' ~ 2,                                   
      `O governo` == '3' ~ 3,                                    
      `O governo` == '4' ~ 4,                                   
      `O governo` == '5' ~ 5,                                    
      `O governo` == '6' ~ 6,                                   
      `O governo` == '7 (Extremamente responsável)' ~ 7         
    )
  )


#Uma maneira de ajudar a combater as mudanças climáticas é que as pessoas mudem seus comportamentos em comparação com as gerações anteriores. 
#Gostaríamos de saber o quão provável é que você, como jovem, faça cada uma das seguintes ações para reduzir seu próprio impacto no meio ambiente, ou seja, reduzir sua pegada de carbono. Para algumas dessas ações, você pode ser capaz de realizá-las sozinho. Outras ações podem envolver tentar convencer outras 
#pessoas a fazê-las (por exemplo, pessoas com quem você mora). Lembre-se, não há respostas certas ou erradas, então, por favor, responda honestamente.
data <- data %>%
  mutate (
    Bin = case_when (
      `Usar uma lixeira separada para resíduos alimentares (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '1 (Nada provável)' ~ 1,        
      `Usar uma lixeira separada para resíduos alimentares (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '2' ~ 2,                        
      `Usar uma lixeira separada para resíduos alimentares (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '3' ~ 3,                        
      `Usar uma lixeira separada para resíduos alimentares (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '4' ~ 4,                        
      `Usar uma lixeira separada para resíduos alimentares (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '5' ~ 5,                        
      `Usar uma lixeira separada para resíduos alimentares (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '6' ~ 6,                        
      `Usar uma lixeira separada para resíduos alimentares (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '7 (Extremamente provável)' ~ 7
    ),
    Eat_Less_Meat = case_when (
      `Comer menos carne` == '1 (Nada provável)' ~ 1,                                                                                                          
      `Comer menos carne` == '2' ~ 2,                                                                                                                          
      `Comer menos carne` == '3' ~ 3,                                                                                                                          
      `Comer menos carne` == '4' ~ 4,                                                                                                                          
      `Comer menos carne` == '5' ~ 5,                                                                                                                          
      `Comer menos carne` == '6' ~ 6,                                                                                                                          
      `Comer menos carne` == '7 (Extremamente provável)' ~ 7
    ),
    Not_Eat_Meat = case_when (
      `Não comer carne (ou seja, ser vegetariano ou baseado em plantas/vegano)` == '1 (Nada provável)' ~ 1,                                                    
      `Não comer carne (ou seja, ser vegetariano ou baseado em plantas/vegano)` == '2' ~ 2,                                                                    
      `Não comer carne (ou seja, ser vegetariano ou baseado em plantas/vegano)` == '3' ~ 3,                                                                    
      `Não comer carne (ou seja, ser vegetariano ou baseado em plantas/vegano)` == '4' ~ 4,                                                                    
      `Não comer carne (ou seja, ser vegetariano ou baseado em plantas/vegano)` == '5' ~ 5,                                                                    
      `Não comer carne (ou seja, ser vegetariano ou baseado em plantas/vegano)` == '6' ~ 6,                                                                    
      `Não comer carne (ou seja, ser vegetariano ou baseado em plantas/vegano)` == '7 (Extremamente provável)' ~ 7
    ),
    Buy_Local_Food = case_when (
      `Comprar alimentos produzidos localmente (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '1 (Nada provável)' ~ 1,                    
      `Comprar alimentos produzidos localmente (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '2' ~ 2,                                    
      `Comprar alimentos produzidos localmente (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '3' ~ 3,                                    
      `Comprar alimentos produzidos localmente (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '4' ~ 4,                                    
      `Comprar alimentos produzidos localmente (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '5' ~ 5,                                    
      `Comprar alimentos produzidos localmente (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '6' ~ 6,                                    
      `Comprar alimentos produzidos localmente (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '7 (Extremamente provável)' ~ 7
    ),
    Transport = case_when (
      `Caminhar, andar de bicicleta ou usar transporte público na maioria das viagens em vez de pegar carona/ir de carro` == '1 (Nada provável)' ~ 1,          
      `Caminhar, andar de bicicleta ou usar transporte público na maioria das viagens em vez de pegar carona/ir de carro` == '2' ~ 2,                          
      `Caminhar, andar de bicicleta ou usar transporte público na maioria das viagens em vez de pegar carona/ir de carro` == '3' ~ 3,                          
      `Caminhar, andar de bicicleta ou usar transporte público na maioria das viagens em vez de pegar carona/ir de carro` == '4' ~ 4,                          
      `Caminhar, andar de bicicleta ou usar transporte público na maioria das viagens em vez de pegar carona/ir de carro` == '5' ~ 5,                          
      `Caminhar, andar de bicicleta ou usar transporte público na maioria das viagens em vez de pegar carona/ir de carro` == '6' ~ 6,                          
      `Caminhar, andar de bicicleta ou usar transporte público na maioria das viagens em vez de pegar carona/ir de carro` == '7 (Extremamente provável)' ~ 7
    ),
    Less_Flights = case_when (
      `Fazer menos voos (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '1 (Nada provável)' ~ 1,                                           
      `Fazer menos voos (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '2' ~ 2,                                                           
      `Fazer menos voos (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '3' ~ 3,                                                           
      `Fazer menos voos (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '4' ~ 4,                                                           
      `Fazer menos voos (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '5' ~ 5,                                                           
      `Fazer menos voos (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '6' ~ 6,                                                           
      `Fazer menos voos (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '7 (Extremamente provável)' ~ 7
    ),
    Lamp_Low = case_when (
      `Comprar lâmpadas de baixo consumo energético (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '1 (Nada provável)' ~ 1,               
      `Comprar lâmpadas de baixo consumo energético (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '2' ~ 2,                               
      `Comprar lâmpadas de baixo consumo energético (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '3' ~ 3,                               
      `Comprar lâmpadas de baixo consumo energético (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '4' ~ 4,                               
      `Comprar lâmpadas de baixo consumo energético (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '5' ~ 5,                               
      `Comprar lâmpadas de baixo consumo energético (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '6' ~ 6,                               
      `Comprar lâmpadas de baixo consumo energético (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '7 (Extremamente provável)' ~ 7
    ),
    Garbage_Floor = case_when (
      `Não jogar lixo no chão` == '2' ~ 2,                                                                                                                     
      `Não jogar lixo no chão` == '3' ~ 3,                                                                                                                     
      `Não jogar lixo no chão` == '4' ~ 4,                                                                                                                     
      `Não jogar lixo no chão` == '5' ~ 5,                                                                                                                     
      `Não jogar lixo no chão` == '6' ~ 6,                                                                                                                     
      `Não jogar lixo no chão` == '7 (Extremamente provável)' ~ 7
    ),
    Avoid_Plastic = case_when (
      `Evitar plásticos de uso único` == '1 (Nada provável)' ~ 1,                                                                                              
      `Evitar plásticos de uso único` == '2' ~ 2,                                                                                                              
      `Evitar plásticos de uso único` == '3' ~ 3,                                                                                                              
      `Evitar plásticos de uso único` == '4' ~ 4,                                                                                                              
      `Evitar plásticos de uso único` == '5' ~ 5,                                                                                                              
      `Evitar plásticos de uso único` == '6' ~ 6,                                                                                                              
      `Evitar plásticos de uso único` == '7 (Extremamente provável)' ~ 7,
    ),
    Buy_Less_Stufs = case_when (
      `Comprar menos coisas novas (por exemplo, comprar em segunda mão ou reutilizar coisas antigas, como roupas)` == '1 (Nada provável)' ~ 1,                 
      `Comprar menos coisas novas (por exemplo, comprar em segunda mão ou reutilizar coisas antigas, como roupas)` == '2' ~ 2,                                 
      `Comprar menos coisas novas (por exemplo, comprar em segunda mão ou reutilizar coisas antigas, como roupas)` == '3' ~ 3,                                 
      `Comprar menos coisas novas (por exemplo, comprar em segunda mão ou reutilizar coisas antigas, como roupas)` == '4' ~ 4,                                 
      `Comprar menos coisas novas (por exemplo, comprar em segunda mão ou reutilizar coisas antigas, como roupas)` == '5' ~ 5,                                 
      `Comprar menos coisas novas (por exemplo, comprar em segunda mão ou reutilizar coisas antigas, como roupas)` == '6' ~ 6,                                 
      `Comprar menos coisas novas (por exemplo, comprar em segunda mão ou reutilizar coisas antigas, como roupas)` == '7 (Extremamente provável)' ~ 7 
    ),
    Bin_Separeted = case_when (
      `Usar uma lixeira separada para reciclagem (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '1 (Nada provável)' ~ 1,
      `Usar uma lixeira separada para reciclagem (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '2' ~ 2,                          
      `Usar uma lixeira separada para reciclagem (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '4' ~ 4,
      `Usar uma lixeira separada para reciclagem (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '5' ~ 5,
      `Usar uma lixeira separada para reciclagem (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '6' ~ 6,
      `Usar uma lixeira separada para reciclagem (ou tentar convencer aqueles com quem você vive a fazerem isso)` == '7 (Extremamente provável)' ~ 7
    )
  )


#Algumas ações pró-ambientais são aquelas que você talvez não consiga realizar no curto prazo, mas que podem ser relevantes quando você for mais velho. 
#Abaixo está uma lista de mudanças que poucas pessoas mais velhas fizeram até hoje. Pensando em você como jovem e em como sua vida pode ser, 
#quão provável é que você faça cada uma das seguintes ações por razões ambientais?
data <- data %>%
  mutate(
    Not_Car = case_when (
      `Viver sem carro` == '1 (Nada provável)' ~ 1,                                                          
      `Viver sem carro` == '2' ~ 2,                                                                          
      `Viver sem carro` == '3' ~ 3,                                                                          
      `Viver sem carro` == '4' ~ 4,                                                                          
      `Viver sem carro` == '5' ~ 5,                                                                          
      `Viver sem carro` == '7 (Extremamente provável)' ~ 7
    ),
    Avoid_Flights = case_when (	
      `Evitar voos` == '1 (Nada provável)' ~ 1,
      `Evitar voos` == '2' ~ 2,                                                                              
      `Evitar voos` == '3' ~ 3,                                                                              
      `Evitar voos` == '4' ~ 4,                                                                              
      `Evitar voos` == '5' ~ 5,                                                                              
      `Evitar voos` == '6' ~ 6,                                                                              
      `Evitar voos` == '7 (Extremamente provável)' ~ 7
    ),
    Plant_Based_Diet = case_when (	
      `Seguir uma dieta baseada em plantas (ou seja, sem carne ou laticínios)` == '1 (Nada provável)' ~ 1,   
      `Seguir uma dieta baseada em plantas (ou seja, sem carne ou laticínios)` == '2' ~ 2,                   
      `Seguir uma dieta baseada em plantas (ou seja, sem carne ou laticínios)` == '3' ~ 3,                   
      `Seguir uma dieta baseada em plantas (ou seja, sem carne ou laticínios)` == '4' ~ 4,                   
      `Seguir uma dieta baseada em plantas (ou seja, sem carne ou laticínios)` == '5' ~ 5,                   
      `Seguir uma dieta baseada em plantas (ou seja, sem carne ou laticínios)` == '6' ~ 6,                   
      `Seguir uma dieta baseada em plantas (ou seja, sem carne ou laticínios)` == '7 (Extremamente provável)' ~ 7
    )
  )

data <- data %>%
  mutate(
    Tax_Increase = case_when (
      `Aumento de impostos sobre gasolina e diesel para financiar mais transporte público.` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Aumento de impostos sobre gasolina e diesel para financiar mais transporte público.` == '2' ~ 2,
      `Aumento de impostos sobre gasolina e diesel para financiar mais transporte público.` == '3' ~ 3,
      `Aumento de impostos sobre gasolina e diesel para financiar mais transporte público.` == '4' ~ 4,
      `Aumento de impostos sobre gasolina e diesel para financiar mais transporte público.` == '5' ~ 5,
      `Aumento de impostos sobre gasolina e diesel para financiar mais transporte público.` == '6' ~ 6,
      `Aumento de impostos sobre gasolina e diesel para financiar mais transporte público.` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Ban_Domestic_Flights = case_when (
      `Proibição de voos domésticos (por exemplo, São Paulo para Rio de Janeiro) a menos que seja para fornecer um serviço essencial.` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Proibição de voos domésticos (por exemplo, São Paulo para Rio de Janeiro) a menos que seja para fornecer um serviço essencial.` == '2' ~ 2,
      `Proibição de voos domésticos (por exemplo, São Paulo para Rio de Janeiro) a menos que seja para fornecer um serviço essencial.` == '3' ~ 3,
      `Proibição de voos domésticos (por exemplo, São Paulo para Rio de Janeiro) a menos que seja para fornecer um serviço essencial.` == '4' ~ 4,
      `Proibição de voos domésticos (por exemplo, São Paulo para Rio de Janeiro) a menos que seja para fornecer um serviço essencial.` == '5' ~ 5,
      `Proibição de voos domésticos (por exemplo, São Paulo para Rio de Janeiro) a menos que seja para fornecer um serviço essencial.` == '6' ~ 6,
      `Proibição de voos domésticos (por exemplo, São Paulo para Rio de Janeiro) a menos que seja para fornecer um serviço essencial.` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Ban_cars = case_when (
      `Proibição de carros em certas partes das cidades e centros urbanos (por exemplo, implementar zonas livres de carros).` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Proibição de carros em certas partes das cidades e centros urbanos (por exemplo, implementar zonas livres de carros).` == '2' ~ 2,
      `Proibição de carros em certas partes das cidades e centros urbanos (por exemplo, implementar zonas livres de carros).` == '3' ~ 3,
      `Proibição de carros em certas partes das cidades e centros urbanos (por exemplo, implementar zonas livres de carros).` == '4' ~ 4,
      `Proibição de carros em certas partes das cidades e centros urbanos (por exemplo, implementar zonas livres de carros).` == '5' ~ 5,
      `Proibição de carros em certas partes das cidades e centros urbanos (por exemplo, implementar zonas livres de carros).` == '6' ~ 6,
      `Proibição de carros em certas partes das cidades e centros urbanos (por exemplo, implementar zonas livres de carros).` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Limited_Flights = case_when (
      `Limite no número de voos que qualquer pessoa pode fazer em um ano.` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Limite no número de voos que qualquer pessoa pode fazer em um ano.` == '2' ~ 2,
      `Limite no número de voos que qualquer pessoa pode fazer em um ano.` == '3' ~ 3,
      `Limite no número de voos que qualquer pessoa pode fazer em um ano.` == '4' ~ 4,
      `Limite no número de voos que qualquer pessoa pode fazer em um ano.` == '5' ~ 5,
      `Limite no número de voos que qualquer pessoa pode fazer em um ano.` == '6' ~ 6,
      `Limite no número de voos que qualquer pessoa pode fazer em um ano.` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Prohibition_Subsidies = case_when (
      `Proibição do uso de subsídios ambientalmente prejudiciais na produção e importação de bens, mesmo que isso faça com que produtos do dia a dia fiquem mais caros.` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Proibição do uso de subsídios ambientalmente prejudiciais na produção e importação de bens, mesmo que isso faça com que produtos do dia a dia fiquem mais caros.` == '2' ~ 2,
      `Proibição do uso de subsídios ambientalmente prejudiciais na produção e importação de bens, mesmo que isso faça com que produtos do dia a dia fiquem mais caros.` == '3' ~ 3,
      `Proibição do uso de subsídios ambientalmente prejudiciais na produção e importação de bens, mesmo que isso faça com que produtos do dia a dia fiquem mais caros.` == '4' ~ 4,
      `Proibição do uso de subsídios ambientalmente prejudiciais na produção e importação de bens, mesmo que isso faça com que produtos do dia a dia fiquem mais caros.` == '5' ~ 5,
      `Proibição do uso de subsídios ambientalmente prejudiciais na produção e importação de bens, mesmo que isso faça com que produtos do dia a dia fiquem mais caros.` == '6' ~ 6,
      `Proibição do uso de subsídios ambientalmente prejudiciais na produção e importação de bens, mesmo que isso faça com que produtos do dia a dia fiquem mais caros.` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Tax_Reduction = case_when (
      `Redução de impostos para produtos importados que são neutros em carbono (com impostos mais altos para aqueles que não são).` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Redução de impostos para produtos importados que são neutros em carbono (com impostos mais altos para aqueles que não são).` == '2' ~ 2,
      `Redução de impostos para produtos importados que são neutros em carbono (com impostos mais altos para aqueles que não são).` == '3' ~ 3,
      `Redução de impostos para produtos importados que são neutros em carbono (com impostos mais altos para aqueles que não são).` == '4' ~ 4,
      `Redução de impostos para produtos importados que são neutros em carbono (com impostos mais altos para aqueles que não são).` == '5' ~ 5,
      `Redução de impostos para produtos importados que são neutros em carbono (com impostos mais altos para aqueles que não são).` == '6' ~ 6,
      `Redução de impostos para produtos importados que são neutros em carbono (com impostos mais altos para aqueles que não são).` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Increase_Taxes_Meat = case_when (
      `Aumento de impostos sobre carne, com o dinheiro arrecadado sendo investido em maneiras de tornar a agricultura mais ambientalmente amigável.` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Aumento de impostos sobre carne, com o dinheiro arrecadado sendo investido em maneiras de tornar a agricultura mais ambientalmente amigável.` == '2' ~ 2,
      `Aumento de impostos sobre carne, com o dinheiro arrecadado sendo investido em maneiras de tornar a agricultura mais ambientalmente amigável.` == '3' ~ 3,
      `Aumento de impostos sobre carne, com o dinheiro arrecadado sendo investido em maneiras de tornar a agricultura mais ambientalmente amigável.` == '4' ~ 4,
      `Aumento de impostos sobre carne, com o dinheiro arrecadado sendo investido em maneiras de tornar a agricultura mais ambientalmente amigável.` == '5' ~ 5,
      `Aumento de impostos sobre carne, com o dinheiro arrecadado sendo investido em maneiras de tornar a agricultura mais ambientalmente amigável.` == '6' ~ 6,
      `Aumento de impostos sobre carne, com o dinheiro arrecadado sendo investido em maneiras de tornar a agricultura mais ambientalmente amigável.` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Energy_Renewable = case_when (
      `Tornar fontes de energia renováveis, como eólica ou solar, obrigatórias, mesmo que custem mais.` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Tornar fontes de energia renováveis, como eólica ou solar, obrigatórias, mesmo que custem mais.` == '2' ~ 2,
      `Tornar fontes de energia renováveis, como eólica ou solar, obrigatórias, mesmo que custem mais.` == '3' ~ 3,                                                                                                                                                                                                          
      `Tornar fontes de energia renováveis, como eólica ou solar, obrigatórias, mesmo que custem mais.` == '4' ~ 4,                                                                                                                                                                                                            
      `Tornar fontes de energia renováveis, como eólica ou solar, obrigatórias, mesmo que custem mais.` == '5' ~ 5,                                                                                                                                                                                                            
      `Tornar fontes de energia renováveis, como eólica ou solar, obrigatórias, mesmo que custem mais.` == '6' ~ 6,                                                                                                                                                                                                            
      `Tornar fontes de energia renováveis, como eólica ou solar, obrigatórias, mesmo que custem mais.` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Increase_Taxes_Property  = case_when (		  
      `Aumento de impostos sobre imóveis que não são energeticamente eficientes, com o dinheiro arrecadado sendo destinado a subsídios para retrofit de casas (ou seja, para pagar parte do custo de tornar as casas mais eficientes em termos energéticos).` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,
      `Aumento de impostos sobre imóveis que não são energeticamente eficientes, com o dinheiro arrecadado sendo destinado a subsídios para retrofit de casas (ou seja, para pagar parte do custo de tornar as casas mais eficientes em termos energéticos).` == '2' ~ 2,                                                      
      `Aumento de impostos sobre imóveis que não são energeticamente eficientes, com o dinheiro arrecadado sendo destinado a subsídios para retrofit de casas (ou seja, para pagar parte do custo de tornar as casas mais eficientes em termos energéticos).` == '3' ~ 3,                                                      
      `Aumento de impostos sobre imóveis que não são energeticamente eficientes, com o dinheiro arrecadado sendo destinado a subsídios para retrofit de casas (ou seja, para pagar parte do custo de tornar as casas mais eficientes em termos energéticos).` == '4' ~ 4,                                                      
      `Aumento de impostos sobre imóveis que não são energeticamente eficientes, com o dinheiro arrecadado sendo destinado a subsídios para retrofit de casas (ou seja, para pagar parte do custo de tornar as casas mais eficientes em termos energéticos).` == '5' ~ 5,                                                      
      `Aumento de impostos sobre imóveis que não são energeticamente eficientes, com o dinheiro arrecadado sendo destinado a subsídios para retrofit de casas (ou seja, para pagar parte do custo de tornar as casas mais eficientes em termos energéticos).` == '6' ~ 6,                                                      
      `Aumento de impostos sobre imóveis que não são energeticamente eficientes, com o dinheiro arrecadado sendo destinado a subsídios para retrofit de casas (ou seja, para pagar parte do custo de tornar as casas mais eficientes em termos energéticos).` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7
    ),
    Fine_Companies = case_when (
      `Multas para empresas que têm emissões acima de um determinado nível.` == '1 (Não gostaria que fosse implementado de forma alguma)' ~ 1,                                                                                                                                                                                 
      `Multas para empresas que têm emissões acima de um determinado nível.` == '2' ~ 2,                                                                                                                                                                                                                                       
      `Multas para empresas que têm emissões acima de um determinado nível.` == '3' ~ 3,                                                                                                                                                                                                                                       
      `Multas para empresas que têm emissões acima de um determinado nível.` == '4' ~ 4,                                                                                                                                                                                                                                       
      `Multas para empresas que têm emissões acima de um determinado nível.` == '5' ~ 5,                                                                                                                                                                                                                                       
      `Multas para empresas que têm emissões acima de um determinado nível.` == '6' ~ 6,                                                                                                                                                                                                                                       
      `Multas para empresas que têm emissões acima de um determinado nível.` == '7 (Gostaria que fosse implementado em grande medida)' ~ 7 
    )
  )

#Nesta última seção, gostaríamos de saber mais sobre o que você, como jovem, pensa sobre sua área local. Especificamente, 
#estamos interessados nos lugares ao ar livre que você pode visitar para atividades (por exemplo, caminhadas). Esses lugares 
#são chamados de "amenidades ao ar livre", e exemplos incluem parques e praias. Até que ponto você concorda com as seguintes afirmações?

data <- data %>%
  mutate(
    Think_Pleasantries = case_when (
      `Eu consigo pensar facilmente em amenidades ao ar livre (por exemplo, parques, praias, lagos) na minha área local.` == '1 (Discordo completamente)' ~ 1,                       
      `Eu consigo pensar facilmente em amenidades ao ar livre (por exemplo, parques, praias, lagos) na minha área local.` == '2' ~ 2,                                                
      `Eu consigo pensar facilmente em amenidades ao ar livre (por exemplo, parques, praias, lagos) na minha área local.` == '3' ~ 3,                                                
      `Eu consigo pensar facilmente em amenidades ao ar livre (por exemplo, parques, praias, lagos) na minha área local.` == '4' ~ 4,                                                
      `Eu consigo pensar facilmente em amenidades ao ar livre (por exemplo, parques, praias, lagos) na minha área local.` == '5' ~ 5,                                                
      `Eu consigo pensar facilmente em amenidades ao ar livre (por exemplo, parques, praias, lagos) na minha área local.` == '6' ~ 6,                                                
      `Eu consigo pensar facilmente em amenidades ao ar livre (por exemplo, parques, praias, lagos) na minha área local.` == '7 (Concordo completamente)' ~ 7
    ),
    Easy_Outdoor_Amenities = case_when (                   
      `É fácil para mim chegar a pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '1 (Discordo completamente)' ~ 1,                                  
      `É fácil para mim chegar a pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '2' ~ 2,                                                           
      `É fácil para mim chegar a pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '3' ~ 3,                                                           
      `É fácil para mim chegar a pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '4' ~ 4,                                                           
      `É fácil para mim chegar a pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '5' ~ 5,                                                           
      `É fácil para mim chegar a pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '6' ~ 6,                                                           
      `É fácil para mim chegar a pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '7 (Concordo completamente)' ~ 7
    ),
    Time_Outdoor_Amenities = case_when (                                                    
      `Eu tenho tempo para visitar pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '1 (Discordo completamente)' ~ 1,                                
      `Eu tenho tempo para visitar pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '2' ~ 2,                                                         
      `Eu tenho tempo para visitar pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '3' ~ 3,                                                         
      `Eu tenho tempo para visitar pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '4' ~ 4,                                                         
      `Eu tenho tempo para visitar pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '5' ~ 5,                                                         
      `Eu tenho tempo para visitar pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '6' ~ 6,                                                         
      `Eu tenho tempo para visitar pelo menos algumas amenidades ao ar livre na minha área local, se eu quiser.` == '7 (Concordo completamente)' ~ 7
    ),
    Validation = case_when (                                                   
      `Sabemos que há várias perguntas nesta seção. Para mostrar que você está lendo cada pergunta com atenção, selecione '1' na escala abaixo.` == '1 (Discordo completamente)' ~ 1,
      `Sabemos que há várias perguntas nesta seção. Para mostrar que você está lendo cada pergunta com atenção, selecione '1' na escala abaixo.` == '2' ~ 2,                         
      `Sabemos que há várias perguntas nesta seção. Para mostrar que você está lendo cada pergunta com atenção, selecione '1' na escala abaixo.` == '3' ~ 3,                         
      `Sabemos que há várias perguntas nesta seção. Para mostrar que você está lendo cada pergunta com atenção, selecione '1' na escala abaixo.` == '4' ~ 4,                         
      `Sabemos que há várias perguntas nesta seção. Para mostrar que você está lendo cada pergunta com atenção, selecione '1' na escala abaixo.` == '5' ~ 5,                         
      `Sabemos que há várias perguntas nesta seção. Para mostrar que você está lendo cada pergunta com atenção, selecione '1' na escala abaixo.` == '6' ~ 6,                         
      `Sabemos que há várias perguntas nesta seção. Para mostrar que você está lendo cada pergunta com atenção, selecione '1' na escala abaixo.` == '7 (Concordo completamente)' ~ 7
    ),
    Visit_Amenities_Important = case_when (                   
      `Visitar amenidades ao ar livre é importante para mim.` == '2' ~ 2,                                                                                                
      `Visitar amenidades ao ar livre é importante para mim.` == '3' ~ 3,                                                                                                
      `Visitar amenidades ao ar livre é importante para mim.` == '4' ~ 4,                                                                                                
      `Visitar amenidades ao ar livre é importante para mim.` == '5' ~ 5,                                                                                                
      `Visitar amenidades ao ar livre é importante para mim.` == '6' ~ 6,                                                                                                
      `Visitar amenidades ao ar livre é importante para mim.` == '7 (Concordo completamente)' ~ 7
    ),
    Visit_Amenities_Like = case_when (                                                                                         
      `Visitar amenidades ao ar livre é algo que eu gosto de fazer.` == '2' ~ 2,                                                                                         
      `Visitar amenidades ao ar livre é algo que eu gosto de fazer.` == '3' ~ 3,                                                                                         
      `Visitar amenidades ao ar livre é algo que eu gosto de fazer.` == '4' ~ 4,                                                                                         
      `Visitar amenidades ao ar livre é algo que eu gosto de fazer.` == '5' ~ 5,                                                                                         
      `Visitar amenidades ao ar livre é algo que eu gosto de fazer.` == '6' ~ 6,                                                                                         
      `Visitar amenidades ao ar livre é algo que eu gosto de fazer.` == '7 (Concordo completamente)' ~ 7
    ),
    Satisfied_Amenities = case_when (                                                                                  
      `Eu estou satisfeito com as amenidades ao ar livre na minha área local.` == '1 (Discordo completamente)' ~ 1,                                                                  
      `Eu estou satisfeito com as amenidades ao ar livre na minha área local.` == '2' ~ 2,                                                                                           
      `Eu estou satisfeito com as amenidades ao ar livre na minha área local.` == '3' ~ 3,                                                                                           
      `Eu estou satisfeito com as amenidades ao ar livre na minha área local.` == '4' ~ 4,                                                                                           
      `Eu estou satisfeito com as amenidades ao ar livre na minha área local.` == '5' ~ 5,                                                                                           
      `Eu estou satisfeito com as amenidades ao ar livre na minha área local.` == '6' ~ 6,                                                                                           
      `Eu estou satisfeito com as amenidades ao ar livre na minha área local.` == '7 (Concordo completamente)' ~ 7  
    )
  )
