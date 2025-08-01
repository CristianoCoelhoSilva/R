baixar_pof_codigo <- function(pasta_destino){
  
  url <- "https://ftp.ibge.gov.br/Orcamentos_Familiares/Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20230713.zip"
  
  tempFile <- tempfile()
  
  download.file(url,tempFile,quiet=TRUE,mode="wb")
  
  unzip(file.path(tempFile), exdir = pasta_destino)
  
}

ler_pof_geral_codigo <- function(arquivo_microdados) {
  
  if(str_detect(arquivo_microdados, regex("aluguel_estimado", ignore_case = TRUE))){
    leitor <- leitor_aluguel_estimado
  }
  else if(str_detect(arquivo_microdados, regex("caderneta_coletiva", ignore_case = TRUE))){
    leitor <- leitor_caderneta_coletiva
  }
  else if(str_detect(arquivo_microdados, regex("caracteristicas_dieta", ignore_case = TRUE))){
    leitor <- leitor_caracteristicas_dieta
  }
  else if(str_detect(arquivo_microdados, regex("condicoes_vida", ignore_case = TRUE))){
    leitor <- leitor_condicoes_vida
  }
  else if(str_detect(arquivo_microdados, regex("consumo_alimentar", ignore_case = TRUE))){
    leitor <- leitor_consumo_alimentar
  }
  else if(str_detect(arquivo_microdados, regex("despesa_coletiva", ignore_case = TRUE))){
    leitor <- leitor_despesa_coletiva
  }
  else if(str_detect(arquivo_microdados, regex("despesa_individual", ignore_case = TRUE))){
    leitor <- leitor_despesa_individual
  }
  else if(str_detect(arquivo_microdados, regex("domicilio", ignore_case = TRUE))){
    leitor <- leitor_domicilio
  }
  else if(str_detect(arquivo_microdados, regex("inventario", ignore_case = TRUE))){
    leitor <- leitor_inventario
  }
  else if(str_detect(arquivo_microdados, regex("morador", ignore_case = TRUE))){
    leitor <- leitor_morador
  }
  else if(str_detect(arquivo_microdados, regex("outros_rendimentos", ignore_case = TRUE))){
    leitor <- leitor_outros_rendimentos
  }
  else if(str_detect(arquivo_microdados, regex("rendimento_trabalho", ignore_case = TRUE))){
    leitor <- leitor_rendimento_trabalho
  }
  else if(str_detect(arquivo_microdados, regex("restricao_produtos_servicos_saude", ignore_case = TRUE))){
    leitor <- leitor_restricao_produtos_servicos_saude
  }
  else if(str_detect(arquivo_microdados, regex("servico_nao_monetario_pof2", ignore_case = TRUE))){
    leitor <- leitor_servico_nao_monetario_pof2
  }
  else if(str_detect(arquivo_microdados, regex("servico_nao_monetario_pof4", ignore_case = TRUE))){
    leitor <- leitor_servico_nao_monetario_pof4
  }
  
  leitor$variavel <- as.character(leitor$variavel)
  colpos <- fwf_widths(leitor$tamanho,
                       col_names = leitor$variavel)
  pof_mod <- read_fwf(file = as.character(arquivo_microdados),
                      col_positions = colpos,
                      col_types = cols(.default = col_character()))
  
}

ler_pof_despesa_codigo <- function(arquivo_microdados){
  
  if(!(str_detect(arquivo_microdados,
                  regex("aluguel_estimado|caderneta_coletiva|despesa_coletiva|despesa_individual|outros_rendimentos|rendimento_trabalho",
                        ignore_case = TRUE)))){
    
    cat("Os registros de POF 2017-2018 aceitos para a despesa sao: \n
          ALUGUEL_ESTIMADO, CADERNETA_COLETIVA, DESPESA_COLETIVA, \n
          DESPESA_INDIVIDUAL, OUTROS_RENDIMENTOS, RENDIMENTO_TRABALHO \n")
    stop()
  }
  
  if(str_detect(arquivo_microdados, regex("aluguel_estimado", ignore_case = TRUE))){
    
    pof <- ler_pof_geral_codigo(arquivo_microdados) %>%
      mutate(across(.cols = c(V8000_DEFLA,V9011,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal=(V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "ALUGUEL_ESTIMADO") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("despesa_coletiva", ignore_case = TRUE))){
    
    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(QUADRO,
                              V8000_DEFLA,V1904_DEFLA,V9011,
                              FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8000_DEFLA*FATOR_ANUALIZACAO)/12),
             inss_mensal = (V1904_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "DESPESA_COLETIVA") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal, UF, COD_UPA, NUM_DOM)
  }
  else if(str_detect(arquivo_microdados, regex("caderneta_coletiva", ignore_case = TRUE))){
    
    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V8000_DEFLA,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO)/12,
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "CADERNETA_COLETIVA") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("despesa_individual", ignore_case = TRUE))){
    
    pof <- ler_pof_geral (arquivo_microdados) %>%
      mutate(across(.cols = c(V8000_DEFLA,FATOR_ANUALIZACAO,PESO_FINAL, V9011),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                    (V8000_DEFLA*FATOR_ANUALIZACAO)/12),
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             deducao_mensal = NA,
             pof = "DESPESA_INDIVIDUAL") %>%
      select(ID_uc, QUADRO, V9001, pof, UF, COD_UPA, NUM_DOM, valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("rendimento_trabalho", ignore_case = TRUE))){
    
    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V531112_DEFLA,V531122_DEFLA,V531132_DEFLA,
                              V9011,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             valor_mensal = NA,
             inss_mensal = NA,
             prev_pub_mensal=(V531112_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             ir_mensal=(V531122_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             iss_mensal=(V531132_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
             deducao_mensal = NA,
             pof = "RENDIMENTO_TRABALHO") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  else if(str_detect(arquivo_microdados, regex("outros_rendimentos", ignore_case = TRUE))){
    
    pof <- ler_pof_geral(arquivo_microdados) %>%
      mutate(across(.cols = c(V8501_DEFLA,V9011,FATOR_ANUALIZACAO,PESO_FINAL),
                    .fns = as.numeric)) %>%
      mutate(NUM_DOM = str_pad(NUM_DOM, 2, "left", "0"),
             NUM_UC = str_pad(NUM_UC, 2, "left", "0"),
             ID_uc = str_c(COD_UPA, NUM_DOM, NUM_UC),
             deducao_mensal = ifelse( QUADRO==54,
                                      (V8501_DEFLA*V9011*FATOR_ANUALIZACAO)/12,
                                      (V8501_DEFLA*FATOR_ANUALIZACAO)/12),
             valor_mensal = NA,
             inss_mensal = NA,
             prev_pub_mensal = NA,
             ir_mensal = NA,
             iss_mensal = NA,
             pof = "OUTROS_RENDIMENTOS",
             variavel = "V8501_DEFLA") %>%
      select(ID_uc, PESO_FINAL, V9001, pof,
             valor_mensal, inss_mensal, prev_pub_mensal, ir_mensal, iss_mensal, deducao_mensal)
  }
  
  # tradutor_despesa <-
  #   read_excel("./tradutores/Tradutor_Despesa_Geral.xls") %>%
  #   mutate(across(.cols = starts_with("Descricao"),
  #                 .fns = toupper))

  
  pof <- pof %>%
    mutate(Codigo = trunc(as.numeric(V9001)/100)) %>%
    left_join(tradutor_despesa,
              by = "Codigo") %>%
    mutate(valor = case_when(
      Variavel == "V8000_DEFLA"    ~ as.numeric(valor_mensal),
      Variavel == "V1904_DEFLA"    ~ as.numeric(inss_mensal),
      Variavel == "V531112_DEFLA"  ~ as.numeric(prev_pub_mensal),
      Variavel == "V531122_DEFLA"  ~ as.numeric(ir_mensal),
      Variavel == "V531132_DEFLA"  ~ as.numeric(iss_mensal),
      Variavel == "V8501_DEFLA"    ~ as.numeric(deducao_mensal),
      TRUE ~ 0
    )) %>%
    select(-c(ends_with("mensal"), ends_with("DEFLA"))) %>%
    mutate(ID_uc = as.numeric(ID_uc))
}