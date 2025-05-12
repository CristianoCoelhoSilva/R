  library(readr)
  library(stargazer)
  library(lfe)
  library(data.table)
  library(bit64)
  library(tidyverse)
  library(stargazer)
  library(lfe)
  library(lubridate)
  library(gridExtra)
  library(grid)
  require(fixest)
  require(tidyverse)
  library(stringr)
  library(viridis)
  library(broom)
  
  options(scipen = 999)
  
  enem <- read_csv("~/AULA/ENEM/DADOS/enem_tratado.csv")
  
  enem <- enem %>%
    mutate(
      Genero = as.factor(Genero),
      Tipo_dependencia = as.factor(Tipo_dependencia),
      Classe_Renda = as.factor(Classe_Renda),
      Raca  = as.factor(Raca),
      Regiao_Norte_Nordeste = as.factor(Regiao_Norte_Nordeste)
    )

  #Nota Matemática
  modelo_fixef <- feols(Nota_Matematica ~ Classe_Renda, data = enem)

  etable(modelo_fixef, 
         dict = c(
           GeneroM = "Genero: Masculino",
           RacaNãoBranca = "Raça: Não Branca",
           Tipo_dependenciaPública = "Escola Pública",
           Regiao_Norte_Nordeste1 = "Região Norte/Nordeste",
           Classe_RendaPobre = "Classe Renda (Pobre)",
           Classe_RendaRico = "Classe Renda (Rico)"
         ))
  
  modelo_fixef <- feols(Nota_Linguagens ~ Classe_Renda, data = enem)
  
  etable(modelo_fixef, 
         dict = c(
           GeneroM = "Genero: Masculino",
           RacaNãoBranca = "Raça: Não Branca",
           Tipo_dependenciaPública = "Escola Pública",
           Regiao_Norte_Nordeste1 = "Região Norte/Nordeste",
           Classe_RendaPobre = "Classe Renda (Pobre)",
           Classe_RendaRico = "Classe Renda (Rico)"
         ))
  
  modelo_fixef <- feols(Nota_REDACAO ~ Classe_Renda, data = enem)
  
  etable(modelo_fixef, 
         dict = c(
           GeneroM = "Genero: Masculino",
           RacaNãoBranca = "Raça: Não Branca",
           Tipo_dependenciaPública = "Escola Pública",
           Regiao_Norte_Nordeste1 = "Região Norte/Nordeste",
           Classe_RendaPobre = "Classe Renda (Pobre)",
           Classe_RendaRico = "Classe Renda (Rico)"
         ))
