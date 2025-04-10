library(readxl)
library(e1071)
library(tseries)
library(gridExtra)
options(scipen = 999)


data <- read_excel("Diogo.xlsx")

data <- data[,-1]

source("Summary Full.R")

retorno <- calcular_estatisticas(data)

knitr::kable(retorno)



library("pander")
# Gere o PDF
pander::pander(retorno, file = "output.pdf")


# Crie a tabela como um gráfico
# Supondo que 'retorno' seja o seu dataframe
tabela <- tableGrob(retorno, rows = rownames(retorno)) # adicionado os nomes das linhas
# Modifique os parâmetros de texto    
# Gere o PDF
pdf("saida_dataframe.pdf")
grid.arrange(tabela)
dev.off()





# Gere o código LaTeX da tabela
tabela_latex <- xtable::xtable(retorno)
writeLines(print(tabela_latex, include.rownames = FALSE), "retorno.tex")
system("pdflatex tabela_dataframe.tex")


