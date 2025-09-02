library(readxl)
library(writexl)

Papers_Metodologia <- read_excel("~/Papers Metodologia.xls", sheet = "WoS")
Papers_Metodologia$`TitlePaper` <-  gsub("[[:punct:]]", "", Papers_Metodologia$`Title Paper`)

Papers_IA <- read_excel("~/Papers Metodologia.xls", sheet = "IA")
Papers_IA$`TitlePaper` <-  gsub("[[:punct:]]", "", Papers_IA$`Title Paper`)

Papers_Completos <- merge(Papers_Metodologia, Papers_IA, by = "TitlePaper", all.x = TRUE)

Papers <- Papers_Completos[c(2,9,10,4,5,6)]

write_xlsx(Papers, path = "Papers.xlsx")

library(dplyr)
validacao <- Papers %>% group_by(Papers$`Title Paper.x`) %>% count()
