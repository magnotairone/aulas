# Bibliotecas utilizadas --------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)


# SeriesTemporais_Autoveiculos ---------------------------------------
# fonte: ANFAVEA https://anfavea.com.br/estatisticas
df <- read_excel("SeriesTemporais_Autoveiculos.xlsm",
                 skip = 4)

df %>% glimpse()

View(df)







# tabela3416 ----------------------------------------------------------
# indice de volume e de receita nominal de vendas no comercio varejista
# fonte: IBGE https://sidra.ibge.gov.br/home/

df <- read_csv(file="tabela3416.csv", skip = 3, n_max = 2)







# tabela6443 ----------------------------------------------------
# Índice de receita nominal de serviços 
df <- read_csv(file="tabela6443.xlsx", 
               skip = 2, n_max = 21)

df <- read_xlsx("tabela6443.xlsx",
                sheet = "Índice de receita nominal de...",
                skip = 2, n_max = 20)






# tabela1737 --------------------------------------------------------------
# IPCA - Série histórica com número-índice, variação mensal e variações acumuladas
df <- read_xlsx("dados/dados_publicos/tabela1737.xlsx",
                sheet = "Tabela 1",
                skip = 2, n_max = 2)





