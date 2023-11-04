library(janitor)    # limpeza de dados
library(tidyverse)  # inclui todos os pacotes do tidyverse
library(readxl)     # leitura de arquivo excel


dados_brutos <- read_excel("pacientes.xlsx")

View(dados_brutos) # visualizar dados como planilha

glimpse(dados_brutos) # ver a estrutura dos dados

names(dados_brutos)  # ver o nome das colunas