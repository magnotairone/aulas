library(tidyr)   # funcoes tidy
library(dplyr)   # manipulacao de dados
library(stringr) # manipulacao de strings
library(forcats) # manipulacao de dados qualitativos
library(ggplot2) # criacao de graficos
library(plotly)  # criacao de graficos interativos 

# Estudo de caso 1: World Health Organization

?who # descricao dos dados

names(who) # nomes das colunas 

View(who) # visualizar planilha dos dados

glimpse(who) # ver a estrutura dos dados