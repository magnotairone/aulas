# Workshop de Otimização: problema de mistura de ração

# Manipulacao de dados
library(tidyverse)

# Programacao linear
library(lpSolveAPI)

# Cria modelo
modelo <- make.lp(
  ncol = 4  # Quantidade de variaveis de decisao
)

# Indica se e "min" minimizacao 
lp.control(
  lprec = modelo, 
  sense = "min"
)

# Definicao da funcao objetivo
set.objfn(
  lprec = modelo,
  obj = c(0.25, 0.30, 0.32, 0.15)
)

add.constraint(
  lprec = modelo, 
  xt = c(1, 1, 1, 1) , 
  type = "=", 
  rhs = 8000
)

add.constraint(
  lprec = modelo, 
  xt = c(0.3/8000, 0.05/8000, 0.2/8000, 0.1/8000), 
  type = ">=", 
  rhs = 0.2
)

add.constraint(
  lprec = modelo, 
  xt = c(0.1/8000, 0.3/8000, 0.15/8000, 0.1/8000), 
  type = ">=", 
  rhs = 0.15
)

add.constraint(
  lprec = modelo, 
  xt = c(0.2/8000, 0.2/8000, 0.2/8000, 0.3/8000), 
  type = ">=", 
  rhs = 0.15
)

# Executa solução do modelo
solve(modelo)

# Resultado da funcao objetivo
get.objective(modelo)

# Valores de cada variavel de decisao
get.variables(modelo)

