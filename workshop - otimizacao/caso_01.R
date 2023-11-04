# Workshop de Otimização: caso fábrica de banheiras

# Programacao linear
library(lpSolveAPI)

# Cria modelo
modelo <- make.lp(
  ncol = 2  # Quantidade de variaveis de decisao
)

# Indica se e "max" 
lp.control(
  lprec = modelo, 
  sense = "max"
)

# Definicao da funcao objetivo
set.objfn(
  lprec = modelo,
  obj = c(350, 300)
)

# Definicao das restrições
add.constraint(
  lprec = modelo, 
  xt = c(1, 1) , 
  type = "<=", 
  rhs = 200
)

add.constraint(
  lprec = modelo, 
  xt = c(9, 6) , 
  type = "<=", 
  rhs = 1566
)


add.constraint(
  lprec = modelo, 
  xt = c(12, 16) , 
  type = "<=", 
  rhs = 2880
)

# Executa solução do modelo
solve(modelo)
?lpSolveAPI::solve.lpExtPtr()

# Resultado da funcao objetivo
get.objective(modelo)

# Valores de cada variavel de decisao
get.variables(modelo)
