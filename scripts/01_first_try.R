#########################################################################
# 
# Métodos Numéricos y Optimización: Proyecto final
# 
# Descripción: En este script implementamos una primera versión del problema
# de optimización propuesto por Yang. Usamos funciones de R para resolverlo;
# la idea es tener una primera implementación funcional que luego iremos 
# "ampliando".
# 
# Fecha de creación: 27/04/2019
# Última modificación: 27/04/2019
# 
#########################################################################

library('tidyverse')
library('broom')

t <- 15          # the length of time this program estimate
k <- 1           # the flow time and fractional flow time is in Lk-norm
Njobs <- 100     # how many job in this program


# 1 id, 2 arrive time, 3 size, 4 cpu, 5 finish rate,
# 6 whether arrive, 7 finish time of oco, 8 finish time of SJF 
job <- matrix(rep(0, Njobs*8), ncol = 8)

# Inicializamos matriz job

for (i in 1:Njobs) {
  
  job[i,1] <- i
  
  if (i == 1) job[i, 2] <- 0
  
  if (i > 1) job[i, 2] <- job[i-1, 2] + round(runif(1))
  
  job[i, 3] <- (round(runif(1) * 5) + 1) / 2;
  
  job[i, 4] <- round(runif(1),1)
  
}



d <- job[,4]
I <- diag(nrow = Njobs)
A <- rbind(-d, I, -I, -I)

f <- job[,3] - job[,5]


for(j in 1:t) {
  
  print(j)
  
  fr <- function(x){
    
    N <- length(x)
    
    cv <- (j - job[,2]) / job[,3] + 1
    
    cv %*% x
    
  }
  
  grr <- function(x) {
    
    N <- length(x)
    
    cv <- (j - job[,2]) / job[,3] + 1
    
    cv #%*% rep(1, N)
    
  }
  
  b <- c(-1, rep(0, Njobs), rep(-1, Njobs), -f)
  # theta = rep(10, Njobs)
  theta = optim(par = rep(0, 100),
                fn = function(x) {sum(A %*% x - b)})
  
  
  theta$par
  
  print(min(theta$par))
  
  xopt <- constrOptim(theta = theta$par, 
                   f = fr, 
                   grad = grr,
                   ui = A, 
                   ci = b)
  
  xsol <- tidy(xopt)$value
  
  job[,5] <- job[,5] + xsol;
  
  f <- job[,3] - job[,5]
  
}







