
library('tidyverse')

t <- 12 # ¿Cuánto tiempo tenemos para analizar el sistema?
k <- 1  # the flow time and fractional flow time is in Lk-norm
numofjob <- 6 # número de tareas simuladas
lambda <- 400 # lambda necesaria incremental del problema dual
time <- 1:t

# 1 id, 
# 2 arrive time, 
# 3 size, 
# 4 cpu, 
# 5 finish rate, 
# 6 whether arrive, 
# 7 finish time of oco
job           <- matrix(rep(0, 7*numofjob), ncol = 7) 
schedule      <- matrix(rep(0, t*numofjob), ncol = t) # the scheduling result of this oco method
fft           <- matrix(rep(0, numofjob), nrow = 1) # fractional flow time
ocofractional <- matrix(rep(0, t), ncol = 1) # the fractional flow time of each time
ocoflow       <- matrix(rep(0, t), ncol = 1) # the flow time of each time

job[,1] <- 1:6 # ID
job[,2] <- rep(0, 6) # arrive_time. Aquí estoy inicializando todos como ceros para el problema offline
job[,3] <- c(0.1, 0.2, 1, 3, 0.5, 0.8) # size
job[,4] <- rep(1, 6) # CPU

## Se simula la información para el problema online
# for (i in 1:numofjob) {
#   job[i,1] <- i
#   if (i == 1) job[i, 2] <- 0
#   if (i > 1) job[i, 2] <- job[i-1, 2] + round(runif(1))
#   job[i, 3] <- (round(runif(1) * 5) + 1) / 2;
#   job[i, 4] <- round(runif(1),1)
# }

# this part is for online convex optimization
for(j in 1:t) {
  
  print(j)
  
  for(i in 1:numofjob) {
    if(job[i,2] >= j) {
      fft[1,i] <- 0
      job[i,6] <- 0 # not arrive
    } else {
      fft[1,i] <- (j-job[i,2])^k / job[i,3] + job[i,3]^(k-1)
      job[i,6] <- 1 # arrive
    }
  }
  
  # resolviendo con disciplined convex optimization
  # cvx_begin quiet
  #     variable x(numofjob)
  #     minimize(fft * x + lamda * job(:,6)' * ((job(:,3)-job(:,5)-x ).* (t*ones(numofjob,1)-job(:,2))) + (x'*x)/100)
  #     subject to
  #         job(:,4)'* x <= 1;
  #         x <= ones(numofjob,1);
  #         x >= zeros(numofjob,1);
  #         x <= job(:,3)-job(:,5); %el faltante
  # cvx_end
  
  fx <- function()
  
  
  
  
  
  
}










