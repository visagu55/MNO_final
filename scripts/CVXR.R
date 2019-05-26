#Leer la librería
#En caso de error al crear la librería instalar los siguientes paquetes desde ubuntu
#sudo apt-get install libgmp-dev
#sudo apt-get install libmpfr-dev

install.packages('CVXR', repos = "https://CRAN.R-project.org")
suppressWarnings(library(CVXR, warn.conflicts=FALSE))

f <- function(x) {2*x[1]+5*x[2]}
x <- Variable(2) ##número de variables
#Función objetivo
objective <- Minimize(f(x))
## Restricciones
constraints <- list(
6-x[1]-x[2] <= 0,
-18+x[1]+2*x[2]<=0,
x[1]>=0,
x[2]>=0
)
## Solve the catenary problem
prob <- Problem(objective, constraints)
result <- solve(prob)
cat(sprintf("Optimal value: %.3f\n", result$value))
vector_solucion <- result$getValue(x)
vector_solucion

