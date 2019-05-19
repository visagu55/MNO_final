tol <- 1e-6
tol_backtracking <- 1e-14
maxiter <- 10000
mu <- 2

# Problema 1 -------------------------------------------------------------
t0 <- 10;
fx <- function(x) {
  t0*(2*x[1]+5*x[2]) - log(x[1]+x[2]-6) - log(18-x[1]-2*x[2]) - log(x[1]) - log(x[2])
}

solucion <- c(0,0)

x_ast <- c(6,0.01)
x0 <- c(15,1)
p_ast <- fx(x_ast)

while(4/t0 > tol) {
  
  resultados <- newton_Axib(f = fx, x_ast = x_ast, p_ast = p_ast, x0 = x0, 
                            tol = tol, tol_backtracking = tol_backtracking,
                            maxiter = maxiter)
  
  solucion <- resultados$x
  
  t0 <- t0*mu
  
  fx <- function(x) {
    t0*(2*x[1]+5*x[2]) - log(x[1]+x[2]-6) - log(18-x[1]-2*x[2]) - log(x[1]) - log(x[2])
  }
  
}



mu <- 10
# Problema 3 -------------------------------------------------------------
t0 <- 10;
fx <- function(x) {
  t0*(x[1]^2 + (x[2] + 1)^2) - log(x[1]) - log(x[2])
}

solucion <- c(0,0)

x_ast <- c(0.01,0.01)
x0 <- c(5,5)
p_ast <- fx(x_ast)

while(2/t0 > tol) {
  
  resultados <- newton_Axib(f = fx, x_ast = x_ast, p_ast = p_ast, x0 = x0, 
                            tol = tol, tol_backtracking = tol_backtracking,
                            maxiter = maxiter)
  
  solucion <- resultados$x
  
  t0 <- t0*mu
  
  fx <- function(x) {
    t0*(x[1]^2 + (x[2] + 1)^2) - log(x[1]) - log(x[2])
  }
  
}

# Problema 4 -------------------------------------------------------------
t0 <- 10;
fx <- function(x) {
  t0*(exp(x[1]+3*x[2]-0.1)+exp(x[1]-3*x[2]-0.1)+exp(-x[1]-0.1)) - log(1-(x[1]-1)^2-(x[2]-0.25)^2)
}

solucion <- c(0,0)

x_ast <- c(1,0.25)
x0 <- c(1,0.25)
p_ast <- fx(x_ast)

while(1/t0 > tol) {
  
  resultados <- newton_Axib(f = fx, x_ast = x_ast, p_ast = p_ast, x0 = x0, 
                            tol = tol, tol_backtracking = tol_backtracking,
                            maxiter = maxiter)
  
  solucion <- resultados$x
  
  t0 <- t0*mu
  
  fx <- function(x) {
    t0*(exp(x[1]+3*x[2]-0.1)+exp(x[1]-3*x[2]-0.1)+exp(-x[1]-0.1)) - log(1-(x[1]-1)^2-(x[2]-0.25)^2)
  }
  
}


# Problema 5 -------------------------------------------------------------
t0 <- 10;
X <- matrix(c(0.91177,-1.15847,0.54351,
              2.27585,-0.31595,-0.10009,
              -0.13817,-0.43913,-0.99416,
              0.20257,0.55622,-0.28660, 
              0.56596,0.54013,1.45889), byrow = T, ncol = 3)

y <- matrix(c(1.4339,5.4793,-5.7085,1.1300,9.1538), ncol = 1)

fx <- function(beta) {
  t0*(sum((X %*% beta - y)^2)) - log(beta[1]) - log(beta[2])- log(beta[3]) 
}

solucion <- c(0,0,0)

x_ast <- c(2.9999862, 3.00001679, 3.99997103)
x0 <- rep(1,3)
p_ast <- fx(x_ast)

while(3/t0 > tol) {
  
  resultados <- newton_Axib(f = fx, x_ast = x_ast, p_ast = p_ast, x0 = x0, 
                            tol = tol, tol_backtracking = tol_backtracking,
                            maxiter = maxiter)
  
  solucion <- resultados$x
  
  t0 <- t0*mu
  
  fx <- function(beta) {
    t0*(sum((X %*% beta - y)^2)) - log(beta[1]) - log(beta[2])- log(beta[3]) 
  }
  
}

