x_ast <- c(6,0)
x0 <- c(4,4)
tol_outer_iter = 1e-6
tol_inner_iter = 1e-5
tol_backtracking = 1e-14
maxiter_path = 30
maxiter_Newton = 30
mu = 10

# Problema 1 -------------------------------------------------------------
f <- function(x) {
  2*x[1]+5*x[2]
}

f_rest <- c(f1 = function(x) -(x[1]+x[2]-6), 
            f2 = function(x) -(18-x[1]-2*x[2]), 
            f3 = function(x) -x[1], 
            f4 = function(x) -x[2])


a <- path_following(f, f_rest, A, b, x_ast, p_ast, x0, tol_outer_iter,
                    tol_inner_iter, tol_backtracking, maxiter_path, maxiter_Newton, mu)

a


# Problema 2 -------------------------------------------------------------
mu <- 100
t0 <- 10;
fx <- function(x) {
  t0*(x[1]^2 + x[2]^2 + x[3]^2 - x[4]^2 - 2*x[1] - 3*x[4] - 
        log(2*x[1] + x[2] + x[3] + 4*x[4] - 7) -
        log(-(2*x[1] + x[2] + x[3] + 4*x[4] - 7)) -
        log(x[1] + x[2] + 2*x[3] + x[4] - 6) -
        log(-(x[1] + x[2] + 2*x[3] + x[4] - 6)) -
        log(x[1]) - log(x[2]) - log(x[3]) - log(x[4]) -
        log(-x[1]) - log(-x[2]) - log(-x[3]) - log(-x[4]))
}

solucion <- c(0,0,0,0)

x_ast <- c(1.1232876712328763,0.6506849315068493,1.8287671232876714,0.5684931506849317)
x0 <- c(0,8,-1,0)
p_ast <- fx(x_ast)

while(12/t0 > tol) {
  
  resultados <- newton.sin.rest(f = fx, x_ast = x_ast, p_ast = p_ast, x0 = x0, 
                            tol = tol, tol_backtracking = tol_backtracking,
                            maxiter = maxiter)
  
  solucion <- resultados$x
  
  t0 <- t0*mu
  
  fx <- function(x) {
    t0*(x[1]^2 + x[2]^2 + x[3]^2 - x[4]^2 - 2*x[1] - 3*x[4] - 
          log(2*x[1] + x[2] + x[3] + 4*x[4] - 6) -
          log(-(2*x[1] + x[2] + x[3] + 4*x[4] - 6)) -
          log(x[1] + x[2] + 2*x[3] + x[4] - 6) -
          log(-(x[1] + x[2] + 2*x[3] + x[4] - 6)) -
          log(x[1]) - log(x[2]) - log(x[3]) - log(x[4]) -
          log(-x[1]) - log(-x[2]) - log(-x[3]) - log(-x[4]))
  }
  
}


# Problema 3 -------------------------------------------------------------

f <- function(x) {
  x[1]^2 + (x[2] + 1)^2
}

f_rest <- c(f1 = function(x) -x[1], 
            f2 = function(x) -x[2])

solucion <- c(0,0)

x_ast <- c(0.01,0.01)
x0 <- c(5,5)
p_ast <- f(x_ast)

a <- path_following(f, f_rest, A, b, x_ast, p_ast, x0, tol_outer_iter,
                    tol_inner_iter, tol_backtracking, maxiter_path, maxiter_Newton, mu)

a

# Problema 4 -------------------------------------------------------------
f <- function(x) {
  exp(x[1]+3*x[2]-0.1)+exp(x[1]-3*x[2]-0.1)+exp(-x[1]-0.1)
}

f_rest <- c(f1 = function(x) -(1-(x[1]-1)^2-(x[2]-0.25)^2))

x_ast <- c(0.28,0.014)
x0 <- c(1,1)
p_ast <- f(x_ast)

a <- path_following(f, f_rest, A, b, x_ast, p_ast, x0, tol_outer_iter,
                    tol_inner_iter, tol_backtracking, maxiter_path, maxiter_Newton, mu)

a


# Problema 5 -------------------------------------------------------------
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
  
  resultados <- newton.sin.rest(f = fx, x_ast = x_ast, p_ast = p_ast, x0 = x0, 
                            tol = tol, tol_backtracking = tol_backtracking,
                            maxiter = maxiter)
  
  solucion <- resultados$x
  
  t0 <- t0*mu
  
  fx <- function(beta) {
    t0*(sum((X %*% beta - y)^2)) - log(beta[1]) - log(beta[2])- log(beta[3]) 
  }
  
}

