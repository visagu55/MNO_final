library('tidyverse')
library('pracma')

f <- function(x) {
  2*x[1]+5*x[2]
}

f_rest <- c(f1 = function(x) -(x[1]+x[2]-6), 
                        f2 = function(x) -(18-x[1]-2*x[2]), 
                        f3 = function(x) -x[1], 
                        f4 = function(x) -x[2])



# bind_rows(map(f_rest, ~.(x0)))

A <- matrix(c(0,0), nrow = 1)
b <- 0
x_ast <- c(6,0)
x0 <- c(4,4)
tol_outer_iter = 1e-6
tol_inner_iter = 1e-5
tol_backtracking = 1e-14
maxiter_path = 30
maxiter_Newton = 30
mu = 10

p_ast = f(x_ast)
# p_ast <- 12

a <- path_following(f, f_rest, A, b, x_ast, p_ast, x0, tol_outer_iter,
               tol_inner_iter, tol_backtracking, maxiter_path, maxiter_Newton, mu)

a
gradiente(f, c(1,1))
hessiana_rest(f_rest, c(1,1))
