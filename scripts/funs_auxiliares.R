gradiente <- function(f, x) {
  
  # In:
  # f función a la que se le calculará su gradiente
  # x punto en el que se aproximará el gradiente de f
  # Out:
  # grad_f: aproximación al gradiente de f por diferencias centradas
  
  h      <- 1e-6
  n      <- length(x)
  grad_f <- matrix(rep(0, n), nrow = n)
  xaux   <- x
  xaux2  <- x
  
  for(i in 1:n) {
    
    xaux[i]   <- x[i] + h
    xaux2[i]  <- x[i] - h
    grad_f[i] <- (f(xaux) - f(xaux2)) / (2 * h)
    xaux[i]   <- xaux[i] - h
    xaux2[i]  <- xaux2[i] + h
    
  }
  
  return(grad_f)
  
}


hessiana <- function(f, x) {
  
  # In:
  # f función a la que se le calculará su Hessiana
  # x punto en el que se aproximará la Hessiana de f
  # Out:
  # Hf: aproximación a la Hessiana de f por diferencias hacia delante
  
  h      <- 1e-5
  n      <- length(x)
  Hf     <- matrix(rep(0, n*n), nrow = n)
  xaux_i <- x
  xaux_j <- x
  
  for(i in 1:n) {
    
    xaux_i[i] <- xaux_i[i] + h
    xaux_ij   <- x
    
    for(j in i:n) {
      
      xaux_j[j]  <- xaux_j[j] + h
      xaux_ij[j] <- xaux_i[j] + h
      dif1       <- f(xaux_ij) - f(xaux_i) 
      dif2       <- f(xaux_j) - f(x)
      Hf[i,j]    <- (dif1 - dif2) / (h * h)
      
      if(j != i) Hf[j,i] <- (dif1 - dif2) / (h * h)
      
      xaux_j[j]  <- xaux_j[j] - h
      xaux_ij[j] <- xaux_ij[j] - h
      
    }
    
    xaux_i[i] <- xaux_i[i] - h
    
  }
  
  return(Hf)
  
}


backtracking <- function(alpha, beta, f, dir_desc, x, derivada_direccional) {
  
  # In:
  # alpha, beta parámetros del método por backtracking
  # f función de Rn a R a minimizar
  # dir_desc dirección de descenso en el punto x
  # x punto en el que se buscará el tamaño de paso para la dirección de descenso dada
  # grad_f gradiente de f en x
  # Out: 
  # t tamaño de paso
  optim
  
  t <- 0.001
  
  if(alpha > 0.5) {
    print('alpha de backtracking debe ser menor o igual a 1/2')
    t <-  -1
  }
  
  if(beta > 1) {
    print('beta de backtracking debe ser menor a 1')
    t <-  -1
  }
  
  if(t != -1) {
    
    eval1 <- f(x + t * dir_desc)
    eval2 <- f(x) + alpha * t * derivada_direccional
    
    while (eval1 > eval2) {
      
      t     <- beta * t
      eval1 <- f(x + t * dir_desc)
      eval2 <- f(x) + alpha * t * derivada_direccional
      
    }
    
  } else {
    
    t <- -1
    
  }
  
  return(t)
  
}



newton_Axib <- function(f, x_ast, p_ast, x0, tol, tol_backtracking, maxiter) {
  
  # Entrada:
  # f: función a encontrar el mínimo. Definida como una function handle
  # x_ast: solución de min f(x) sujeto a: Ax=b.
  # p_ast: valor óptimo de f: p_ast = f(x_ast)
  # x0: aproximaciones iniciales a x_ast (mínimo de f) para los algoritmos
  # tol: para el criterio de paro. Típicamente menor o igual a 1e-8. Controla decremento en x 
  # tol_backtracking: para backtracking. Típicamente menor o igual a 1e-14. Controla actualización de x.
  # maxiter: máximo número de iteraciones a realizar.
  # Salida:
  # x: aproximación a x_ast
  # iter: número de iteraciones realizadas (para gráficas de monitoreo)
  # Err_plot: error medido como error absoluto o relativo respecto a p_ast (para gráficas de monitoreo)
  # x_plot: vector de aproximaciones (para gráficas de monitoreo)
  
  library('pracma')
  
  iter <- 1
  x <- x0
  
  # Evaluaciones
  feval <- f(x)
  gfeval <- gradiente(f, x)
  Hfeval <- hessiana(f, x)
  
  normagf <- norm(gfeval, type = '2')
  condHf <- kappa(Hfeval, exact = TRUE)
  
  Err_plot_aux <- matrix(rep(0, maxiter), ncol = 1)
  Err_plot_aux[iter] <- abs(feval - p_ast)
  
  if(norm(x_ast, type = '2') > 1e-300) {
    Err <- norm(x_ast - x, type = '2') / norm(x_ast, type = '2')
  } else {
    Err <- norm(x_ast - x, type = '2')
  }
  
  n <- length(x)
  x_plot_aux <- matrix(rep(0, maxiter * n), nrow = n)
  x_plot <- matrix(rep(0, maxiter * n), nrow = n)
  x_plot[,1] <- x
  
  alpha <- 0.15 # parámetro para el bactracking
  beta <- 0.5 # parámetro para el bactracking
  
  # Definición decremento en x:
  # print(Hfeval)
  dir_Newton <- solve(Hfeval, -gfeval)
  lambda_cuadrada <- t(dir_Newton) %*% (Hfeval %*% dir_Newton)
  
  fprintf('Iter      Normagf     Decremento_en_x     Error_x_ast   Error_p_ast    backtracking_result  Condición_Hf\n')
  fprintf('%3i   %1.6e     %1.6e     %1.6e     %1.6e     %s             %1.6e\n',
          iter, normagf, lambda_cuadrada, Err, Err_plot_aux[iter], '---', condHf);
  
  criterio_de_paro <- lambda_cuadrada / 2
  contador <- 1
  
  while(criterio_de_paro > tol & iter < maxiter) {
    derivada_direccional = -lambda_cuadrada
    t <- backtracking(alpha = alpha, beta = beta, f = f, dir_desc = dir_Newton,
                      x = x, derivada_direccional = derivada_direccional)
    x <- x + t*dir_Newton
    feval <- f(x)
    
    gfeval <- gradiente(f, x)
    Hfeval <- hessiana(f, x)
    normagf <- norm(gfeval, type = '2')
    condHf <- kappa(Hfeval, exact = TRUE)
    # print(Hfeval)
    dir_Newton <- solve(Hfeval, -gfeval)
    lambda_cuadrada <- t(dir_Newton) %*% (Hfeval %*% dir_Newton)
    
    if(norm(x_ast, type = '2') > 1e-300) {
      Err <- norm(x_ast - x, type = '2') / norm(x_ast, type = '2')
    } else {
      Err <- norm(x_ast - x, type = '2')
    }
    
    iter <- iter + 1
    Err_plot_aux[iter] <- abs(feval - p_ast)
    x_plot_aux[,iter-1] <- x
    
    if(contador %% 100 == 0)
      fprintf('%3i   %1.6e     %1.6e     %1.6e     %1.6e     %1.6e    %1.6e\n',
              iter, normagf, lambda_cuadrada, Err, Err_plot_aux[iter], t, condHf);
    
    criterio_de_paro <- lambda_cuadrada / 2
    
    if(t < tol_backtracking) {
      iter_salida <- iter
      iter <- maxiter
    }
    contador <- contador + 1
  }
  
  fprintf('Error utilizando valor de x_ast: %1.6e\n', Err)
  fprintf('Valor aproximado a x_ast:\n')
  print(x)
  Err_plot <- Err_plot_aux[which(abs(Err_plot_aux) > 1e-300)]
  
  helper <- matrix(rep(0, maxiter * n), nrow = n)
  for(i in 1: ncol(x_plot_aux)) {
    helper[,i] <- x_plot_aux[,i] - x_ast
  }
  
  if(norm(x_ast, type = '2') > 1e-300) {
    aux_diferencia_x_plot_aux <- (helper) / norm(x_ast, type = '2')
  } else {
    aux_diferencia_x_plot_aux <- helper
  }
  
  
  index <- which(apply(aux_diferencia_x_plot_aux, 1, function(x) norm(x ,type = '2')) > 1e-300  & rowSums(x_plot_aux) != 0)
  index2 <- apply(aux_diferencia_x_plot_aux, 1, function(x) norm(x ,type = '2')) > 1e-300 & rowSums(x_plot_aux) != 0
  
  if(sum(index) != 0) {
    x_plot[,2:(2+sum(index2)-1)] <- x_plot_aux[,index]
  }
  
  if(iter == maxiter & t < tol_backtracking) {
    disp('Valor de backtracking menor a tol_backtracking, revisar aproximación')
    iter <- iter_salida
  }
  
  
  return(list(x = x, iter = iter, Err_plot = Err_plot, x_plot = x_plot))
  
} 


# fx <- function(x) {
#   sum((x+1)^2) 
# }
# gradiente(fx, c(2,2))
# hessiana(fx, c(2,2))

gamma_cte <- 10;
fx <- function(x) {
  0.5 * (x[1]^2 + gamma_cte*x[2]^2)
}

x_ast <- matrix(c(0,0), ncol = 1)
x0 <- matrix(c(100,100), ncol = 1)
tol <- 1e-14
tol_backtracking <- 1e-14
maxiter <- 30
p_ast <- fx(x_ast)


resultados <- newton_Axib(fx, x_ast,p_ast,x0,tol,tol_backtracking,maxiter)

plot(1:resultados$iter, resultados$Err_plot)
plot(resultados$x_plot[1,], resultados$x_plot[2,])


