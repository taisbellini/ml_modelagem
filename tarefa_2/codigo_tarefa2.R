#---- Exercicio 1 ----

#---- Package installation and load ----
install.packages("glmnet")
library(glmnet)

install.packages("tidyverse")
library(tidyverse)


#---- Funcoes auxiliares ----

norm_random_sample = function(ncol, nrow){
  X = matrix(0, ncol = ncol, nrow = nrow)
  for(j in 1:ncol) {
    X[,j] = rnorm(nrow, sd = sqrt(j))
  } 
  return(X)
}

# 1 - 100 amostras independentes Xi~N(0,sqrt(i))

ncol = 100
nrow = 50

X = norm_random_sample(ncol, nrow)

b = rep(0,ncol)
b[c(1,10,20,50,90)] = rep(5,5)

# aqui definimos o modelo correto
# y = 20 + 5X1 + 5X10+ 5X50 + 5X90 + e (dist normal)
Y = 20 + X%*%b + rnorm(nrow)

# 2 - LASSO

cross_validation = cv.glmnet(X, Y, alpha=1, nfold = 5)
#plot(cross_validation)
opt_lambda = cross_validation$lambda.min
opt_adjust = glmnet(X, Y, alpha=1, lambda = opt_lambda)
cf = coef(opt_adjust)

nonzero_cf = rownames(cf)[cf[,1] != 0]
valid_vars = c("(Intercept)", paste("V",c(1,10,20,50,90), sep = ""))

nonzero_cf

# setdiff (A,B): quais vars A tem que B nao tem
length(setdiff(nonzero_cf, valid_vars))
all(valid_vars %in% nonzero_cf)


# 3 LASSO 1000x


ncol = 100
nrow = 50
valid_beta = c(1,10,20,50,90)

# estruturas para armazenar resultados

opt_lambda_vec_k5 = rep(0, 1000)
opt_lambda_vec_k10 = rep(0, 1000)

exact_vars_count_k5 = 0
exact_vars_count_k10 = 0

contains_vars_count_k5 = 0
contains_vars_count_k10 = 0

# colocar apenas nas posicoes em que acertou, depois limpar os que n sao
aprox_round_coefs_count_vec = rep(0, 1000)



set.seed(2056502)
for (i in 1:1000) {
  
  # amostra
  X = norm_random_sample(ncol, nrow)
  
  # modelo base
  p = 5
  b = rep(0,ncol)
  b[c(1,10,20,50,90)] = rep(5,5)
  Y = 20 + X%*%b + rnorm(nrow)
  
  #ajuste
  
  #k = 5
  cross_validation = cv.glmnet(X, Y, alpha=1, nfold = 5)
  opt_lambda = cross_validation$lambda.min
  opt_adjust = glmnet(X, Y, alpha=1, lambda = opt_lambda)
  
  opt_lambda_vec[i]
  cf = coef(opt_adjust)
  
  
  
  
}



