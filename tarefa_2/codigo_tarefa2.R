#---- Exercicio 1 ----

#---- Package installation and load ----
#install.packages("glmnet")
library(glmnet)

#install.packages("tidyverse")
library(tidyverse)

install.packages('stargazer')


#---- Funcoes auxiliares ----

norm_random_sample = function(ncol, nrow){
  X = matrix(0, ncol = ncol, nrow = nrow)
  for(j in 1:ncol) {
    X[,j] = rnorm(nrow, sd = sqrt(j))
  } 
  return(X)
}

exp_random_sample = function(ncol, nrow){
  X = matrix(0, ncol = ncol, nrow = nrow)
  for(j in 1:ncol) {
    X[,j] = rnorm(nrow, sd = sqrt(j))
  } 
  return(X)
}

# 3 LASSO 1000x

#---- estruturas para armazenar resultados ----

opt_lambda_k5 = matrix(0, ncol=2, nrow=1000)
colnames(opt_lambda_k5) = c('lambda', 'acertou')

opt_lambda_k10 = matrix(ncol=2, nrow=1000)
colnames(opt_lambda_k10) = c('lambda', 'acertou')

hits_count = matrix(ncol=2, nrow=2)
colnames(hits_count) = c('k=5', 'k=10')
rownames(hits_count) = c('Exato', 'Inclui')
hits_count[,1] = rep(0, 2)
hits_count[,2] = rep(0, 2)

valid_vars = c("(Intercept)", paste("V",c(1,10,20,50,90), sep = ""))

# colocar apenas nas posicoes em que acertou, depois limpar os que n sao
coefs_k5 = matrix(ncol=2, nrow=1000)
colnames(coefs_k5) = c('Intercept', 'Media Coeficientes')

coefs_k10 = matrix(ncol=2, nrow=1000)
colnames(coefs_k10) = c('Intercept', 'Media Coeficientes')

#---- Loop LASSO ----
ncol = 100
nrow = 50
valid_beta = c(1,10,20,50,90)
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
  
  #k=5
  cross_validation = cv.glmnet(X, Y, alpha=1, nfold = 5)
  opt_lambda = cross_validation$lambda.min
  opt_lambda_k5[i,1] = opt_lambda
  opt_adjust = glmnet(X, Y, alpha=1, lambda = opt_lambda)
  cf = coef(opt_adjust)
  nonzero_cf = rownames(cf)[cf[,1] != 0]
  
  # setdiff (A,B): quais vars A tem que B nao tem
  if (length(setdiff(nonzero_cf, valid_vars)) == 0) {
    hits_count[1,1] = hits_count[1,1] + 1
    # falar no relatorio que usou round para validar
    model = cf[nonzero_cf,]
    model_intercept = model[1:1]
    model_coefs = model[2:6]
    coefs_k5[i,1] = model_intercept
    coefs_k5[i,2] = mean(round(model_coefs))
    opt_lambda_k5[i,2] = 1
  } else{
    if(all(valid_vars %in% nonzero_cf)){
      hits_count[2,1] = hits_count[2,1] + 1
    }
    opt_lambda_k5[i,2] = 0
  }
  
  #k=10
  cross_validation = cv.glmnet(X, Y, alpha=1, nfold = 10)
  opt_lambda = cross_validation$lambda.min
  opt_lambda_k10[i,1] = opt_lambda
  opt_adjust = glmnet(X, Y, alpha=1, lambda = opt_lambda)
  cf = coef(opt_adjust)
  nonzero_cf = rownames(cf)[cf[,1] != 0]
  
  # setdiff (A,B): quais vars A tem que B nao tem
  if (length(setdiff(nonzero_cf, valid_vars)) == 0) {
    hits_count[1,2] = hits_count[1,2] + 1
    # falar no relatorio que usou round para validar
    model = cf[nonzero_cf,]
    model_intercept = model[1:1]
    model_coefs = model[2:6]
    coefs_k10[i,1] = model_intercept
    coefs_k10[i,2] = mean(round(model_coefs))
    opt_lambda_k5[i,2] = 1
  } else{
    if(all(valid_vars %in% nonzero_cf)){
      hits_count[2,2] = hits_count[2,2] + 1
    }
    opt_lambda_k10[i,2] = 0
  }
}

hits_count
coefs_k5
coefs_k5 = coefs_k5[!is.na(coefs_k5[,'Intercept'])]
coefs_k5
coefs_k10
opt_lambda_k5
opt_lambda_k10

colnames(coefs_k5)
coefs_k5 = subset(coefs_k5, "Intercept" != 0)
mean(coefs_k5[,'Intercept'])

hist(opt_lambda_k5[, "lambda"])
plot(opt_lambda_k5[, "lambda"])
summary(opt_lambda_k5[, "lambda"])

install.packages("ggplot2")
library(ggplot2)
ggplot(opt_lambda_k5, aes(x = opt_lambda_k5[, "acertou"], y = opt_lambda_k5[, "lambda"])) +
  geom_boxplot()

# 1 - 50 amostras independentes Xi~N(0,sqrt(i))

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
opt_lambda = cross_validation$lambda.min
opt_adjust = glmnet(X, Y, alpha=1, lambda = opt_lambda)
cf = coef(opt_adjust)


nonzero_cf = rownames(cf)[which(cf[,1] != 0)]
valid_vars = c(paste("V",c(1,10,20,50,90), sep = ""))
model = cf[nonzero_cf,]
model_int = model[1:1]
model_coefs = model[2:6]
round(cf[nonzero_cf,],4)

nonzero_cf

# setdiff (A,B): quais vars A tem que B nao tem
length(setdiff(nonzero_cf, valid_vars))
all(valid_vars %in% nonzero_cf)


# var resposta 12345

opt_lambda_k5 = data.frame(ncol=2, nrow=1000)
colnames(opt_lambda_k5) = c('lambda', 'acertou')

opt_lambda_k10 = data.frame(ncol=2, nrow=1000)
colnames(opt_lambda_k10) = c('lambda', 'acertou')

hits_count = matrix(ncol=2, nrow=2)
colnames(hits_count) = c('k=5', 'k=10')
rownames(hits_count) = c('Exato', 'Inclui')
hits_count[,1] = rep(0, 2)
hits_count[,2] = rep(0, 2)

valid_vars = c("(Intercept)", paste("V",c(1,10,20,50,90), sep = ""))

# colocar apenas nas posicoes em que acertou, depois limpar os que n sao
coefs_k5 = data.frame(0, ncol=2, nrow=1000)
colnames(coefs_k5) = c('Intercept', 'Media Coeficientes')

coefs_k10 = data.frame(0, ncol=2, nrow=1000)
colnames(coefs_k10) = c('Intercept', 'Media Coeficientes')


ncol = 100
nrow = 50
valid_beta = c(1,2,3,4,5)
set.seed(2056502)
for (i in 1:1000) {
  
  # amostra
  #X = exp_random_sample(ncol, nrow)
  X = matrix(0, ncol = ncol, nrow = nrow)
  for(j in 1:ncol) {
    X[,j] = rchisq(nrow, 1)
  }
  
  # modelo base
  p = 5
  b = rep(0,ncol)
  b[c(1,2,3,4,5)] = rep(5,5)
  Y = 20 + X%*%b + rchisq(nrow, 1)
  
  #ajuste
  
  #k=5
  cross_validation = cv.glmnet(X, Y, alpha=1, nfold = 5)
  opt_lambda = cross_validation$lambda.min
  opt_lambda_k5[i,1] = opt_lambda
  opt_adjust = glmnet(X, Y, alpha=1, lambda = opt_lambda)
  cf = coef(opt_adjust)
  nonzero_cf = rownames(cf)[cf[,1] != 0]
  
  # setdiff (A,B): quais vars A tem que B nao tem
  if (length(setdiff(nonzero_cf, valid_vars)) == 0) {
    hits_count[1,1] = hits_count[1,1] + 1
    # falar no relatorio que usou round para validar
    model = cf[nonzero_cf,]
    model_intercept = model[1:1]
    model_coefs = model[2:6]
    coefs_k5[i,1] = model_intercept
    coefs_k5[i,2] = mean(round(model_coefs))
    opt_lambda_k5[i,2] = 1
  } else{
    if(all(valid_vars %in% nonzero_cf)){
      hits_count[2,1] = hits_count[2,1] + 1
    }
    opt_lambda_k5[i,2] = 0
  }
  
  #k=10
  cross_validation_k10 = cv.glmnet(X, Y, alpha=1, nfold = 10)
  opt_lambda10 = cross_validation$lambda.min
  opt_lambda_k10[i,1] = opt_lambda10
  opt_adjust_k10 = glmnet(X, Y, alpha=1, lambda = opt_lambda10)
  cf_k10 = coef(opt_adjust_k10)
  nonzero_cf_k10 = rownames(cf_k10)[cf_k10[,1] != 0]
  
  # setdiff (A,B): quais vars A tem que B nao tem
  if (length(setdiff(nonzero_cf_k10, valid_vars)) == 0) {
    hits_count[1,2] = hits_count[1,2] + 1
    # falar no relatorio que usou round para validar
    model_k10 = cf_k10[nonzero_cf_k10,]
    model_intercept_k10 = model_k10[1:1]
    model_coefs_k10 = model_k10[2:6]
    coefs_k10[i,1] = model_intercept_k10
    coefs_k10[i,2] = mean(round(model_coefs_k10))
    opt_lambda_k10[i,2] = 1
  } else{
    if(all(valid_vars %in% nonzero_cf_k10)){
      hits_count[2,2] = hits_count[2,2] + 1
    }
    opt_lambda_k10[i,2] = 0
  }
}
hits_count
nonzero_cf
hist(opt_lambda_k10[,1])
