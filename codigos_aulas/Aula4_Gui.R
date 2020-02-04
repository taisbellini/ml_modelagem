# Sugestão: Pacote olsrr
# https://cran.r-project.org/web/packages/olsrr/vignettes/intro.html

##########################################
#
#   Pacotes usados nesse exemplo
#
##########################################
# install.packages("glmnet")
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("leaps")


#############################################
#
#  Gerando os dados para o exemplo
#
#############################################
n=120
p=5

set.seed(1100)
x = matrix(ncol = p, nrow = n)
for(i in 1:p) x[,i] = rnorm(n)^i

colnames(x)= paste("X", 1:5, sep = "")
coef=c(10,round(rnorm(p-2),2),0,0)
names(coef) = c("(Intercept)", colnames(x))
    
# coeficientes do modelo:
coef
# gerando Y = X*beta + e
y=cbind(rep(1,n),x)%*%coef+rnorm(n)
cor(y,x)


#############################################
#
#  Ajustando o modelo: 
#
#############################################

### BACKWARD

m1=lm(y~x)
summary(m1)

m2=lm(y~x[,-4])
summary(m2)

m3=lm(y~x[,-c(4,5)])
summary(m3)

m4=lm(y~x[,-c(5)])
summary(m4)

final.B = lm(y~x[,1:3])
cf.B = coefficients(final.B); names(cf.B) = names(coef)[0:3+1]

#### FORWARD

m1=lm(y~x[,1])
summary(m1)
m2=lm(y~x[,2])
summary(m2)
m3=lm(y~x[,3])
summary(m3)
m4=lm(y~x[,4])
summary(m4)

m1=lm(y~x[,c(3,1)])
summary(m1)
m2=lm(y~x[,c(3,2)])
summary(m2)
m3=lm(y~x[,c(3,4)])
summary(m3)
m4=lm(y~x[,c(3,5)])
summary(m4)

m1=lm(y~x[,c(3,2,1)])
summary(m1)
m2=lm(y~x[,c(3,2,4)])
summary(m2)
m3=lm(y~x[,c(3,2,5)])
summary(m3)

m1=lm(y~x[,c(3,2,1,4)])
summary(m1)

m2=lm(y~x[,c(3,2,1,5)])
summary(m2)

final.F =lm(y~x[,c(3,2,1)])
cf.F = coefficients(final.F); names(cf.F) = names(coef)[c(0,3,2,1)+1]

# comparando
res = matrix(0, ncol = 3, nrow = length(coef))
rownames(res) = names(coef)
colnames(res) = c("Coeficiente", "Estimativa.B", "Estimativa.F")
res[, 1] = coef
res[names(cf.B),2] = cf.B
res[names(cf.F),3] = cf.F
round(res,2)

################

### Baseado em erro de previs?o:
# install.packages("tidyverse")
# install.packages("caret")
# install.packages("leaps")

library(tidyverse)
library(caret)
library(leaps)
library(MASS)

dados = data.frame(x,y)

# Fit the full model
full.model <- lm(y~x)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both",
                      trace = FALSE)
summary(step.model)

models <- regsubsets(y~., data=dados, nvmax = 4,
                     method = "forward")
summary(models)


set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(y ~., data = dados,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
)
step.model$results

step.model$bestTune

summary(step.model$finalModel)
coef.cv = coef(step.model$finalModel, 3)

res = cbind(res, rep(0,p+1))
res[names(coef.cv),4] = coef.cv 
colnames(res)[4] = "Estimativa.cv"
res

#################

install.packages("glmnet")
library(glmnet)

# Método para obter os coeficientes = Coordinate descent
# pag 8:  https://cran.r-project.org/web/packages/glmnet/vignettes/glmnet.pdf

# alpha = 0 -> Ridge
# alpha = 1 -> LASSO

#-------- Lasso ------------------------
#### glmnet: ajusta o modelo para vários lambdas (grade default ou personalizada)
ajuste = glmnet(x, y, alpha=1)

# coefficientes associados a cada lambda:
coefs.ajuste = coef(ajuste)
coefs.ajuste
dim(coefs.ajuste)  
coefs.ajuste[,10]  # coefs. para o décimo lambda da seq.
# eixo-x = lambda (no topo = nr. de coefs nao zero)
# eixo-y norma dos coeficientes
plot(ajuste)
# escolhe baseado no tamanho da amostra e qtde de variaveis (pra zerar mais ou menos coefs)
range(ajuste$lambda)

# coeficients para um outro valor de lambda
predict(ajuste, s = 0.001, type = "coefficients")
predict(ajuste, s = 1.21, type = "coefficients")

# valores preditos 
yhat = predict(ajuste, s = ajuste$lambda[60], type = "response", newx = x)
yhat2 = predict(ajuste, s = ajuste$lambda[10], type = "response", newx = x)
plot(y)
lines(yhat, col = "red")
lines(yhat2, col = "blue")

### cv.glmnet - essa diz qual eh o lambda otimo usa validação cruzada:
validacaoCruzada = cv.glmnet(x, y, alpha=1)
plot(validacaoCruzada) 
lambdaOtimo = validacaoCruzada$lambda.min
# os coeficients para o lambda ótimo pode ser obtidos 
coefficients(ajuste, s = lambdaOtimo)

# mesmo resultado será obtido usando a função predict
predict(ajuste, s = lambdaOtimo, type = "coefficients")

# se não tivéssemos a variável "ajuste":
ajuste.otimo = glmnet(x, y, alpha=1, lambda = lambdaOtimo)
coef(ajuste.otimo)


#-------- Ridge ------------------------

# raro zerar coeficientes

ajusteR = glmnet(x, y, alpha=0)
plot(ajusteR)
validacaoCruzada = cv.glmnet(x, y, alpha=0)
plot(validacaoCruzada)
lambdaOtimo = validacaoCruzada$lambda.min
coefficients(ajusteR, s = lambdaOtimo)



################################
# p maior que n
ncol = 100
nrow = 50
#gerar matriz com distribuições diferentes
X = matrix(0, ncol = ncol, nrow = nrow)
#for(j in 1:ncol) X[,j] = rnorm(nrow, sd = sqrt(j)) 
for(j in 1:ncol) X[,j] = rexp(nrow, rate = sqrt(j)) 

# 5 vars que realmente importam
p = 5
beta = rep(0,ncol)
beta[c(1,10,20,50,90)] = rep(10,5)

#aqui definimos o modelo correto
#y = 20 + 10X1 + 10X10+ 10X50 ... + e (dist normal)
Y = 20 + X%*%beta + rnorm(nrow)

#pergunta: sera que o lasso vai encontrar os coeficientes certos?

validacaoCruzada = cv.glmnet(X, Y, alpha=1, nfold = 5)
plot(validacaoCruzada)
lambdaOtimo = validacaoCruzada$lambda.min
ajuste.otimo = glmnet(X, Y, alpha=1, lambda = lambdaOtimo)
cf = coef(ajuste.otimo)
cf
nm = rownames(cf)[cf[,1] != 0]
nm
round(cf[nm,],4)

vars = paste("V",c(1,10,20,50,90), sep = "")
sum(vars %in% nm)

# tem um modelo com 100 variaveis, mas apenas 5 são uteis (como definiu? como validou?)
# sabemos quais sao exatamente as variaveis
# a terefa é aplicar o Lasso no modelo e ver se ele acerta quais sao as nao nulas uteis
#qtas vezes o lasso vai acertar quais são as variaveis certas em 100 repetições?

#E nesse caso: #for(j in 1:ncol) X[,j] = rexp(nrow, rate = sqrt(j)) ??
# Dai começa a ter problemas

#Testar com coeficientes de outras magnitudes (0.1, 0.5, 20, 60)

ncol = 100
nrow = 50
#gerar matriz com distribuições diferentes
X = matrix(0, ncol = ncol, nrow = nrow)
#for(j in 1:ncol) X[,j] = rnorm(nrow, sd = sqrt(j)) 
for(j in 1:ncol) X[,j] = rexp(nrow, rate = sqrt(j)) 

# 5 vars que realmente importam
p = 5
beta = rep(0,ncol)
beta[c(1,10,20,50,90)] = rep(10,5)

#aqui definimos o modelo correto
#y = 20 + 10X1 + 10X10+ 10X50 ... + e (dist normal)
Y = 20 + X%*%beta + rnorm(nrow)

#pergunta: sera que o lasso vai encontrar os coeficientes certos?


# -------------- Se agora o coeficiente é 1------------------

ncol = 100
nrow = 50
#gerar matriz com distribuições diferentes
X = matrix(0, ncol = ncol, nrow = nrow)
#for(j in 1:ncol) X[,j] = rnorm(nrow, sd = sqrt(j)) 
for(j in 1:ncol) X[,j] = rexp(nrow, rate = sqrt(j)) 

# 5 vars que realmente importam
p = 5
beta = rep(0,ncol)
beta[c(1,10,20,50,90)] = rep(1,5)

#aqui definimos o modelo correto
#y = 20 + 10X1 + 10X10+ 10X50 ... + e (dist normal)
Y = 20 + X%*%beta + rnorm(nrow)


validacaoCruzada = cv.glmnet(X, Y, alpha=1, nfold = 5)
plot(validacaoCruzada)
lambdaOtimo = validacaoCruzada$lambda.min
ajuste.otimo = glmnet(X, Y, alpha=1, lambda = lambdaOtimo)
cf = coef(ajuste.otimo)
cf
nm = rownames(cf)[cf[,1] != 0]
nm
round(cf[nm,],4)

sum(vars %in% nm)
