# Importar dados #
data = read.csv2("205650.txt", sep=" ", dec = ".")

# (a) Dividir a amostra aleatoriamente em duas partes: treinamento (85%) e teste (15%) #
train_size <- floor(0.85 * nrow(data))
set.seed(205650)
train_index <- sample(seq_len(nrow(data)), size = train_size)
train <- data[train_index, ]
test <- data[-train_index, ]

split_data = function(data){
  
}
  
# (b) #
# Regressao simples (MQO) #
reg = lm(Y ~ X, data=train)

# Previsao de cada ponto amostra de teste #
#Y = reg$coefficients[1] + reg$coefficients[2]*test$X
test_Ypred = predict(reg, test)

# Erro absoluto medio teste #
test_abs_error = abs(test$Y - test_Ypred)
MAEout = sum(test_abs_error)/15

# Erro absoluto medio treinamento #
train_Ypred = predict(reg, train)
train_abs_error = abs(train$Y - train_Ypred)
MAEin = sum(train_abs_error)/85


# (c) Funcao que calcula melhor reta para minimizar L1 #

#revisar questao de test/train
opt_l1_loss = function(dataset){
  l1_loss = function(y, f) {
    sum(abs(y - f))
  }
  
  fn = function(params){
    b0 = params[1]
    b1 = params[2]
    l1_loss(y = dataset$Y, f = (b0 + b1 * dataset$X))
  }
  
  opt_b = optim(
    par = c(b0 = 0, b1=0),
    fn = fn
  )
  opt_b$par
}

# (d) #
train_Ypred_l1 = opt_l1_loss(train)[1] + opt_l1_loss(train)[2]*train$X
test_Ypred_l1 = opt_l1_loss(test)[1] + opt_l1_loss(test)[2]*test$X

# Erro absoluto medio teste L1 #
test_abs_error_l1 = abs(test$Y - test_Ypred_l1)
MAEout_l1 = sum(test_abs_error_l1)/15

# Erro absoluto medio treinamento L1 #
train_abs_error_l1 = abs(train$Y - train_Ypred_l1)
MAEin_l1 = sum(train_abs_error_l1)/85

MAE_reg = c(MAEin, MAEout)
MAE_l1 = c(MAEin_l1, MAEout_l1)

setNames(MAE_reg, MAE_l1)

# (e) #
b_vec = rep(0, 100)
est_b_list = data.frame(b0_reg=b_vec,
                   b1_reg=b_vec,
                   b0_l1=b_vec,
                   b1_l1=b_vec)

MAEin_reg_list = rep(0, 100)
MAEout_reg_list = rep(0, 100)
MAEin_l1_list = rep(0,100)
MAEout_l1_list = rep(0,100)

set.seed(205650)
for (i in seq(1:100)){
  train_index <- sample(seq_len(nrow(data)), size = train_size)
  train <- data[train_index, ]
  test <- data[-train_index, ]
  
  reg = lm(Y ~ X, data=train)
  est_b_list$b0_reg[i] = reg$coefficients[1]
  est_b_list$b1_reg[i] = reg$coefficients[2]
  
  test_Ypred = predict(reg, test)
  train_Ypred = predict(reg, train)
  
  test_abs_error = abs(test$Y - test_Ypred)
  MAEout = sum(test_abs_error)/15
  MAEout_reg_list[i] = MAEout
  
  train_abs_error = abs(train$Y - train_Ypred)
  MAEin = sum(train_abs_error)/85
  MAEin_reg_list[i] = MAEin
  
  train_Ypred_l1 = opt_l1_loss(train)[1] + opt_l1_loss(train)[2]*train$X
  test_Ypred_l1 = opt_l1_loss(test)[1] + opt_l1_loss(test)[2]*test$X
  
  est_b_list$b0_l1[i] = opt_l1_loss(train)[1]
  est_b_list$b1_l1[i] = opt_l1_loss(train)[2]
  
  # Erro absoluto medio teste L1 #
  test_abs_error_l1 = abs(test$Y - test_Ypred_l1)
  MAEout_l1 = sum(test_abs_error_l1)/15
  
  # Erro absoluto medio treinamento L1 #
  train_abs_error_l1 = abs(train$Y - train_Ypred_l1)
  MAEin_l1 = sum(train_abs_error_l1)/85
  
}

View(est_b_list)
install.packages("ggplot2")
library(ggplot2)

# b0
boxplot(est_b_list$b0_reg)
summary(est_b_list$b0_reg)

#b1
boxplot(est_b_list$b1_reg)
summary(est_b_list$b1_reg)



ggplot(est_b_list, aes(x="", y=b0)) + 
  geom_boxplot() + 
  theme_bw()
