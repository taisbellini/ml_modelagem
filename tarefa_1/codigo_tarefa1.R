# Importar dados #
data = read.csv2("205650.txt", sep=" ", dec = ".")

# Funcao para dividir dados aleatoriamente em treinamento e teste #
split_data = function(data, train_perc){
  train_size <- floor(train_perc * nrow(data))
  train_index <- sample(seq_len(nrow(data)), size = train_size)
  train <- data[train_index, ]
  test <- data[-train_index, ]
  return(list("train" = train,
              "test" = test))
}

# Funcao para encontrar params que minimizam perda L1 #
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
  
  return(opt_b$par)
}

# Objetos para salvar os resultados das 100 repeticoes #

bhat_vec = rep(0,100)
bhat_params = data.frame(b0_reg=bhat_vec,
                         b1_reg=bhat_vec,
                         b0_l1=bhat_vec,
                         b1_l1=bhat_vec)

MAE_vec = rep(0, 100)
MAE_table = data.frame(MAEin_reg=MAE_vec,
                       MAEout_reg=MAE_vec,
                       MAEin_l1=MAE_vec,
                       MAEout_l1=MAE_vec)

# Loop para executar o experimento 100 vezes #

set.seed(205650)
for (i in seq(1:100)){
  splitted_data = split_data(data, 0.85)
  train <- splitted_data$train
  test <- splitted_data$test
  
  ## Regressao simples (MQO) ##
  
  # Encontrar params b0 e b1 #
  reg = lm(Y ~ X, data=train)
  bhat_params$b0_reg[i] = reg$coefficients[1]
  bhat_params$b1_reg[i] = reg$coefficients[2]
  
  # Calcular MAEout e MAEin #
  Ypred_reg_test = predict(reg, test)
  Ypred_reg_train = predict(reg, train)
  
  abs_error_test_reg = abs(test$Y - Ypred_reg_test)
  MAEout_reg = sum(abs_error_test_reg)/15
  MAE_table$MAEout_reg[i] = MAEout_reg
  
  abs_error_train_reg = abs(train$Y - Ypred_reg_train)
  MAEin_reg = sum(abs_error_train_reg)/85
  MAE_table$MAEin_reg[i] = MAEin_reg
  
  ## Minimizar perda L1 ##
  
  # Encontrar b0 e b1 otimos #
  opt_l1_loss_params = opt_l1_loss(train)
  Ypred_test_l1 = opt_l1_loss_params[1] + opt_l1_loss_params[2]*test$X
  Ypred_train_l1 = opt_l1_loss_params[1] + opt_l1_loss_params[2]*train$X
  bhat_params$b0_l1[i] = opt_l1_loss_params[1]
  bhat_params$b1_l1[i] = opt_l1_loss_params[2]
  
  # Calcular MAEin e MAEout #
  abs_error_test_l1 = abs(test$Y - Ypred_test_l1)
  MAEout_l1 = sum(abs_error_test_l1)/15
  MAE_table$MAEout_l1[i] = MAEout_l1
  
  abs_error_train_l1 = abs(train$Y - Ypred_train_l1)
  MAEin_l1 = sum(abs_error_train_l1)/85
  MAE_table$MAEin_l1[i] = MAEin_l1
  
}

# Assets Relatorio #

# Graficos #

install.packages("ggplot2")
library(ggplot2)

# b0 reg
boxplot(bhat_params$b0_reg)
summary(bhat_params$b0_reg)

#b1 reg
boxplot(bhat_params$b1_reg)
summary(bhat_params$b1_reg)

# b0 l1
boxplot(bhat_params$b0_reg)
summary(bhat_params$b0_reg)

#b1 l1
boxplot(bhat_params$b1_reg)
summary(bhat_params$b1_reg)

# MAEin reg
boxplot(MAE_table$MAEin_reg)
summary(MAE_table$MAEin_reg)

# MAEin L1
boxplot(MAE_table$MAEin_l1)
summary(MAE_table$MAEin_l1)

# MAEout reg
boxplot(MAE_table$MAEout_reg)
summary(MAE_table$MAEout_reg)

# MAEout L1
boxplot(MAE_table$MAEout_l1)
summary(MAE_table$MAEout_l1)

# Grafico de dispersao # 
plot(data$X, data$Y)

# Tabela medias #

avg_results = data.frame(matrix(ncol = 2, nrow = 4))
col_names = c("Regressao", "Opt L1")
row_names = c("b0 medio", "b1 medio", "MAEin medio", "MAEout medio")
colnames(avg_results) = col_names
rownames(avg_results) = row_names

avg_results$Regressao[1] = mean(bhat_params$b0_reg)
avg_results$Regressao[2] = mean(bhat_params$b1_reg)
avg_results$Regressao[3] = mean(MAE_table$MAEin_reg)
avg_results$Regressao[4] = mean(MAE_table$MAEout_reg)

avg_results$`Opt L1`[1] = mean(bhat_params$b0_l1)
avg_results$`Opt L1`[2] = mean(bhat_params$b1_l1)
avg_results$`Opt L1`[3] = mean(MAE_table$MAEin_l1)
avg_results$`Opt L1`[4] = mean(MAE_table$MAEout_l1)
View(avg_results)

