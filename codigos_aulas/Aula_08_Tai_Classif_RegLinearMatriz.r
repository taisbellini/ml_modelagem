# Hastie, Tibshirani and Friedman's
# example in section 2.3.1
library(mda)
library(plotly)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)


data(ESL.mixture)
dados = data.frame(y = ESL.mixture$y,
                  x1 = ESL.mixture$x[,1],
                  x2 = ESL.mixture$x[,2], 
                  gp = as.factor(ESL.mixture$y))

#---------------------------------------
# Representação dos dados em 2D
#---------------------------------------
twoClassColor <- brewer.pal(3,'Set1')[2:1]
names(twoClassColor) <- c('0','1')
ggplot(data = dados,aes(x = x1, y = x2)) + 
    geom_point(aes(color = gp), size = 3, alpha = .5) +
    scale_colour_manual(name = 'classes', values = twoClassColor) +
  theme_bw()

#---------------------------------------
# Representação dos dados em 3D
#---------------------------------------
plot_ly(dados, x = ~x1, y = ~x2, z = ~y,
        color = ~gp, colors = twoClassColor) %>%
    add_markers(size=1) %>%
    layout(scene = list(xaxis = list(title = 'x1'),
                        yaxis = list(title = 'x2'),
                        zaxis = list(title = 'y')))


#############################################
#
#   Regra de decisão vista em 2D
#
##############################################
# função para fazer os dois gráficos:
# dados devem conter: x1, x2, gp
# pred deve ser uma variável categórica
# Grid é a grade para a qual obtivemos "pred"
# cores = cores para as classes
PlotGrid <- function(pred, dados, Grid, cores) {
    surf <- (ggplot(data = dados, aes(x = x1, y = x2), color = gp) +
                 geom_tile(data = cbind(Grid, classes = pred), aes(fill = classes)) +
                 scale_fill_manual(name = 'classes', values = cores) +
                 ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
                 scale_colour_manual(name = 'classes', values = cores)) 
    pts <- (ggplot(data = dados, aes(x = x1, y = x2, color = gp)) +
                geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                             color = "red") +
                geom_point(size = 4, alpha = .5) + 
                ggtitle("Decision boundary") +
                theme(legend.text = element_text(size = 10)) +
                scale_colour_manual(name = 'classes', values = cores)) 
    grid.arrange(surf, pts, ncol = 2)
}

MQO = lm(y ~ x1 + x2, data = dados)
npt = 200   
# quanto maior npt mais suave fica a linha traçada
# no gráfico, mas fica mais demorado....
x1 <- seq(min(dados$x1), max(dados$x1), length.out=npt)    
x2 <- seq(min(dados$x2), max(dados$x2), length.out=npt)    
Grid = expand.grid(x1 = x1, x2 = x2,KEEP.OUT.ATTRS = FALSE) 
pred = predict(MQO, Grid)
pred = ifelse(pred < 0.5, "0", "1")
PlotGrid(pred, dados, Grid, twoClassColor)


#############################################
#
#   Regra de decisão vista em 3D
#
##############################################
# matrix com as previsões:
x1_grid <- seq(min(dados$x1), max(dados$x1), length.out=50)    
x2_grid <- seq(min(dados$x2), max(dados$x2), length.out=50)    
xyz = expand.grid(x1 = x1_grid, x2 = x2_grid,KEEP.OUT.ATTRS = FALSE) 
xyz$yhat = predict(MQO, xyz)
yhat = reshape2::acast(xyz, x2 ~ x1, value.var = "yhat")

# regra para classificação:
rule = matrix(0.5, 50,50)

plot_ly(dados, x = ~x1, y = ~x2, z = ~y,
        color = ~gp, colors = twoClassColor) %>%
    add_markers(size=1) %>%
    layout(scene = list(xaxis = list(title = 'x1'),
                        yaxis = list(title = 'x2'),
                        zaxis = list(title = 'y'))) %>%
    add_trace(z = yhat, x = x1_grid, y = x2_grid,
              type = "surface", opacity = 0.6) %>%
    add_trace(z = rule, x = x1_grid, y = x2_grid,
              type = "surface", colors='black',opacity = 0.6)


# verificando os acertos e erros
temp = data.frame(x1 = dados$x1, 
                  x2 = dados$x2,
                  y = dados$y, 
                  yhat = ifelse(MQO$fitted.values <0.5, 0, 1))
with(temp, sum(y == yhat))
nrow(temp)
w = with(temp, which(y != yhat))
temp$gp = temp$y
temp$gp[w] = 2
temp$y[w] = NA
temp$x1[w] = NA
temp$x2[w] = NA
colTemp = c(twoClassColor, "green")
temp = temp[-w,]

plot_ly(dados, x = ~x1, y = ~x2, z = ~y, 
        color = ~gp, colors = twoClassColor) %>%
  add_markers(size=1) %>%
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y'))) %>%
  add_trace(z = yhat, x = x1_grid, y = x2_grid,
            type = "surface", opacity = 0.6) %>%
  add_markers(data = temp, x = ~x1, y = ~x2, z = ~y)


#############################################
#
#   Regressão linear na matriz indicadora
#
##############################################

# quais sao os grupos que temos nos dados?
lvl = levels(dados$gp)

# matriz das vars dummy. No caso sao apenas 2: 0 e 1
Z = sapply(lvl, function(i) as.integer(dados$gp == i))
X = cbind(intercept = rep(1, nrow(dados)), x1 = dados$x1, x2 = dados$x2)
B = solve(t(X)%*%X)%*%t(X)%*%Z
B
# são so mesmos coeficients obtidos usando lm do R.
# removendo o 1 pois a constante já foi incluida em X:
lm(Z ~ -1 + X)
# poderia chamar diretamente sem criar X:
lm(Z ~ x1 + x2, data = dados) 

# calculando Zhat
Zhat01 = X%*%B
# verificando que as linhas de Zhat01 somam 1 
apply(Zhat01, 1, sum)
# classificando:
yhat = ifelse(Zhat01[,1] > Zhat01[,2], 0, 1)

# matrix de confusão  (confusion matrix)
table(dados$y, yhat)

x1_grid <- seq(min(dados$x1), max(dados$x1), length.out=50)    
x2_grid <- seq(min(dados$x2), max(dados$x2), length.out=50)    
Grid = expand.grid(x1 = x1_grid, x2 = x2_grid,KEEP.OUT.ATTRS = FALSE) 
Grid$Zhat0 = cbind(1, as.matrix(Grid[,1:2]))%*%B[,1]
Grid$Zhat1 = cbind(1, as.matrix(Grid[,1:2]))%*%B[,2]
Zhat0 = reshape2::acast(Grid, x2 ~ x1, value.var = "Zhat0")
Zhat1 = reshape2::acast(Grid, x2 ~ x1, value.var = "Zhat1")
rule = matrix(0.5, 50,50)

plot_ly(dados, x = ~x1, y = ~x2, z = ~y,
        color = ~gp, colors = twoClassColor) %>%
  add_markers(size=1) %>%
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y'))) %>%
  add_trace(z = Zhat0, x = x1_grid, y = x2_grid,
            type = "surface", opacity = 0.6) %>%
  add_trace(z = Zhat1, x = x1_grid, y = x2_grid,
            type = "surface", colors='black',opacity = 0.6)  %>%
  add_trace(z = rule, x = x1_grid, y = x2_grid,
            type = "surface", colors='black',opacity = 0.6)


################################################
# Note que, como temos apenas duas categorias
# que são Y = 0 e Y = 1, poderíamos usar 
# lm no R para modelar E(I(Y = 1))
# Como os beta0's somam 1 e os betaj's somam 0
# basta obter as estimativas de um grupo.
# Isso é o que fizemos no exemplo no início 
# desse documento
################################################
MQO = lm(y ~ x1+x2, data = dados)
fhat = fitted(MQO)
# fhat = P(Y = 1 | X) => 1 - fhat = P(Y = 0 |X)
# note que isso é equivalente a verificar se fhat > 1/2
yhat.MQO = ifelse(fhat > 1 - fhat, 1, 0)

# comparando
table(yhat, yhat.MQO)


#############################################
#
#   Um exemplo com 3 grupos:
#   Aqui NÃO é equivalente a modelar um
#   deles e perguntar se xB > 0.5
#
##############################################
library(mvtnorm)
n = 100


# gerando dados
set.seed(1234)
x = NULL
Y = NULL
for(i in 1:3){
  tp = rmvnorm(n, mean = rep(4.5*i, 2), sigma = diag(2))
  x = rbind(x, tp)
  # A codificação abaixo não muda o resultado da classificação
  Y = c(Y, rep(i-2, n))
}

dados = data.frame(x1 = x[,1], x2 = x[,2], y = Y, gp = as.factor(Y+2))

#---------------------------------------
# Representação dos dados em 2D
#---------------------------------------
cores3 <- brewer.pal(3,'Set1')[3:1]
names(cores3) <- c('1','2', "3")
ggplot(data = dados,aes(x = x1, y = x2)) + 
  geom_point(aes(color = gp), size = 3, alpha = .5) +
  scale_colour_manual(name = 'classes', values = cores3) +
  theme_bw()

#---------------------------------------
# Representação dos dados em 3D
#---------------------------------------
plot_ly(dados, x = ~x1, y = ~x2, z = ~y,
        color = ~gp, colors = cores3) %>%
  add_markers(size=1) %>%
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y')))

#   Regressão linear na matriz indicadora
lvl = levels(dados$gp)
Z = sapply(lvl, function(i) as.integer(dados$gp == i))
X = cbind(intercept = rep(1, nrow(dados)), x1 = dados$x1, x2 = dados$x2)
B = solve(t(X)%*%X)%*%t(X)%*%Z
B

dados$Z1 = Z[,1]
dados$Z2 = Z[,2]
dados$Z3 = Z[,3]

x1_grid <- seq(min(dados$x1), max(dados$x1), length.out=50)    
x2_grid <- seq(min(dados$x2), max(dados$x2), length.out=50)    
Grid = expand.grid(x1 = x1_grid, x2 = x2_grid,KEEP.OUT.ATTRS = FALSE) 
Grid$Zhat1 = cbind(1, as.matrix(Grid[,1:2]))%*%B[,1]
Grid$Zhat2 = cbind(1, as.matrix(Grid[,1:2]))%*%B[,2]
Grid$Zhat3 = cbind(1, as.matrix(Grid[,1:2]))%*%B[,3]
Zhat1 = reshape2::acast(Grid, x2 ~ x1, value.var = "Zhat1")
Zhat2 = reshape2::acast(Grid, x2 ~ x1, value.var = "Zhat2")
Zhat3 = reshape2::acast(Grid, x2 ~ x1, value.var = "Zhat3")

plot_ly(dados, x = ~x1, y = ~x2, z = ~Z1,
        color = ~gp, colors = cores3) %>%
  add_markers(size=1) %>%
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y'))) %>%
  add_trace(z = Zhat1, x = x1_grid, y = x2_grid, opacity = .6, 
            type = "surface", colorscale = list(c(0, 1), c("white", cores3[1])))


plot_ly(dados, x = ~x1, y = ~x2, z = ~Z2,
        color = ~gp, colors = cores3) %>%
  add_markers(size=1) %>%
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y'))) %>%
  add_trace(z = Zhat2, x = x1_grid, y = x2_grid, opacity = .6, 
            type = "surface", colorscale = list(c(0, 1), c("white", cores3[2])))



plot_ly(dados, x = ~x1, y = ~x2, z = ~Z3,
        color = ~gp, colors = cores3) %>%
  add_markers(size=1) %>%
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y'))) %>%
  add_trace(z = Zhat3, x = x1_grid, y = x2_grid,
            type = "surface", opacity = 0.6, colorscale = list(c(0, 1), c("white", cores3[3])))


plot_ly(dados, x = ~x1, y = ~x2, z = ~y,
        color = ~gp, colors = cores3) %>%
  add_markers(size=1) %>%
  layout(scene = list(xaxis = list(title = 'x1'),
                      yaxis = list(title = 'x2'),
                      zaxis = list(title = 'y'))) %>%
  add_trace(z = Zhat1, x = x1_grid, y = x2_grid, 
            type = "surface", opacity = 0.6, colorscale = list(c(0,1), c("tan", cores3[1]))) %>%
  add_trace(z = Zhat2, x = x1_grid, y = x2_grid,
            type = "surface", opacity = 0.6, colorscale = list(c(0, 1), c("tan", cores3[2])))  %>%
  add_trace(z = Zhat3, x = x1_grid, y = x2_grid,
            type = "surface", opacity = 0.6, colorscale =  list(c(0, 1), c("tan", cores3[3])))%>% hide_colorbar()


# A região que o ponto cair, ele vai classificar onde o plano estava mais alto naquela região.


# matrix com as previsões:
npt = 200   
x1 <- seq(min(dados$x1), max(dados$x1), length.out=npt)    
x2 <- seq(min(dados$x2), max(dados$x2), length.out=npt)    
Grid = expand.grid(x1 = x1, x2 = x2,KEEP.OUT.ATTRS = FALSE) 
pred = numeric(nrow(Grid))
for(i in 1:nrow(Grid)){
   xb = t(c(1, Grid$x1[i], Grid$x2[i]))%*%B
   w = which(xb == max(xb))
   pred[i] = w
}   
pred = as.character(pred)

PlotGrid(pred, dados, Grid, cores3)

# ficou bem ruim pra azul, pois a maioria cai em azul ou verde


# visualizando as previsões:
predin = numeric(nrow(dados))
for(i in 1:nrow(dados)){
  xb = t(c(1, dados$x1[i], dados$x2[i]))%*%B
  w = which(xb == max(xb))
  predin[i] = w
}   
predin = as.character(predin)
dados$predin = as.character(predin)
ggplot(data = dados,aes(x = x1, y = x2)) + 
  geom_point(aes(color = predin), size = 3, alpha = .5) +
  scale_colour_manual(name = 'classes', values = cores3) +
  theme_bw()

# tem que fazer pras outras cores ... (erro do software.. rodar de novo)


## OUTRO METODO: ANALISE DISCRIMINANTE
# LDA
library(plyr)
Medias = ddply(dados, ~y, summarise, mx1 = mean(x1), mx2 = mean(x2))
Medias
m = t(Medias[,-1])
colnames(m) = unique(dados$y)

S = 0
for(i in c(-1,0,1)){
  w = with(dados, which(y == i))
  S2 = with(dados[w,], cov(cbind(x1,x2)))
  S = S + (length(w)-1)*S2 
}
S = 1/(nrow(dados) - 3)*S

B = matrix(ncol = 3, nrow = 3)
D = matrix(ncol = 3, nrow = nrow(dados))
for(i in 1:3){
  B[1,i] = -1/2*t(m[,i])%*%solve(S)%*%m[,i] 
  B[2:3,i] = solve(S)%*%m[,i]
}
X = cbind(1, dados$x1, dados$x2)
D = X%*%B
Yhat = apply(D, 1, function(x) c(-1,0,1)[which(x == max(x))])
pred = as.character(Yhat)
table(dados$y, Yhat)

npt = 200   
x1 <- seq(min(dados$x1), max(dados$x1), length.out=npt)    
x2 <- seq(min(dados$x2), max(dados$x2), length.out=npt)    
Grid = expand.grid(x1 = x1, x2 = x2,KEEP.OUT.ATTRS = FALSE) 
Dout = cbind(1, as.matrix(Grid))%*%B
pred = apply(Dout, 1, function(x) c(-1,0,1)[which(x == max(x))])
pred = as.character(pred)

dados$gp = as.factor(dados$y)
cores3 <- brewer.pal(3,'Set1')[3:1]
names(cores3) <- c('-1','0', "1")
PlotGrid(pred, dados, Grid, cores3)

# para este problema especifico, o resultado ficou bem melhor com este metodo.
# tem funcao LDA pronta no R ->vai dar bem proximo
