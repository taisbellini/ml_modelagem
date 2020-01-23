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
# twoClassColor = cores para as classes
PlotGrid <- function(pred, dados, Grid, twoClassColor) {
    surf <- (ggplot(data = dados, aes(x = x1, y = x2), color = gp) +
                 geom_tile(data = cbind(Grid, classes = pred), aes(fill = classes)) +
                 scale_fill_manual(name = 'classes', values = twoClassColor) +
                 ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
                 scale_colour_manual(name = 'classes', values = twoClassColor)) 
    pts <- (ggplot(data = dados, aes(x = x1, y = x2, color = gp)) +
                geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                             color = "red", breaks = c(1.5)) +
                geom_point(size = 4, alpha = .5) + 
                ggtitle("Decision boundary") +
                theme(legend.text = element_text(size = 10)) +
                scale_colour_manual(name = 'classes', values = twoClassColor)) 
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
lvl = levels(dados$gp)
Z = sapply(lvl, function(i) as.integer(dados$gp == i))
X = cbind(inter = rep(1, nrow(dados)), x1 = dados$x1, x2 = dados$x2)
B = solve(t(X)%*%X)%*%t(X)%*%Z
B
# calculando Zhat
Zhat01 = X%*%B
# as linhas de Zhat01 somam 1 
apply(Zhat01, 1, sum)

# usando lm do R:
lm(Z ~ -1 + X)

# lm no R para modelar E(I(Y = 1))
lm(y ~ x1+x2, data = dados)$coefficients




