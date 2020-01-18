# simulando um modelo linear

# parâmetros
beta0 = 2
beta1 = .8

# sample size
n = 50

set.seed(7)
# termos de erro
epsilon = rnorm(n, sd = .7)

# covariável
X = runif(n, min = 0, max = 5)

Y = beta0 + beta1*X + epsilon

plot(0, 0, xlim = c(-1, 7), ylim = c(-1, 7), ann = FALSE, bty = "n", axes = FALSE, type = "n")

abline(a = beta0, b = beta1, lwd = .5)

abline(v = 0, h = 0, lwd = .5)

points(0, beta0, pch = 16, cex = .5)
text(0, beta0, labels = bquote(( list(0, beta[0]) )), pos = 2, cex = .7)
points(1, 0, pch = 16, cex = .5)
text(1, 0, labels = bquote(( list(1, 0) )), pos = 1, cex = .7)

polygon(c(0,1,1), c(beta0, beta0, beta0+beta1), lty = "dotted")

text(1, (2*beta0+beta1)/2, labels = bquote(beta[1]), pos = 4, cex = .7)

text(1/2, beta0, labels = bquote(1), pos = 1, cex = .7)



# GERANDO ALGUMAS OBSERVAÇÕES
k = 1
points(X[k], 0, pch = 16, cex = .5)
text(X[k], 0, labels = bquote(( list(X[.(k)], 0) )), pos = 1, cex = .7)

points(X[k], beta0 + beta1*X[k], pch = 16, cex = .5)

segments(X[k], beta0 + beta1*X[k], X[k], beta0+beta1*X[k] + epsilon[k], lwd = .5)

text(X[k], (Y[k] + beta0 +beta1*X[k])/2, labels = bquote(epsilon[.(k)]), pos = 4, cex = .7)

points(X[k], Y[k], pch = 21, col=rgb(0,.4,.8,.6), bg=rgb(0,.4,.8,.3), cex = .8)
text(X[k], Y[k], labels = bquote((list(X[.(k)], Y[.(k)]))), pos = 3, cex = .5)

points(0, Y[k], pch = 16, cex = .5)
text(0, Y[k], labels = bquote(( list(0, Y[.(k)]) )), pos = 2, cex = .7)

# demais observações. esse loop é apenas uma maneira iterativa de plotar os pontos e é equivalente a
# points(Y[-k] ~ X[-k])
for (k in 3:n){
points(X[k], 0, pch = 16, col = rgb(0,0,0, .2), cex = .5)
points(X[k], beta0 + beta1*X[k], pch = 16, col = rgb(0,0,0, .2), cex = .5)

segments(X[k], beta0 + beta1*X[k], X[k], beta0+beta1*X[k] + epsilon[k], lwd = .5, col=rgb(0,0,0,.5))

points(X[k], Y[k], pch = 21, col=rgb(0,.4,.8,.6), bg=rgb(0,.4,.8,.3), cex = .8)

points(0, Y[k], pch = 16, col = rgb(0,0,0, .2), cex = .5)
}


# ilustrando o procedimento de estimação: procurar uma reta que minimize a soma dos quadrados dos resíduos
plot(0, 0, xlim = c(-1, 7), ylim = c(-1, 7), ann = FALSE, bty = "n", axes = FALSE, type = "n")


abline(v = 0, h = 0, lwd = .5)

# scatterplot dos dados
points(X, Y, pch = 21, col=rgb(0,.4,.8,.6), bg=rgb(0,.4,.8,.3), cex = .8)

# uma possível reta
A = 3
B = .5
abline(a = A, b = B, col = "red", lwd = .5)

X = sort(X, index.return = TRUE)
Y = Y[X$ix]
X = X$x

# alguns erros quadráticos
for(k in c(10,20,30,40,50)){
polygon(c(X[k], X[k], X[k] + abs(Y[k] - A - B*X[k]), X[k] + abs(Y[k] - A - B*X[k])), c(Y[k], A + B*X[k], A + B*X[k], Y[k]), border = "white", col = rgb(.5,0,0, .5), lwd = .3)
}

# Reta estimada VS verdadeira
plot(0, 0, xlim = c(-1, 7), ylim = c(-1, 7), ann = FALSE, bty = "n", axes = FALSE, type = "n")

abline(v = 0, h = 0, lwd = .5)

points(X, Y, pch = 21, col=rgb(0,.4,.8,.6), bg=rgb(0,.4,.8,.3), cex = .8)

abline(lm(Y~X), col = "red", lwd = .5)

abline(a = beta0, b = beta1, lwd = .5)
