set.seed(123)
n = 10
x1 = rnorm(n)

x2 = x1 + rnorm(n, sd=.5)

x1 = x1-mean(x1)
x2 = x2-mean(x2)

mydat = data.frame(x1 = x1, x2 = x2)

# calcula os componentes principais usando a funcao prcomp
foo = prcomp(mydat, center = TRUE,scale. = FALSE,retx=TRUE)

# matriz de covariância de c(x1,x2)
Sigma = cov(cbind(x1,x2))

# calcula os componentes principais via decomposicao espectral de Sigma
pca = eigen(Sigma)

# autovetores
v = pca$vectors

# ajusta o sinal dos autovetores
v[,1] = sign(as.numeric(crossprod(v[,1],foo$rotation[,1])))*v[,1]
v[,2] = sign(as.numeric(crossprod(v[,2],foo$rotation[,2])))*v[,2]

# os dois metodos sao equivalentes:
abs(v - foo$rotation)




# scatterplot dos dados + componentes principais (na base canonica)
plot(x1,x2, pch=16, xlim=c(-3,3), ylim=c(-3,3))
arrows(x0=0,y0=0, x1=v[1,1], y1=v[2,1], lwd=2, col=rgb(0,.4,.7), length=.06)
arrows(x0=0,y0=0, x1=v[1,2], y1=v[2,2], lwd=2, col=rgb(0,.4,.7),length=.06)
e = cbind(c(1,0),c(0,1))
arrows(x0=0,y0=0, x1=e[1,1], y1=e[2,1], lwd=2, col=rgb(.7,.4,0), length=.06)
arrows(x0=0,y0=0, x1=e[1,2], y1=e[2,2], lwd=2, col=rgb(.7,.4,0),length=.06)



# mudanca de base
X = cbind(x1,x2)
par(mar=c(2,2,1,1))
plot(X%*%v,pch=16, xlab='PC1', ylab='PC2', xlim=c(-3,3), ylim=c(-1,1))
arrows(x0=0,y0=0, x1=e[1,1], y1=e[2,1], lwd=2, col=rgb(0,.4,.7), length=.06)
arrows(x0=0,y0=0, x1=e[1,2], y1=e[2,2], lwd=2, col=rgb(0,.4,.7),length=.06)
E = e%*%v
arrows(x0=0,y0=0, x1=E[1,1], y1=E[2,1], lwd=2, col=rgb(.7,.4,0), length=.06)
arrows(x0=0,y0=0, x1=E[1,2], y1=E[2,2], lwd=2, col=rgb(.7,.4,0),length=.06)

plot(X%*%v,pch=16, xlab='PC1', ylab='PC2')
library(ggbiplot)
ggbiplot(foo)+theme_minimal()
