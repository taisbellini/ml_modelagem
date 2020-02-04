library(glmnet)

n_repl = 1000
lambdas = seq(from = 0, to = 3, by = .5)

n=100
beta = c(1,1,0)

betahat = array(0, dim = c(4,length(lambdas),n_repl))


for (repl in 1:n_repl){
X = cbind(rexp(n,1), rexp(n,2), rexp(n,3))

Y = 1 + X%*%beta + rexp(n)-1

#foo = cv.glmnet(X, Y, alpha=1, nfold = 10)
#foo = glmnet(X, Y, alpha=1, lambda = foo$lambda.min)
foo = lm(Y~X)

## = adaLASSO = ##
tau=1
coef.init=coef(foo)[-1]
penalty.factor=abs(coef.init+1/sqrt(nrow(X)))^(-tau)
#foo2 = cv.glmnet(X,Y, alpha=1, penalty.factor=penalty.factor)
adalasso=glmnet(X,Y, alpha=1, penalty.factor=penalty.factor, lambda = lambdas)
betahat[,,repl] = as.matrix(coef(adalasso))
#plot(adalasso, xvar='norm', label=TRUE)
#coef(adalasso)
#coef(foo)

}

plot(log(lambdas), rev(rowMeans(betahat[2,,])), pch=16, cex = .5, col = rgb(0,0,0,1), ylim = range(betahat[2,,]), type='l')

par(mfrow = c(3,2))
for (k in 1:6){
  hist(betahat[2,8-k,], freq = FALSE, xlab = 'beta1', main=paste('lambda =', lambdas[k]), border=NA, col='lightgray', right=FALSE, breaks = seq(from=0,to=2, length=14))
}

par(mfrow = c(3,2))
for (k in 1:6){
  hist(betahat[4,8-k,], freq = FALSE, xlab = 'beta3', main=paste('lambda =', lambdas[k]), border=NA, col='lightgray', right = FALSE, breaks = seq(from=-2,to=3,length=14) )
}

