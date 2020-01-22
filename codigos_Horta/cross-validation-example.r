# an example of cross-validation to select the degree of a poinomial in linear regression


# simulating the regression model
set.seed(1)
n = 100
x = runif(n)
beta0 = 1
beta1 = -1
beta2 = .01
beta3 = 0

y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + rnorm(n, sd=1/2)
plot(y ~ x, pch=21, bg=rgb(0,0,1,.5))
lines(sort(x), beta0 + beta1*sort(x) + beta2*sort(x)^2)

# suppose we do not know the true model and that we want to select one among the following
# y = beta0 + epsilon (M0)
# y = beta0 + beta1*x + epsilon (M1)
# y = beta0 + beta1*x + beta2*x^2 + epsilon (M2)
# y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + epsilon (M3)
# of course, there are many possibilities for selecting, for example based on AIC or on hypothesis testing, etc.
# 
# in leave-one-out cross-validation, we estimate each one of the models (M1)-(M3) above,
# repeating the estimation procedure n times for each model. Each time we estimate, we remove one of the sample observations and calculate the beta-hat's using a sample of
# size n-1. The remaining observation is used to compute the squared prediction error. 
# Summing the n squared prediction errors for each model gives us an estimate of it's 
# out-of-sample theoretical performance. We can then compare these to select the model
# with better out-of-sample performance.


# M0
M0 = numeric()
for (i in 1:n){
  response = y[-i] # excludes ith observation
  yhat = mean(response)
  M0[i] = (y[i] - yhat)^2
}

# M1
M1 = numeric()
for (i in 1:n){
  response = y[-i]
  covariate = x[-i]
  betahats = lm(response ~ covariate)$coef
  yhat = betahats[1] + betahats[2]*x[i]
  M1[i] = (y[i] - yhat)^2
}

# M2
M2 = numeric()
for (i in 1:n){
  response = y[-i]
  covariate = x[-i]
  covariate.squared = covariate^2
  betahats = lm(response ~ covariate + covariate.squared)$coef
  yhat = betahats[1] + betahats[2]*x[i] + betahats[3]*x[i]^2
  M2[i] = (y[i] - yhat)^2
}

# M3
M3 = numeric()
for (i in 1:n){
  response = y[-i]
  covariate = x[-i]
  covariate.squared = covariate^2
  covariate.cubed = covariate^3
  betahats = lm(response ~ covariate + covariate.squared + covariate.cubed)$coef
  yhat = betahats[1] + betahats[2]*x[i] + betahats[3]*x[i]^2 + betahats[4]*x[i]^3
  M3[i] = (y[i] - yhat)^2
}

# comparing the three models:
sum(M0) # = 27.1637
sum(M1) # = 22.61721
sum(M2) # = 23.07161
sum(M3) # = 23.52633
# so, for this data set we would select the model with an intercept plus a linear term
abline(lm(y~x), col='red')
