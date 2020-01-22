set.seed(1)

n = 200
x = runif(n)

# one observation/realization of z (theory tells us it is approximately N(0,1))
z = sqrt(n)*(mean(x) - 1/2)/sqrt(1/12)

n_repl = 500
z = numeric()

# collecting a total of n_repl samples of size n from the _population_
# and computing the value of z corresponding to each sample
for (r in 1:n_repl){
  x = runif(n)
  z[r] = sqrt(n)*(mean(x) - 1/2)/sqrt(1/12)
}

hist(z, freq = FALSE)
curve(dnorm(x, mean=0, sd=1), add=TRUE, col = 'red')



# collecting ONE sample of size n from the population
x = runif(n)
# collecting a total of n_repl samples of size n from the _fixed sample, x_
# and computing the value of z corresponding to each sample
for (r in 1:n_repl){
  x_local = sample(x, size = n, replace = TRUE)
  z[r] = sqrt(n)*(mean(x_local) - mean(x))/sd(x)
}

hist(z, freq = FALSE)
curve(dnorm(x, mean=0, sd=1), add=TRUE, col = 'red')


# Repeating the above procedure, a total of n_repl2 times
n_repl2 = 500
keeptrack = numeric()
for (k in 1:n_repl2){
# collecting ONE sample of size n from the population
x = runif(n)
# collecting a total of n_repl samples of size n from the _fixed sample, x_
# and computing the value of z corresponding to each sample
for (r in 1:n_repl){
  x_local = sample(x, size = n, replace = TRUE)
  z[r] = sqrt(n)*(mean(x_local) - mean(x))/sd(x)
}
keeptrack[k] = sqrt(n)*(mean(x) - 1/2)/sqrt(1/12) > quantile(z, .75)
}

sum(keeptrack)
