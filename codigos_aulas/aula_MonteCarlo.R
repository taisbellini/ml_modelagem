#####
# Vamos realizar uma simulacao para responder a pergunta:
# Sera que a media amostral e a mediana amostral sao bons 
# estimadores da media populacional?
####

# verificando num exemplo
x=rnorm(100)
mean(x)
median(x)
hist(x)

#---- Simulacao de Monte Carlo ----
#
## DGP: geramos amostras de tamanho n de uma distribuicao
## DGP = Data Generator Process
# N(mu,sigma2)

n = 100
mu = 5
sigma2 = 2
rep = 1000


#### Analisando as propriedades de estimacao pontual 
####        da media e mediana

res = matrix(ncol=2,nrow=rep)
colnames(res)=c("media","mediana")

# Loop
for(i in 1:rep){
  # gerando a amostra de tamanho n da normal escolhida
    x = rnorm(n,mean=mu, sd=sqrt(sigma2)) 
    res[i,1] = mean(x) # calculando a media
    res[i,2] = median(x) # calculando a mediana
  }
# Analisando os resultados

means = colMeans(res)
means 

# Parece estar funcionando bem. Mas sera que foi coincidencia?

# Variancias amostrais no contexto normal:
# media: sigma2/n
# mediana: pi*sigma2/(2*n)

# o esperado de variancia: 
media = sigma2/n
mediana = pi*sigma2/(2*n)

# calcular a variancia para os dados simulados 
# apply: 1- linha 2- coluna
# ta aplicando a funcao var (que calcula a variancia) nas colunas da matriz res
vars = apply(res,2,var) 
vars
# Conclusao: tanto a media quanto a mediana estimam bem
# a media populacional NESTE contexto

# Sera que a media e mediana amostral seguem uma distribuicao
# normal quando propriamente normalizadas?

# Normalizando os resultados:

resN = cbind((res[,1]-means[1])/sqrt(vars[1]),(res[,2]-means[2])/sqrt(vars[2]))

# Avaliando a normalidade dos estimadores: 

par(mfrow=c(2,1))
hist(resN[,1], main = "Media", xlim=c(4,6))
hist(resN[,2], main = "Mediana", xlim=c(4,6))

hist(res[,1], main = "Media")
hist(res[,2], main = "Mediana")

#  Avaliando a consistencia dos estimadores neste
#  contexto 

ns = 10:200
res2 = matrix(ncol=2, nrow=length(ns))
for(s in 1:length(ns)){
  temp=matrix(ncol=2, nrow=rep)
  for(i in 1:rep){
    x=rnorm(ns[s],mean=mu, sd= sqrt(sigma2))
    temp[i,1]=mean(x)
    temp[i,2]=median(x)
  }
  res2[s,] = colMeans(temp)
}

#dev.off()
install.packages("XQuartz")
x11()
plot.ts(ns, res2[,1], type="l", ylim=c(min(res2),max(res2)))
lines(res2[,2], col="red")
abline(h=mu, col="blue")
legend("bottomright", legend=c("media", "mediana", "verdadeiro"),
       lty=1, box.col = "white", col=c(1,"red","blue"))
box()

#############################
# PARTE 2
# Sera que estas conclusoes valem em outros contextos?

# verificando num exemplo
x=rexp(100, rate=2)
mean(x)
median(x)

########### Simulacao de Monte Carlo
#
#  DGP: amostras de tamanho n de uma exponencial com parametro par
#  Propriedades: esperan?a = 1/lambda, variancia = 1/lambda^2
n = 100
lambda = 2
rep = 1000


#### Analizando as propriedades de estimacao pontual 
####        da media e mediana

res = matrix(ncol=2,nrow=rep)
colnames(res)=c("media","mediana")

# Loop
for(i in 1:rep){
  # gerando a amostra de tamanho n da normal escolhida
  x = rexp(n, rate = lambda) 
  res[i,1] = mean(x) # calculando a media
  res[i,2] = median(x) # calculando a mediana
}
# Analisando os resultados

means = colMeans(res)
means 

# Variancias amostrais no contexto normal:
# media: 1/lambda
# mediana: log(2)/lambda

vars = apply(res,2,var) 
vars

# Conclusao: a mediana NAO estimam bem
# a media populacional NESTE contexto

# Sera que a media e mediana amostral seguem uma distribuicao
# normal quando propriamente normalizadas?

# Normalizando os resultados:

resN = cbind((res[,1]-means[1])/sqrt(vars[1]),(res[,2]-means[2])/sqrt(vars[2]))

# Avaliando a normalidade dos estimadores: 

par(mfrow=c(2,1))
hist(res[,1], main = "Media", breaks=15)
hist(res[,2], main = "Mediana", breaks=15)

hist(resN[,1], main = "Media")
hist(resN[,2], main = "Mediana")

# Observe porem que a mediana de uma exponencial par eh
# log(2)/par
# Ou seja, para estimar a media populacional a mediana amostral nem sempre eh boa,
# mas para estimar a mediana populacional, parece excelente!


#  Avaliando a consistencia dos estimadores neste
#  contexto 

ns = 10:200
res3 = matrix(ncol=2, nrow=length(ns))
for(s in 1:length(ns)){
  temp=matrix(ncol=2, nrow=rep)
  for(i in 1:rep){
    x=rexp(ns[s], rate=lambda)
    temp[i,1]=mean(x)
    temp[i,2]=median(x)
  }
  res3[s,] = colMeans(temp)
}

# dev.off()
x11()

plot.ts(res3[,1], ylim=c(min(res3),max(res3)))
lines(res3[,2], col="red")
abline(h=1/lambda, col="blue")
abline(h=log(2)/lambda, col="green")



