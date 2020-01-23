#####
# Vamos realizar uma simulação para responder à pergunta:
# Será que a média amostral e a mediana amostral são bons 
# estimadores da média populacional?
####

# verificando num exemplo
x=rnorm(100)
mean(x)
median(x)

########### Simulação de Monte Carlo
#
## DGP: geramos amostras de tamanho n de uma distribuição
# N(mu,sigma²)

n = 100
mu = 5
sigma2 = 2
rep = 1000


#### Analizando as propriedades de estimação pontual 
####        da média e mediana

res = matrix(ncol=2,nrow=rep)
colnames(res)=c("media","mediana")

# Loop
for(i in 1:rep){
  # gerando a amostra de tamanho n da normal escolhida
    x = rnorm(n,mean=mu, sd=sqrt(sigma2)) 
    res[i,1] = mean(x) # calculando a média
    res[i,2] = median(x) # calculando a mediana
  }
# Analisando os resultados

means = colMeans(res)
means 

# Variâncias amostrais no contexto normal:
# média: sigma2/n
# mediana: pi*sigma2/(2*n)

vars = apply(res,2,var) 
vars
# Conclusão: tanto a média quanto a mediana estimam bem
# a média populacional NESTE contexto

# Será que a média e mediana amostral seguem uma distribuição
# normal quando propriamente normalizadas?

# Normalizando os resultados:

resN = cbind((res[,1]-means[1])/sqrt(vars[1]),(res[,2]-means[2])/sqrt(vars[2]))

# Avaliando a normalidade dos estimadores: 

par(mfrow=c(2,1))
hist(resN[,1], main = "Média")
hist(resN[,2], main = "Mediana")

hist(res[,1], main = "Média")
hist(res[,2], main = "Mediana")

#  Avaliando a consistência dos estimadores neste
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
x11()
plot.ts(res2[,1], ylim=c(min(res2),max(res2)))
lines(res2[,2], col="red")
abline(h=mu, col="blue")
legend("bottomright", legend=c("média", "mediana", "verdadeiro"),
       lty=1, box.col = "white", col=c(1,"red","blue"))
box()

#############################
# PARTE 2
# Será que estas conclusões valem em outros contextos?

# verificando num exemplo
x=rexp(100, rate=2)
mean(x)
median(x)

########### Simulação de Monte Carlo
#
#  DGP: amostras de tamanho n de uma exponencial com parâmetro par
#  Propriedades: esperança = 1/lambda, variancia = 1/lambda^2
n = 100
lambda = 2
rep = 1000


#### Analizando as propriedades de estimação pontual 
####        da média e mediana

res = matrix(ncol=2,nrow=rep)
colnames(res)=c("media","mediana")

# Loop
for(i in 1:rep){
  # gerando a amostra de tamanho n da normal escolhida
  x = rexp(n, rate = lambda) 
  res[i,1] = mean(x) # calculando a média
  res[i,2] = median(x) # calculando a mediana
}
# Analisando os resultados

means = colMeans(res)
means 

# Variâncias amostrais no contexto normal:
# média: sigma2/n
# mediana: pi*sigma2/(2*n)

vars = apply(res,2,var) 
vars

# Conclusão: a mediana NÃO estimam bem
# a média populacional NESTE contexto

# Será que a média e mediana amostral seguem uma distribuição
# normal quando propriamente normalizadas?

# Normalizando os resultados:

resN = cbind((res[,1]-means[1])/sqrt(vars[1]),(res[,2]-means[2])/sqrt(vars[2]))

# Avaliando a normalidade dos estimadores: 

par(mfrow=c(2,1))
hist(res[,1], main = "Média", breaks=15)
hist(res[,2], main = "Mediana", breaks=15)

hist(resN[,1], main = "Média")
hist(resN[,2], main = "Mediana")

# Observe porém que a mediana de uma exponencial par é
# log(2)/par
# Ou seja, para estimar a média populacional a mediana amostral nem sempre é boa,
# mas para estimar a mediana populacional, parece excelente!


#  Avaliando a consistência dos estimadores neste
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



