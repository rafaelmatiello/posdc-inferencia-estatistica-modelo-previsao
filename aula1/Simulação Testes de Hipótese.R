# ------------------------------------------
# Testes de Normalidade
# ------------------------------------------

# Gerar n�meros aleat�rios
# Fun��es estat�sticas para dados normais
  #dnorm -> densidade da distribui��o normal (P[X=x])
  #pnorm -> fun��o distribui��o de probabilidade (P[X<x])
  #qnorm -> inserir uma probabilidade (0,1) retoma o valor que a geraria.
  #rnorm -> retoma um vetor de dados normais

N10 <- rnorm(10, mean=30, sd=5)
N50 <- rnorm(50, mean=30, sd=5)
N100 <- rnorm(100, mean=30, sd=5)
N500 <- rnorm(500, mean=30, sd=5)
N50000 <- rnorm(50000, mean=30, sd=5)

hist(N10)
hist(N50)
hist(N100)
hist(N500)
hist(N50000)

# Estat�sticas Descritivas
summary(N10)
summary(N50)
summary(N100)
summary(N500)
summary(N50000)

library(fBasics)
basicStats(N10)
basicStats(N50)
basicStats(N100)
basicStats(N500)
basicStats(N50000)

# Testes de Normalidade
  #H0 - A distribui��o � normal
  #H1 - A distribui��o n�o � normal
# Usar o Pacote "normtest"
library(normtest) #install.packages("normtest")

# Fun��o do teste K-s - ks.test(varX, "pnorm")
# Fun��o do Teste de Shapiro-Wilk - shapiro.test( VarX)
# Fun��o do Teste de Jarque-Bera - jb.norm.test( varX )

ks.test(N10, "pnorm")
ks.test(N50, "pnorm")
ks.test(N100, "pnorm")
ks.test(N500, "pnorm")

shapiro.test(N10)
shapiro.test(N50)
shapiro.test(N100)
shapiro.test(N500)
shapiro.test(N50000)

jb.norm.test(N10)
jb.norm.test(N50)
jb.norm.test(N100)
jb.norm.test(N500)
jb.norm.test(N50000)

