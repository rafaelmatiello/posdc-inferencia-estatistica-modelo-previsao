# funções estatísticas para dados normais
# dnorm -> densidade da distribuição nomal (P[X=x]), ponto exato
# pnorm -> função distribuição de probabilidade (P[X<x]), intervalo
# qnorm -> inserir um probabilidade (0,1), retornar o valor que a geraria. Com qual valor x vai gerar um X
# rnorm -> retorna um vetor de dados normais

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

library(fBasics)

summary(N10)
summary(N50)
summary(N100)
summary(N500)

basicStats(N10)
basicStats(N50)
basicStats(N100)
basicStats(N500)
basicStats(N50000)

# LCL -> 95% media inverior
# UCL -> 95% media superior
# Se mean -> erro padrão 


install.packages("normtest")
library(normtest)

# Teste de normalidade
# h0 -> a distribuição é normal
# h1 -> a distribuição não é normal

# Função do teste k-s - ks.test(varX, "pnorm")
# Função do teste Shapiro-Wilk shapiro.test(varX)
# Função do teste Jarque-Bera - jb.norm.test(varX)

ks.test(N10, "pnorm")
ks.test(N50, "pnorm")
ks.test(N100, "pnorm")
ks.test(N500, "pnorm")


shapiro.test(N10)
shapiro.test(N50)
shapiro.test(N100)
shapiro.test(N500)
# Shapiro-Wilk normality test
# 
# data:  N500
# W = 0.99774, p-value = 0.7446

jb.norm.test(N10)
jb.norm.test(N50)
jb.norm.test(N100)
jb.norm.test(N500)
jb.norm.test(N50000)
# Jarque-Bera test for normality
# 
# data:  N50000
# JB = 0.14462, p-value = 0.934


