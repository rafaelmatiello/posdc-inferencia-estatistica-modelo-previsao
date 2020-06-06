# funções estatísticas para dados normais
# dnorm -> densidade da distribuição nomal (P[X=x]), ponto exato
# pnorm -> função distribuição de probabilidade (P[X<x]), intervalo
# qnorm -> inserir um probabilidade (0,1), retornar o valor que a geraria. Com qual valor x vai gerar um X
# rnorm -> retorna um vetor de dados normais

N10 <- rnorm(10, mean=30, sd=5)
N50 <- rnorm(50, mean=30, sd=5)
N100 <- rnorm(100, mean=30, sd=5)
N500 <- rnorm(500, mean=30, sd=5)


hist(N10)
hist(N50)
hist(N100)
hist(N500)

library(fBasics)

summary(N10)
summary(N50)
summary(N100)
summary(N500)

basicStats(N10)
basicStats(N50)
basicStats(N100)
basicStats(N500)

# LCL -> 95% media inverior
# UCL -> 95% media superior
#Se mean -> erro padrão 
#