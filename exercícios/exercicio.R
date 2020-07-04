
# Questão 1 – Uma empresa aplicou um programa de formação de seus colaboradores
# na área de informática. Passada a aplicação deste programa, a empresa está
# avaliando o nível de efetividade do curso aplicado. Assim, foi feito um
# levantamento com seus colaboradores medindo seu nível de conhecimento em
# informática antes do curso e depois do curso. Análise estes dados e verifique
# qual a média de conhecimento antes e depois do curso, e se houve diferença no
# conhecimento percebido. Utiliza os dados do Banco “Avaliacao Formacao.csv”.


#DIRETÓRIO DE TRABALHO
setwd('C:\\repositorios\\posdc-inferencia-estatistica-modelo-previsao\\exercícios')


library(fBasics)
library(car)
library(normtest)


# carregar o database
dados1 <- read.csv("Avaliacao Formacao.csv", header=T, sep=";", dec = ".")

dif <- dados1$Conhecimento_Depois - dados1$Conhecimento_Antes

# estatítica descritiva
summary(dados1$Conhecimento_Antes)
sd(dados1$Conhecimento_Antes)
sd(dados1$Conhecimento_Antes) /mean(dados1$Conhecimento_Antes) 

basicStats(dados1$Conhecimento_Antes)

# > summary(dados1$Conhecimento_Antes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.000   4.000   4.000   4.562   6.000   9.000 
# > sd(dados1$Conhecimento_Antes)
# [1] 1.470355
# > sd(dados1$Conhecimento_Antes) /mean(dados1$Conhecimento_Antes) 
# [1] 0.3223163


summary(dados1$Conhecimento_Depois)
sd(dados1$Conhecimento_Depois)
sd(dados1$Conhecimento_Depois) /mean(dados1$Conhecimento_Depois)#coêfiente de variação
basicStats(dados1$Conhecimento_Depois)

# > summary(dados1$Conhecimento_Depois)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.000   7.000   7.000   7.329   9.000  10.000 
# > sd(dados1$Conhecimento_Depois)
# [1] 1.860792
# > sd(dados1$Conhecimento_Depois) /mean(dados1$Conhecimento_Depois)
# [1] 0.2539075


# Considerando a média pode considerar que o conhecimento aumento na média.
# -1 < 0 > 1
skewness(dados1$Conhecimento_Antes)
skewness(dados1$Conhecimento_Depois)

# > skewness(dados1$Conhecimento_Antes)
# [1] 0.2258844
# attr(,"method")
# [1] "moment"
# > skewness(dados1$Conhecimento_Depois)
# [1] -0.4791512
# attr(,"method")
# [1] "moment"

# R: Não tem problema de assimetria

# -7 < 0 > 7
kurtosis(dados1$Conhecimento_Antes)
kurtosis(dados1$Conhecimento_Depois)

# > kurtosis(dados1$Conhecimento_Antes)
# [1] -0.3997687
# attr(,"method")
# [1] "excess"
# > kurtosis(dados1$Conhecimento_Depois)
# [1] -0.4761051
# attr(,"method")
# [1] "excess"

# Não tem problemas de Curtose
library(normtest)

shapiro.test(dados1$Conhecimento_Antes)
shapiro.test(dados1$Conhecimento_Depois)
shapiro.test(dif)

# > shapiro.test(dados1$Conhecimento_Antes)
# 
# Shapiro-Wilk normality test
# 
# data:  dados1$Conhecimento_Antes
# W = 0.9482, p-value = 1.906e-08
# 
# > shapiro.test(dados1$Conhecimento_Depois)
# 
# Shapiro-Wilk normality test
# 
# data:  dados1$Conhecimento_Depois
# W = 0.92011, p-value = 3.667e-11


# h0 - a distribuição normal
# h1 - distribuição não normal
# conclusão: como o P-value= 1.906e-08 < 0.05, rejeitamos a normalidade.

# h0 - a distribuição normal
# h1 - distribuição não normal
# conclusão: como o P-value= 3.667e-11 < 0.05, rejeitamos a normalidade.

## REJEITA AS NOMARLIDADE, TRABALHAMOS COM A DIFERENÇA

# TODOS OS TESTE REJEITARAM A NORMALIDADE, ENTÃO PRECISAMOS FAZER UM TESTE PARÂMETRICO.
# PODEM O CONJUNTO DE DADOS POSSUI 283 OBSERVAÇÕES, PODENDO SER USADO A FLEXIBILIZAÇÃO
# DO PRESUPOSTO DO TEROMA CENTRAL DOS LIMITES

# Para um amostra grande, a média estimada, para qualquer probalidade, tem a convergir para normalidade.

hist(dif)


# teste de amostras pareadas, não normal!
# elementos pareados paired = T
# H0 -> a média é igual a media depois.
# H1 -> a media antes NÃO é igual a media depois
t.test(dados1$Conhecimento_Depois, dados1$Conhecimento_Antes, paired = T) # teste Bi-caudal

# > t.test(dados1$Conhecimento_Depois, dados1$Conhecimento_Antes, paired = T) # teste Bi-caudal
# 
# Paired t-test
# 
# data:  dados1$Conhecimento_Depois and dados1$Conhecimento_Antes
# t = 42.602, df = 282, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.638946 2.894623
# sample estimates:
#   mean of the differences 
# 2.766784  



# conclusão: com p-value = 2.2e-16M 0.05 que é o alfa de significancia, rejeita H0, aceita h1
# logo, há diferença entre o nível de conhecimento antes de depois da formação 



# teste unicaudal, 
# h0 -> a média antes é igual a media depois
# h1 -> a media antes é menor que media depois
# unicaudal o peso antes ser maior que o peso depois
t.test(dados1$Conhecimento_Antes, dados1$Conhecimento_Depois, alternative = "less", paired = T) 

# > t.test(dados1$Conhecimento_Antes, dados1$Conhecimento_Depois, alternative = "less", paired = T) 
# 
# Paired t-test
# 
# data:  dados1$Conhecimento_Antes and dados1$Conhecimento_Depois
# t = -42.602, df = 282, p-value < 2.2e-16
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf -2.659608
# sample estimates:
#   mean of the differences 
# -2.766784 

# conclusão: com p-value = 2.2e-16 é menor que 0.05 que é o alfa de significancia, rejeita H0 e assumimos h1
# logo, a media antes é menor que a media depois



# dentro do rank se mudou de posição
wilcox.test(dados1$Conhecimento_Depois, dados1$Conhecimento_Antes, paired = T)
# > wilcox.test(dados1$Conhecimento_Depois, dados1$Conhecimento_Antes, paired = T)
# 
# Wilcoxon signed rank test with continuity correction
# 
# data:  dados1$Conhecimento_Depois and dados1$Conhecimento_Antes
# V = 37401, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

# conclusão, como o p-value = 0000 < 0.05, rejeitamos H0 e aceitamos H1, 
# confirmando o teste t
