# Questão 2 – Em um levantamento feito por uma consultoria de clima organizacional,
# foram obtidas duas informações importantes segundo os consultores, Salário do 
# colaborador e seu Nível de Stress. Utilizando os dados 
# “Clima Organizacional.csv”, responda o que se pede:
#   a. Nesta empresa haveria diferença entre o salário pago a homens e mulheres?
#   b. Nesta empresa as mulheres possuem um nível de Stress diferente do nível 
#     de Stress dos homens? Qual é maior?

#DIRETÓRIO DE TRABALHO
setwd('C:\\repositorios\\posdc-inferencia-estatistica-modelo-previsao\\exercícios')

library(fBasics)
library(car)
library(normtest)


# carregar o database
dados2 <- read.csv("Clima Organizacional.csv", header=T, sep=";", dec = ".")


# estatítica descritiva
summary(dados2$Stress)
summary(dados2$Stress[dados2$Sexo == 0])#home
summary(dados2$Stress[dados2$Sexo == 1])#mulher

# > summary(dados2$Stress)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 665    2858    3563    3565    4284    6465 
# > summary(dados2$Stress[dados2$Sexo == 0])#home
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1176    3236    3851    3885    4588    6465 
# > summary(dados2$Stress[dados2$Sexo == 1])#mulher
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 665    2606    3270    3281    3994    6277 
# > 


basicStats(dados2$Stress)
basicStats(dados2$Stress[dados2$Sexo == 0])#home
basicStats(dados2$Stress[dados2$Sexo == 1])#mulher

# > shapiro.test(dados2$Stress[dados2$Sexo == 1])#mulher
# 
# Shapiro-Wilk normality test
# 
# data:  dados2$Stress[dados2$Sexo == 1]
# W = 0.99476, p-value = 0.6335
# 
# > basicStats(dados2$Stress)
# X..dados2.Stress
# nobs              423.000000
# NAs                 0.000000
# Minimum            19.000000
# Maximum           101.000000
# 1. Quartile        55.000000
# 3. Quartile        74.000000
# Mean               64.586288
# Median             65.000000
# Sum             27320.000000
# SE Mean             0.691370
# LCL Mean           63.227330
# UCL Mean           65.945247
# Variance          202.190996
# Stdev              14.219388
# Skewness           -0.097209
# Kurtosis           -0.004287
# > basicStats(dados2$Stress[dados2$Sexo == 0])#home
# X..dados2.Stress.dados2.Sexo....0
# nobs                               199.000000
# NAs                                  0.000000
# Minimum                             19.000000
# Maximum                             83.000000
# 1. Quartile                         49.000000
# 3. Quartile                         63.000000
# Mean                                55.030151
# Median                              56.000000
# Sum                              10951.000000
# SE Mean                              0.781847
# LCL Mean                            53.488336
# UCL Mean                            56.571966
# Variance                           121.645551
# Stdev                               11.029304
# Skewness                            -0.402981
# Kurtosis                             0.162184
# > basicStats(dados2$Stress[dados2$Sexo == 1])#mulher
# X..dados2.Stress.dados2.Sexo....1
# nobs                               224.000000
# NAs                                  0.000000
# Minimum                             40.000000
# Maximum                            101.000000
# 1. Quartile                         66.000000
# 3. Quartile                         79.250000
# Mean                                73.075893
# Median                              73.000000
# Sum                              16369.000000
# SE Mean                              0.734133
# LCL Mean                            71.629167
# UCL Mean                            74.522619
# Variance                           120.725156
# Stdev                               10.987500
# Skewness                             0.103697
# Kurtosis                            -0.037665



# Não tem problemas de Curtose
library(normtest)

shapiro.test(dados2$Stress[dados2$Sexo == 0])
shapiro.test(dados2$Stress[dados2$Sexo == 1])#mulher

# > shapiro.test(dados2$Stress[dados2$Sexo == 0])
# 
# Shapiro-Wilk normality test
# 
# data:  dados2$Stress[dados2$Sexo == 0]
# W = 0.98769, p-value = 0.08258
# 
# > shapiro.test(dados2$Stress[dados2$Sexo == 1])#mulher
# 
# Shapiro-Wilk normality test
# 
# data:  dados2$Stress[dados2$Sexo == 1]
# W = 0.99476, p-value = 0.6335


# h0 - a distribuição normal
# h1 - distribuição não normal
# conclusão: como o P-value= 0.08258 > 0.05, Não rejeitamos a normalidade para homens

# h0 - a distribuição normal
# h1 - distribuição não normal
# conclusão: como o P-value= 0.6335 > 0.05, Não rejeitamos a normalidade.

hist(dados2$Stress[dados2$Sexo == 0])
hist(dados2$Stress[dados2$Sexo == 1])


library(car)


# teste de levene -> igualdade das variáncias.
# h0, as variancias são igual
# h1, as variancias são diferentes

leveneTest(dados2$Stress, dados2$Sexo)

# > leveneTest(dados2$Stress, dados2$Sexo)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   1  0.0138 0.9064
#       421   

#conclusão: 0.9064 > 0.05, não rejeiramos H0


# gráfico

plot(dados2$Stress, dados2$Sexo, xlab="Salário", ylab="Sexo")
abline(h=c(0,1), v=c(mean(dados2$Stress[dados2$Sexo == 0])), col="red")
abline(h=c(0,1), v=c(mean(dados2$Stress[dados2$Sexo == 1])), col="blue")


# teste t amostra idependente
#H0 -> as médias são iguais
#H0 -> as médias são diferentes

t.test(dados2$Stress ~ dados2$Sexo, var.equals=T)

# > t.test(dados2$Stress ~ dados2$Sexo, var.equals=T)
# 
# Welch Two Sample t-test
# 
# data:  dados2$Stress by dados2$Sexo
# t = -16.826, df = 414.78, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -20.15394 -15.93755
# sample estimates:
#   mean in group 0 mean in group 1 
# 55.03015        73.07589 

# Considerando o p-value 2.2e-16 < 0.05, logo rejeitamos HO, a media de stress dos homens é diferente das mulheres
# A média do stress das mulheres é maior que as homens podemos, os em média possuem um stress maior que as homens





# teste t amostra idependente - inicaldal
#H0 -> stress das mulheres é menor ou igual a dos homen
#H0 -> stress das mulheres NÃO émenor a dos homen
t.test(dados2$Stress ~ dados2$Sexo, alternative="greater")

# > t.test(dados2$Stress ~ dados2$Sexo, alternative="greater")
# 
# Welch Two Sample t-test
# 
# data:  dados2$Stress by dados2$Sexo
# t = -16.826, df = 414.78, p-value = 1
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -19.81378       Inf
# sample estimates:
#   mean in group 0 mean in group 1 
# 55.03015        73.07589

# como o p-value é 1 > 0.05, Não rejeitamos h0
# logo, temos que mulher tem mais stress que os homens


# Teste Não parametricos



wilcox.test(dados2$Stress~dados2$Sexo)
# > wilcox.test(dados2$Stress~dados2$Sexo)
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  dados2$Stress by dados2$Sexo
# W = 5234, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0