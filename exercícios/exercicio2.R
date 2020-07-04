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
summary(dados2$Salario)
summary(dados2$Salario[dados2$Sexo == 0])#home
summary(dados2$Salario[dados2$Sexo == 1])#mulher

# > summary(dados2$Salario)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 665    2858    3563    3565    4284    6465 
# > summary(dados2$Salario[dados2$Sexo == 0])#home
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1176    3236    3851    3885    4588    6465 
# > summary(dados2$Salario[dados2$Sexo == 1])#mulher
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 665    2606    3270    3281    3994    6277 
# > 


basicStats(dados2$Salario)
basicStats(dados2$Salario[dados2$Sexo == 0])#home
basicStats(dados2$Salario[dados2$Sexo == 1])#mulher
 
# > basicStats(dados2$Salario)
# X..dados2.Salario
# nobs             4.230000e+02
# NAs              0.000000e+00
# Minimum          6.650000e+02
# Maximum          6.465000e+03
# 1. Quartile      2.858500e+03
# 3. Quartile      4.284000e+03
# Mean             3.565461e+03
# Median           3.563000e+03
# Sum              1.508190e+06
# SE Mean          5.155895e+01
# LCL Mean         3.464117e+03
# UCL Mean         3.666805e+03
# Variance         1.124471e+06
# Stdev            1.060411e+03
# Skewness        -4.883200e-02
# Kurtosis        -1.301220e-01
# > basicStats(dados2$Salario[dados2$Sexo == 0])#home
# X..dados2.Salario.dados2.Sexo....0
# nobs                                199.000000
# NAs                                   0.000000
# Minimum                            1176.000000
# Maximum                            6465.000000
# 1. Quartile                        3236.000000
# 3. Quartile                        4587.500000
# Mean                               3885.381910
# Median                             3851.000000
# Sum                              773191.000000
# SE Mean                              70.569205
# LCL Mean                           3746.218204
# UCL Mean                           4024.545615
# Variance                         991022.520075
# Stdev                               995.501140
# Skewness                             -0.070934
# Kurtosis                              0.018018
# > basicStats(dados2$Salario[dados2$Sexo == 1])#mulher
# X..dados2.Salario.dados2.Sexo....1
# nobs                              2.240000e+02
# NAs                               0.000000e+00
# Minimum                           6.650000e+02
# Maximum                           6.277000e+03
# 1. Quartile                       2.606500e+03
# 3. Quartile                       3.993500e+03
# Mean                              3.281246e+03
# Median                            3.270500e+03
# Sum                               7.349990e+05
# SE Mean                           6.929257e+01
# LCL Mean                          3.144694e+03
# UCL Mean                          3.417798e+03
# Variance                          1.075527e+06
# Stdev                             1.037076e+03
# Skewness                          1.639700e-02
# Kurtosis                         -1.930880e-01





# Não tem problemas de Curtose
library(normtest)

shapiro.test(dados2$Salario[dados2$Sexo == 0])
shapiro.test(dados2$Salario[dados2$Sexo == 1])#mulher

# > shapiro.test(dados2$Salario[dados2$Sexo == 0])
# 
# Shapiro-Wilk normality test
# 
# data:  dados2$Salario[dados2$Sexo == 0]
# W = 0.9923, p-value = 0.3789
# 
# > shapiro.test(dados2$Salario[dados2$Sexo == 1])#mulher
# 
# Shapiro-Wilk normality test
# 
# data:  dados2$Salario[dados2$Sexo == 1]
# W = 0.99743, p-value = 0.9765


# h0 - a distribuição normal
# h1 - distribuição não normal
# conclusão: como o P-value= 0.3789 > 0.05, Não rejeitamos a normalidade para homens

# h0 - a distribuição normal
# h1 - distribuição não normal
# conclusão: como o P-value= 0.9765 > 0.05, Não rejeitamos a normalidade.

hist(dados2$Salario[dados2$Sexo == 0])
hist(dados2$Salario[dados2$Sexo == 1])


library(car)


# teste de levene -> igualdade das variáncias.
# h0, as variancias são igual
# h1, as variancias são diferentes

leveneTest(dados2$Salario, dados2$Sexo)

# > leveneTest(dados2$Salario, dados2$Sexo)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   1  0.7427 0.3893
#       421               
# Warning message:
# In leveneTest.default(dados2$Salario, dados2$Sexo) :
#   dados2$Sexo coerced to factor.

# Pr(>F) > 0.05, 
# Como 0.3893 > 0.05, então aceitamos H0, logo as veriáncias são iguals
## é invertido


# gráfico

plot(dados2$Salario, dados2$Sexo, xlab="Salário", ylab="Sexo")
abline(h=c(0,1), v=c(mean(dados2$Salario[dados2$Sexo == 0])), col="red")
abline(h=c(0,1), v=c(mean(dados2$Salario[dados2$Sexo == 1])), col="blue")


# teste t amostra idependente
#H0 -> as médias são iguais
#H0 -> as médias são diferentes

t.test(dados2$Salario ~ dados2$Sexo, var.equals=T)

# > t.test(dados2$Salario ~ dados2$Sexo, var.equals=T)
# 
# Welch Two Sample t-test
# 
# data:  dados2$Salario by dados2$Sexo
# t = 6.1085, df = 418.47, p-value = 2.301e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   409.7311 798.5417
# sample estimates:
#   mean in group 0 mean in group 1 
# 3885.382        3281.246 

# Considerando o p-value 2.301e-09 < 0.05, logo rejeitamos HO, a media salárial dos homens é diferente das mulheres
# A média do salário do homens é maior que as mulheres podemos, os em média possuem um salário maior que as mulheres.





# teste t amostra idependente - inicaldal
#H0 -> média salárial das mulheres é menor ou igual a dos homen
#H0 -> média salárial das mulheres NÃO émenor a dos homen
t.test(dados2$Salario ~ dados2$Sexo, alternative="greater")

# > t.test(dados2$Salario ~ dados2$Sexo, alternative="greater")
# 
# Welch Two Sample t-test
# 
# data:  dados2$Salario by dados2$Sexo
# t = 6.1085, df = 418.47, p-value = 1.151e-09
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   441.0972      Inf
# sample estimates:
#   mean in group 0 mean in group 1 
# 3885.382        3281.246 

# como o p-value é 1.151e-09 < 0.05, rejeitamos h0
# logo, temos que homens possuem salário médio maior que o salário medio da mulheres


# Como fez relação com os grupos????