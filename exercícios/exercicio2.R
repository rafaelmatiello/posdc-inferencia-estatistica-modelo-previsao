# Questão 2 – Em um levantamento feito por uma consultoria de clima organizacional,
# foram obtidas duas informações importantes segundo os consultores, Salário do 
# colaborador e seu Nível de Stress. Utilizando os dados 
# “Clima Organizacional.csv”, responda o que se pede:
#   a. Nesta empresa haveria diferença entre o salário pago a homens e mulheres?
#   b. Nesta empresa as mulheres possuem um nível de Stress diferente do nível 
#     de Stress dos homens? Qual é maior?

#DIRETÓRIO DE TRABALHO
setwd('C:\\repositorios\\posdc-inferencia-estatistica-modelo-previsao\\exercícios')


# carregar o database
dados2 <- read.csv("Clima Organizacional.csv", header=T, sep=";", dec = ".")


# estatítica descritiva
summary(dados2$Salario[dados2$Sexo == 0])#home
sd(dados2$Salario[dados2$Sexo == 0])
sd(dados2$Salario[dados2$Sexo == 0]) /mean(dados2$Salario[dados2$Sexo == 0]) 

# > summary(dados2$Salario[dados2$Sexo == 0])#home
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1176    3236    3851    3885    4588    6465 
# > sd(dados2$Salario[dados2$Sexo == 0])
# [1] 995.5011
# > sd(dados2$Salario[dados2$Sexo == 0]) /mean(dados2$Salario[dados2$Sexo == 0]) 
# [1] 0.2562171



summary(dados2$Salario[dados2$Sexo == 1])#mulher
sd(dados2$Salario[dados2$Sexo == 1])
sd(dados2$Salario[dados2$Sexo == 1]) /mean(dados2$Salario[dados2$Sexo == 1])

# > summary(dados2$Salario[dados2$Sexo == 1])#mulher
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 665    2606    3270    3281    3994    6277 
# > sd(dados2$Salario[dados2$Sexo == 1])
# [1] 1037.076
# > sd(dados2$Salario[dados2$Sexo == 1]) /mean(dados2$Salario[dados2$Sexo == 1])
# [1] 0.3160618



# Considerando a média pode considerar que o conhecimento aumento na média.

skewness(dados2$Salario[dados2$Sexo == 0])
skewness(dados2$Salario[dados2$Sexo == 1])#mulher

# > skewness(dados2$Salario[dados2$Sexo == 0])
# [1] -0.07093351
# attr(,"method")
# [1] "moment"
# > skewness(dados2$Salario[dados2$Sexo == 1])
# [1] 0.01639686
# attr(,"method")
# [1] "moment"


# R: Não tem problema de assimetria


kurtosis(dados2$Salario[dados2$Sexo == 0])
kurtosis(dados2$Salario[dados2$Sexo == 1])#mulher

# > kurtosis(dados2$Salario[dados2$Sexo == 0])
# [1] 0.01801823
# attr(,"method")
# [1] "excess"
# > kurtosis(dados2$Salario[dados2$Sexo == 1])
# [1] -0.1930883
# attr(,"method")
# [1] "excess"

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

# Como analisar?????


plot(dados2$Salario, dados2$Sexo, xlab="Salário", ylab="Sexo")
abline(h=c(0,1), v=c(mean(dados2$Salario[dados2$Sexo == 0])), col="red")
abline(h=c(0,1), v=c(mean(dados2$Salario[dados2$Sexo == 1])), col="blue")


# teste t amostra idependente
#H0 -> salário das mulher é igual ao dos homens
#H0 -> o salário das mulhers não é igual dos homnes

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