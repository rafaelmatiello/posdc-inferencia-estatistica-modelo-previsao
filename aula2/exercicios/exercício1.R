#DIRETÓRIO DE TRABALHO
setwd('C:/repositorios/posdc-inferencia-estatistica-modelo-previsao/aula2/exercicios')

#------------------------------------------------
# Test t de uma amostra

# carregar o database
dados1 <- read.csv("Dados Exemplos Teste t uma amostra.csv", header=T, sep=";", dec = ".")

# estatítica descritiva
summary(dados1$horas.Semanais)
sd(dados1$horas.Semanais)

library(fBasics)
# assimetria -1 a 1
skewness(dados1$horas.Semanais)
# curtose, -7 a 7
kurtosis(dados1$horas.Semanais)

# teste de normalidade

library(normtest)
# h0 - a distribuição normal
#h1 - distribuição não normal
shapiro.test(dados1$horas.Semanais)
# W = 0.96262, p-value = 0.5973
# conclusão: como o P-value= 0.5973 > 0.05, não rejeitamos a normalidade.

# Test t de uma amostra
# H0, média de horas semanais trabalhadas é IGUAL a 44 horas semanais
# H1, a média de horas semanais trabalhadas, NÃO É IGUAL a 44 horas semanais.
# alfa = 0.05 -> 5 porcento de erro
# mu -> numero padrão que quer comparar

t.test(dados1$horas.Semanais, mu=44)

# One Sample t-test
# 
# data:  dados1$horas.Semanais
# t = -0.097984, df = 19, p-value = 0.923
# alternative hypothesis: true mean is not equal to 44
# 95 percent confidence interval:
#   42.88195 45.01805
# sample estimates:
#   mean of x 
# 43.95 


# conclusão: como p-value = 0.097984 > 0.05 alfa, não rejeitamos h0, se menor pode aceitar a alternativa
# logo podemov assumir como verdadeira a ideia que em média trabalham 44 horas em 95% dos casos



#------------------------------------------------------
# teste t duas amostras pareadas

# nutricionista testando novo tipo de dieta, peso antes e depois

dados2 <- read.csv("Dados Exemplos Teste t par.csv", header=T, sep=";", dec = ".")

dif <- dados2$Depois-dados2$Antes

summary(dados2$Antes)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 62.0    72.5    77.5    77.1    81.5    94.0 
summary(dados2$Depois)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 60.00   74.00   75.50   75.30   79.25   90.00 

sd(dados2$Antes)

sd(dados2$Antes) /mean(dados2$Antes) # coeficiente da variação = 0.1211244, 12%

sd(dados2$Depois)
sd(dados2$Depois) /mean(dados2$Depois) # coeficiente da variação = 0.111119, 11%

# 1% na diferença da variação

skewness(dados2$Antes)
skewness(dados2$Depois)

# > skewness(dados2$Antes)
# [1] 0.08613553
# attr(,"method")
# [1] "moment"
# > skewness(dados2$Depois)
# [1] -0.1864727
# attr(,"method")
# [1] "moment"


kurtosis(dados2$Antes)
kurtosis(dados2$Depois)

# > kurtosis(dados2$Antes)
# [1] -0.9432308
# attr(,"method")
# [1] "excess"
# > kurtosis(dados2$Depois)
# [1] -0.6483895
# attr(,"method")
# [1] "excess"


# indicam normalidade

# teste de normalidade
# 

shapiro.test(dados2$Antes)
# Shapiro-Wilk normality test
# 
# data:  dados2$Antes
# W = 0.98879, p-value = 0.9951


shapiro.test(dados2$Depois)
# Shapiro-Wilk normality test
# 
# data:  dados2$Depois
# W = 0.95251, p-value = 0.6982

shapiro.test(dif)
# Shapiro-Wilk normality test
# 
# data:  dif
# W = 0.95027, p-value = 0.6717


hist(dif)

# teste de amostras pareadas
# elementos pareados paired = T
# H0 -> a média é igual a media depois.
# H1 -> a media antes NÃO é igual a media depois
t.test(dados2$Antes, dados2$Depois, paired = T) # teste Bi-caudal
# Paired t-test
# 
# data:  dados2$Antes and dados2$Depois
# t = 1.9905, df = 9, p-value = 0.07774
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.2456928  3.8456928
# sample estimates:
#   mean of the differences 
# 1.8 

# conclusão: com p-value = 0.07774 é maior que 0.05 que é o alfa de significancia, não rejeita H0
# logo, o peso antes da dieta é gual em média ao peso depois.

# teste unicaudal, 
#h0 -> a média antes é igual a media depois
# h1 -> a media antes é maior que media depois
t.test(dados2$Antes, dados2$Depois, alternative = "greater", paired = T) # unicaudal o peso antes ser maior que o peso depois

# Paired t-test
# 
# data:  dados2$Antes and dados2$Depois
# t = 1.9905, df = 9, p-value = 0.03887
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   0.1422964       Inf
# sample estimates:
#   mean of the differences 
# 1.8 

# conclusão: com p-value = 0.03887 é menor que 0.05 que é o alfa de significancia, rejeita H0 e assumimos h1
# logo, o peso antes da dieta é maior que o peso depois.

# bi-caudal X uni-caudal
# bi-caudal -> igual o erro considera antes e depois da média, erro para os dois lados
# uni-caudal -> considera o erro só de um lado da amostra, no caso para mais. erro só de um lado.

# de repetir 100 vez, tem 96% de hipotese que acontecer, 3 % da meia

#------------------------------------------------------------
# teste t duas amostras independentes
dados3 <-  read.csv("Dados Exemplos Teste t ind.csv", header=T, sep=";", dec = ".")

# sexo 0 homem 1 mulher, e anotado o peso da comida, tendencia que home comem mais que mulheres

# estatísticas descritivas

summary(dados3$peso_comida) # peso total
summary(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
summary(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher

# > summary(dados3$peso_comida) # peso total
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 300.0   450.0   540.0   545.7   660.0   860.0 
# > summary(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 450.0   550.0   620.0   619.1   670.0   860.0 
# > summary(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 300     405     465     465     520     680 


sd(dados3$peso_comida) # peso total
sd(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
sd(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher

# > sd(dados3$peso_comida) # peso total
# [1] 139.8775
# > sd(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
# [1] 122.7562
# > sd(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher
# [1] 113.6515

sd(dados3$peso_comida)/mean(dados3$peso_comida) # coefiênte de variação
sd(dados3$peso_comida[dados3$sexo == 0])/mean(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
sd(dados3$peso_comida[dados3$sexo == 1])/mean(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher

# Mulheres tem uma variáção maior 24%

# > sd(dados3$peso_comida)/mean(dados3$peso_comida) # coefiênte de variação
# [1] 0.25632
# > sd(dados3$peso_comida[dados3$sexo == 0])/mean(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
# [1] 0.1982847
# > sd(dados3$peso_comida[dados3$sexo == 1])/mean(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher
# [1] 0.2444119

skewness(dados3$peso_comida) # peso total
skewness(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
skewness(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher





kurtosis(dados3$peso_comida) # peso total
kurtosis(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
kurtosis(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher


# > kurtosis(dados3$peso_comida) # peso total
# [1] -0.5452198
# attr(,"method")
# [1] "excess"
# > kurtosis(dados3$peso_comida[dados3$sexo == 0]) # peso para homens
# [1] -0.8501551
# attr(,"method")
# [1] "excess"
# > kurtosis(dados3$peso_comida[dados3$sexo == 1]) # peso para Mulher
# [1] -0.9370651
# attr(,"method")
# [1] "excess"



# teste de normalidade
# h0, a distribuição é normal
# h1, a distribuição dos dados não é normal

shapiro.test(dados3$peso_comida)
shapiro.test(dados3$peso_comida[dados3$sexo == 0])
shapiro.test(dados3$peso_comida[dados3$sexo == 1])

# conclusão: normalidade satisfeita


library(car)

# teste de levene -> igualdade das variáncias.
# h0, as variancias são igual
# h1, as variancias são diferentes


leveneTest(dados3$peso_comida, dados3$sexo)



# !!!!tem algo errado aqui
plot(dados3$peso_comida, dados3$sexo, xlab="Peso da comuda (g)", ylab="Sexo")
abline(h=c(0,1), v=c(mean(dados3$peso_comida[dados3$sexo == 0]), dados3$peso_comida[dados3$sexo == 1]), col="red")

# teste t amostra idependente
#H0 -> peso da comida das mulher é igual a dos homen
#H0 -> peso da comida das mulher NÃO é igual a dos homen

# ~ comparado
t.test(dados3$peso_comida ~ dados3$sexo)
t.test(dados3$peso_comida ~ dados3$sexo, var.equals=T)

# como o p-value é 0.075 < 0.05 rejeitamos h0
# logo, temos que homens consomen peso médio de comida diferente do peso media de comida das mulheres;


# teste t amostra idependente - inicaldal
#H0 -> peso da comida das mulher é menor igual a dos homen
#H0 -> peso da comida das mulher NÃO é  menor a dos homen
t.test(dados3$peso_comida ~ dados3$sexo, alternative="greater")

# como o p-value é 0.003792 < 0.05, rejeitamos h0
# logo, temos que homens consomen peso maior médio de comida maior do peso media de comida das mulheres;



#-------------------------------------------
# teste anova
#-------------------------------------------

# três grupos de tratamento, para tratamento, tem um taxa e hemoglobina, em um dos três grupos que diferenciaria dos demais


dados4 <-  read.csv("Dados Exemplos ANOVA.csv", header=T, sep=";", dec = ".")


# estatísticas descritivas
summary(dados4$tx_Hemo)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.70   11.28   11.85   11.80   12.43   14.50 
summary(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
# acima da média geral
summary(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
# abaixo da media geral
summary(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3
# mais perto da media geral

# > summary(dados4$tx_Hemo)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.70   11.28   11.85   11.80   12.43   14.50 
# > summary(dados4$tx_Hemo[dados4$Grupo ==1])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.50   12.00   12.90   12.89   13.53   14.50 
# > summary(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.70   10.80   11.35   11.10   12.03   12.50 
# > summary(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 10.50   11.12   11.45   11.41   11.70   12.20 

# desvio padrão
sd(dados4$tx_Hemo)
sd(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
sd(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
sd(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3


# > sd(dados4$tx_Hemo)
# [1] 1.230563
# > sd(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
# [1] 0.9248669
# > sd(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
# [1] 1.29755
# > sd(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3
# [1] 0.526495


# coêfiente de variáção
sd(dados4$tx_Hemo) /mean(dados4$tx_Hemo)
sd(dados4$tx_Hemo[dados4$Grupo ==1]) / mean(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
sd(dados4$tx_Hemo[dados4$Grupo ==2]) / mean(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
sd(dados4$tx_Hemo[dados4$Grupo ==3]) / mean(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3


# > sd(dados4$tx_Hemo) /mean(dados4$tx_Hemo)
# [1] 0.104285
# > sd(dados4$tx_Hemo[dados4$Grupo ==1]) / mean(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
# [1] 0.07174145
# > sd(dados4$tx_Hemo[dados4$Grupo ==2]) / mean(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
# [1] 0.1168964
# > sd(dados4$tx_Hemo[dados4$Grupo ==3]) / mean(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3
# [1] 0.04615004

# assimetria
skewness(dados4$tx_Hemo)
skewness(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
skewness(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
skewness(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3


# > skewness(dados4$tx_Hemo)
# [1] -0.2956028
# attr(,"method")
# [1] "moment"
# > skewness(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
# [1] 0.08281977
# attr(,"method")
# [1] "moment"
# > skewness(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
# [1] -0.7404093
# attr(,"method")
# [1] "moment"
# > skewness(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3
# [1] -0.1834258
# attr(,"method")
# [1] "moment"

# curtose
kurtosis(dados4$tx_Hemo)
kurtosis(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
kurtosis(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
kurtosis(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3


# > kurtosis(dados4$tx_Hemo)
# [1] 0.4540532
# attr(,"method")
# [1] "excess"
# > kurtosis(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
# [1] -1.385755
# attr(,"method")
# [1] "excess"
# > kurtosis(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
# [1] -1.051128
# attr(,"method")
# [1] "excess"
# > kurtosis(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3
# [1] -1.219269
# attr(,"method"

# teste de normalidade
  #H0 -> a distribuição é normal
  #H1 -> a distribuição NÃO é normal

shapiro.test(dados4$tx_Hemo)
shapiro.test(dados4$tx_Hemo[dados4$Grupo ==1]) # grupo 1
shapiro.test(dados4$tx_Hemo[dados4$Grupo ==2]) # grupo 2
shapiro.test(dados4$tx_Hemo[dados4$Grupo ==3]) # grupo 3

# conclusão o teste ANOVA, precisa ser analisado com um pouco de cuidade devido a não normalidade dos dados do grupos 2



# teste de levene, 
# h0 -> os grupos possuen a mesma variância entre si
# h2 -> os grupos possuen variâncias distintas.

leveneTest(dados4$tx_Hemo, dados4$Grupo)
# Levene's Test for Homogeneity of Variance (center = median)
#       Df F value Pr(>F)
# group  2  2.3758 0.1087
#       33      

#conclusão: como p-value (pr(>F)) = 0.1087 > 0.05 = alfa, não rejeita, e fica com H0

# conclusão que os grupos possuen variáncias semelhantes entre si.
# probabidade de erro  "0.1087" 10%




# Analise gráfica

plot(dados4$tx_Hemo, dados4$Grupo, xlab="Taxa de hemoglobina", ylab="Grupos tratamento")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo == 1]), col="red")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo == 2]), col="green")
abline(v=mean(dados4$tx_Hemo[dados4$Grupo == 3]), col="blue")

# grupo 1 se difere bastante do 2 e do 3, a média esta bem a direita.


# teste ANOVA
# h0 -> os grupos possem médias iguais
#h1 -> extem 2 grupos com média distintas
ANOVA <- aov(dados4$tx_Hemo ~dados4$Grupo)

summary(ANOVA)

# Df Sum Sq Mean Sq F value  Pr(>F)   
# dados4$Grupo  1   13.2  13.202   11.28 0.00194 **
# Residuals    34   39.8   1.171                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# residuais, variáção dentro dos grupos

# significancia 0.001 ‘**’ 0.01, então rejeita o 0


# Conclusão: com o p-value = 0.00194 < 0.05, então rejeita a H0 e aceita H1



#Testes 2 a 2
t.test(dados4$tx_Hemo[dados4$Grupo != 3] ~dados4$Grupo[dados4$Grupo != 3]) # 1 e 2 
t.test(dados4$tx_Hemo[dados4$Grupo != 2] ~dados4$Grupo[dados4$Grupo != 2]) # 1 e 3
t.test(dados4$tx_Hemo[dados4$Grupo != 1] ~dados4$Grupo[dados4$Grupo != 1]) # 2 e 3



##---------------------
# Paramétricos


# Test wilcoxon - alteranativa ao teste t de 2 amostras pareadas.
# h0 -> peso anters é igual o peso depois
# h1 -> peso anters diferente ao peso depois
wilcox.test(dados2$Antes, dados2$Depois, paired = T)

# 
# Wilcoxon signed rank test with continuity correction
# 
# data:  dados2$Antes and dados2$Depois
# V = 37, p-value = 0.09485
# alternative hypothesis: true location shift is not equal to 0

# Conclusão: o p-value = 0.09485 > 0.05 = alfa, não rejeitamos h0



# Test Mann-whitney - alteranativa ao teste t de 2 amostras independetes
# H0 - Peso da comida dos homens é igual o peso da comida das mulheres
# h1 - peso da comida dos homens é diferente do peso da comida das mulheres.
wilcox.test(dados3$peso_comida ~dados3$sexo)
# Wilcoxon rank sum test with continuity correction
# 
# data:  dados3$peso_comida by dados3$sexo
# W = 91, p-value = 0.0122
# alternative hypothesis: true location shift is not equal to 0

# conclusão: como p-value = 0.0122 < 0.05 = alfa, então rejeitamos a H0, aceitamos H1.



# Test kruskal-wallis - alteranativa ao teste ANOVA
# H0 - Os grupos possuem o mesmo nível médio de hemoglobina
# h1 - Existem, pelo menos 2 grupos diferentes de hemoglobina

kruskal.test(dados4$tx_Hemo~dados4$Grupo)
# 
# Kruskal-Wallis rank sum test
# 
# data:  dados4$tx_Hemo by dados4$Grupo
# Kruskal-Wallis chi-squared = 15.078, df = 2, p-value = 0.0005319

# conclusão: p-value  = 0.0005319 < 0.05 = alfa, logo rejeitamos h0 e aceitamos h1


#-------------------------------------------------------
#  Test Qui-quadrado 
# H0 - Não é discrepancia entre o esperado e o realizado
# H1 - Há é discrepancia entre o esperado e o realizado

dados5 <-  read.csv("Dados Exemplos Chi-Square.csv", header=T, sep=";", dec = ",")

chisq.test(dados5$Tratamento, dados5$Dor_Abdomen)



