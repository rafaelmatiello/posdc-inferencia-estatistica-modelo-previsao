# Questão 3 – Um levantamento feito em restaurantes do estado de Santa Catarina,
# identificou a os preços médios praticados para três tipos de comida,
# (1) Comida Italiana; 
# (2) Comida Japonesa; e 
# (3) Churrascaria. 
# Verifique se há diferença entre estas três categorias de alimentação.
#   Qual das categorias se diferencia mais das demais? 
#   Utiliza os dados de “Preco Comida.csv”.


#DIRETÓRIO DE TRABALHO
setwd('C:\\repositorios\\posdc-inferencia-estatistica-modelo-previsao\\exercícios')


# carregar o database
dados <- read.csv("Preco Comida.csv", header=T, sep=";", dec = ",")


# estatísticas descritivas
summary(dados$Preco_Kg)
# > summary(dados$Preco_Kg)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.44   18.00   23.77   23.23   27.76   36.79 


summary(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# > summary(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11.44   15.22   16.68   16.90   18.37   21.36

# abaixo da média geral

summary(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# > summary(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.78   22.68   23.99   24.33   26.25   32.06 

# proximo a média


summary(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# 
# > summary(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 24.88   28.73   30.48   30.34   31.80   36.79 
# acima da média



basicStats(dados$Preco_Kg)
basicStats(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
basicStats(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
basicStats(dados$Preco_Kg[dados$Comida ==3]) # grupo 3


# > basicStats(dados$Preco_Kg)
# X..dados.Preco_Kg
# nobs               150.000000
# NAs                  0.000000
# Minimum             11.440000
# Maximum             36.790000
# 1. Quartile         18.002500
# 3. Quartile         27.762500
# Mean                23.235000
# Median              23.765000
# Sum               3485.250000
# SE Mean              0.481281
# LCL Mean            22.283983
# UCL Mean            24.186017
# Variance            34.744644
# Stdev                5.894459
# Skewness             0.075647
# Kurtosis            -1.002519
# > basicStats(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# X..dados.Preco_Kg.dados.Comida....1
# nobs                                  52.000000
# NAs                                    0.000000
# Minimum                               11.440000
# Maximum                               21.360000
# 1. Quartile                           15.225000
# 3. Quartile                           18.372500
# Mean                                  16.900962
# Median                                16.680000
# Sum                                  878.850000
# SE Mean                                0.319007
# LCL Mean                              16.260527
# UCL Mean                              17.541396
# Variance                               5.291821
# Stdev                                  2.300396
# Skewness                               0.083028
# Kurtosis                              -0.480234
# > basicStats(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# X..dados.Preco_Kg.dados.Comida....2
# nobs                                  61.000000
# NAs                                    0.000000
# Minimum                               14.780000
# Maximum                               32.060000
# 1. Quartile                           22.680000
# 3. Quartile                           26.250000
# Mean                                  24.326557
# Median                                23.990000
# Sum                                 1483.920000
# SE Mean                                0.412136
# LCL Mean                              23.502162
# UCL Mean                              25.150953
# Variance                              10.361243
# Stdev                                  3.218888
# Skewness                              -0.256800
# Kurtosis                               0.550086
# > basicStats(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# X..dados.Preco_Kg.dados.Comida....3
# nobs                                  37.000000
# NAs                                    0.000000
# Minimum                               24.880000
# Maximum                               36.790000
# 1. Quartile                           28.730000
# 3. Quartile                           31.800000
# Mean                                  30.337297
# Median                                30.480000
# Sum                                 1122.480000
# SE Mean                                0.441889
# LCL Mean                              29.441106
# UCL Mean                              31.233489
# Variance                               7.224826
# Stdev                                  2.687904
# Skewness                               0.311713
# Kurtosis                               0.092137



# teste de normalidade
#H0 -> a distribuição é normal
#H1 -> a distribuição NÃO é normal

shapiro.test(dados$Preco_Kg)
shapiro.test(dados$Preco_Kg[dados$Comida ==1]) # italiana
shapiro.test(dados$Preco_Kg[dados$Comida ==2]) # japonesa
shapiro.test(dados$Preco_Kg[dados$Comida ==3]) # churrascaria

# > shapiro.test(dados$Preco_Kg)
# 
# Shapiro-Wilk normality test
# 
# data:  dados$Preco_Kg
# W = 0.97091, p-value = 0.00285
# 
# > shapiro.test(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# 
# Shapiro-Wilk normality test
# 
# data:  dados$Preco_Kg[dados$Comida == 1]
# W = 0.97908, p-value = 0.4877
# 
# > shapiro.test(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# 
# Shapiro-Wilk normality test
# 
# data:  dados$Preco_Kg[dados$Comida == 2]
# W = 0.98307, p-value = 0.5597
# 
# > shapiro.test(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# 
# Shapiro-Wilk normality test
# 
# data:  dados$Preco_Kg[dados$Comida == 3]
# W = 0.97352, p-value = 0.5119

# consideração somente a média geral esta fora da normalidade



# teste de levene, 
# h0 -> os grupos possuen a mesma variância entre si
# h2 -> os grupos possuen variâncias distintas.
leveneTest(dados$Preco_Kg, dados$Comida)

# > leveneTest(dados$Preco_Kg, dados$Comida)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group   2  1.2907 0.2782
#       147               
# Warning message:
# In leveneTest.default(dados$Preco_Kg, dados$Comida) :
#   dados$Comida coerced to factor.


#conclusão: como p-value (pr(>F)) = 0.2782 > 0.05 = alfa, não rejeita, e fica com H0

# conclusão que os grupos possuen variáncias semelhantes entre si.
# probabidade de erro  "0.2782" 27.82%



plot(dados$Preco_Kg,  dados$Comida, xlab="Preço", ylab="Comida")
abline(v=mean(dados$Preco_Kg[dados$Comida == 1]), col="red")
abline(v=mean(dados$Preco_Kg[dados$Comida == 2]), col="green")
abline(v=mean(dados$Preco_Kg[dados$Comida == 3]), col="blue")


# teste ANOVA
# h0 -> os grupos possem médias iguais
# h1 -> extem 2 grupos com média distintas
ANOVA <- aov(dados$Preco_Kg ~ dados$Comida)
summary(ANOVA)
# > summary(ANOVA)
# Df Sum Sq Mean Sq F value Pr(>F)    
# dados$Comida   1   4007    4007   507.1 <2e-16 ***
#   Residuals    148   1170       8                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#consideração: 0 ‘***’ 0.001 é menor que 0.05, então rejeita H0

# Conclusão como o  Pr(>F) 2e-16 < 0.05, rejeitamos H0 e aceitamos H1. 
# Logo a pelo menos um grupo, que se diferencia no preço dos demais tipos.


boxplot(dados$Preco_Kg~dados$Comida)


#Testes 2 a 2
t.test(dados$Preco_Kg[dados$Comida != 3] ~ dados$Preco_Kg[dados$Comida != 3]) # 1 e 2 
t.test(dados$Preco_Kg[dados$Comida != 2] ~ dados$Preco_Kg[dados$Comida != 2]) # 1 e 3
t.test(dados$Preco_Kg[dados$Comida != 1] ~ dados$Preco_Kg[dados$Comida != 1]) # 2 e 3


# 1 - Comida Italiana se diferencia mais em relação as outras
# existem diferêncas entre as categorias.