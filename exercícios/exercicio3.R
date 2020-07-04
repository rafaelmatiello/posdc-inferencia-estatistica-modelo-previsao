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


sd(dados$Preco_Kg)
sd(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
sd(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
sd(dados$Preco_Kg[dados$Comida ==3]) # grupo 3

# > sd(dados$Preco_Kg)
# [1] 5.894459
# > sd(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# [1] 2.300396
# > sd(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# [1] 3.218888
# > sd(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# [1] 2.687904


# coêfiente de variáção
sd(dados$Preco_Kg) /mean(dados$Preco_Kg)
sd(dados$Preco_Kg[dados$Comida ==1]) / mean(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
sd(dados$Preco_Kg[dados$Comida ==2]) / mean(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
sd(dados$Preco_Kg[dados$Comida ==3]) / mean(dados$Preco_Kg[dados$Comida ==3]) # grupo 3

# > sd(dados$Preco_Kg) /mean(dados$Preco_Kg)
# [1] 0.2536888
# > sd(dados$Preco_Kg[dados$Comida ==1]) / mean(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# [1] 0.1361103
# > sd(dados$Preco_Kg[dados$Comida ==2]) / mean(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# [1] 0.1323199
# > sd(dados$Preco_Kg[dados$Comida ==3]) / mean(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# [1] 0.08860063


# assimetria
skewness(dados$Preco_Kg)
skewness(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
skewness(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
skewness(dados$Preco_Kg[dados$Comida ==3]) # grupo 3


# > skewness(dados$Preco_Kg)
# [1] 0.07564657
# attr(,"method")
# [1] "moment"
# > skewness(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# [1] 0.08302814
# attr(,"method")
# [1] "moment"
# > skewness(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# [1] -0.2567996
# attr(,"method")
# [1] "moment"
# > skewness(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# [1] 0.3117131
# attr(,"method")
# [1] "moment"


kurtosis(dados$Preco_Kg)
kurtosis(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
kurtosis(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
kurtosis(dados$Preco_Kg[dados$Comida ==3]) # grupo 3

# > kurtosis(dados$Preco_Kg)
# [1] -1.002519
# attr(,"method")
# [1] "excess"
# > kurtosis(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
# [1] -0.4802342
# attr(,"method")
# [1] "excess"
# > kurtosis(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
# [1] 0.5500855
# attr(,"method")
# [1] "excess"
# > kurtosis(dados$Preco_Kg[dados$Comida ==3]) # grupo 3
# [1] 0.09213739
# attr(,"method")
# [1] "excess"


# teste de normalidade
#H0 -> a distribuição é normal
#H1 -> a distribuição NÃO é normal

shapiro.test(dados$Preco_Kg)
shapiro.test(dados$Preco_Kg[dados$Comida ==1]) # grupo 1
shapiro.test(dados$Preco_Kg[dados$Comida ==2]) # grupo 2
shapiro.test(dados$Preco_Kg[dados$Comida ==3]) # grupo 3

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
#h1 -> extem 2 grupos com média distintas
ANOVA <- aov(dados$Preco_Kg ~dados$Comida)
summary(ANOVA)
# > summary(ANOVA)
# Df Sum Sq Mean Sq F value Pr(>F)    
# dados$Comida   1   4007    4007   507.1 <2e-16 ***
#   Residuals    148   1170       8                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#consideração: 0 ‘***’ 0.001 é menor que 0.05, então rejeita H0



#Testes 2 a 2
t.test(dados$Preco_Kg[dados$Comida != 3] ~ dados$Preco_Kg[dados$Comida != 3]) # 1 e 2 
t.test(dados$Preco_Kg[dados$Comida != 2] ~ dados$Preco_Kg[dados$Comida != 2]) # 1 e 3
t.test(dados$Preco_Kg[dados$Comida != 1] ~ dados$Preco_Kg[dados$Comida != 1]) # 2 e 3


# 1 - Comida Italiana se diferencia mais em relação as outras
# existem diferêncas entre as categorias.