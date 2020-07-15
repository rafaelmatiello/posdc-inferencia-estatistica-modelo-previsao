###############################
# TRABALHO FINAL - Questão 1  #
###############################

setwd('C:\\repositorios\\posdc-inferencia-estatistica-modelo-previsao\\avaliação final');

library(fBasics)
library(car)
library(normtest)


# Questão 1 (3,00) – Considerando os dados apresentados acima, construa os 
# testes de hipótese dadas as questões apresentadas. Elaborar: 
# (i) A hipótese nula e a alternativa a ser testada; 
# (ii) Destaque o teste que será realizado; 
# (iii) Descreva os pressupostos necessários para cada teste; 
# (iv) Efetue o teste de hipótese pretendido
# (v) Conclua sobre quais os indícios trazidos pelos dados.


################################################################################ 
# a) O Salário médio dos homens se difere do salário médio das mulheres?
# (i) A hipótese nula e a alternativa a ser testada; 
# R: 
#  H0 o média salárial dos homens é igual o das mulhers
#  H1 o média salárial dos homens não é igual o das mulhers

# (ii) Destaque o teste que será realizado; 
# R:

# (iii) Descreva os pressupostos necessários para cada teste; 

# (iv) Efetue o teste de hipótese pretendido

# (v) Conclua sobre quais os indícios trazidos pelos dados.




dados <- read.csv("Dados.csv", header=T, sep=";", dec = ",")

# Estatítica descritiva

summary(dados$salario) #Geral
summary(dados$salario[dados$sexo == 0]) #home
summary(dados$salario[dados$sexo == 1]) #mulher

# > summary(dados$salario) #Geral
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.84    6.92   10.08   12.37   15.63   64.08 
# > summary(dados$salario[dados$sexo == 0]) #home
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.15    8.00   12.00   14.12   18.00   49.46 
# > summary(dados$salario[dados$sexo == 1]) #mulher
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.84    6.00    8.89   10.59   13.45   64.08 

# R: Considerando a estatística descritiva a média salarial geral é 12,37, das mulheres 10,59 e dos homens 12,37.


basicStats(dados$salario) #Geral
basicStats(dados$salario[dados$sexo == 0]) #home
basicStats(dados$salario[dados$sexo == 1]) #mulher


# > basicStats(dados$salario) #Geral
# X..dados.salario
# nobs             1289.000000
# NAs                 0.000000
# Minimum             0.840000
# Maximum            64.080002
# 1. Quartile         6.920000
# 3. Quartile        15.630000
# Mean               12.365849
# Median             10.080000
# Sum             15939.580003
# SE Mean             0.219938
# LCL Mean           11.934374
# UCL Mean           12.797325
# Variance           62.352348
# Stdev               7.896350
# Skewness            1.845964
# Kurtosis            4.824411
# > basicStats(dados$salario[dados$sexo == 0]) #home
# X..dados.salario.dados.sexo....0
# nobs                              648.000000
# NAs                                 0.000000
# Minimum                             1.150000
# Maximum                            49.459999
# 1. Quartile                         8.000000
# 3. Quartile                        18.000000
# Mean                               14.118889
# Median                             12.000000
# Sum                              9149.039995
# SE Mean                             0.330573
# LCL Mean                           13.469763
# UCL Mean                           14.768015
# Variance                           70.812611
# Stdev                               8.415023
# Skewness                            1.426094
# Kurtosis                            2.216991
# > basicStats(dados$salario[dados$sexo == 1]) #mulher
# X..dados.salario.dados.sexo....1
# nobs                              641.000000
# NAs                                 0.000000
# Minimum                             0.840000
# Maximum                            64.080002
# 1. Quartile                         6.000000
# 3. Quartile                        13.450000
# Mean                               10.593666
# Median                              8.890000
# Sum                              6790.540008
# SE Mean                             0.272619
# LCL Mean                           10.058330
# UCL Mean                           11.129002
# Variance                           47.639874
# Stdev                               6.902164
# Skewness                            2.571936
# Kurtosis                           11.303823


# Temos problemas de Kutose e e assimetria em todos os grupos.


#  Teste shapiro de normalidade, para verificar a normalidade dos dados, considerando:
#  H0 - amostra esta dentro da normalidade
#  H1 - amostra nãoesta dentro da normalidade  


shapiro.test(dados$salario[dados$sexo == 0]) #home
shapiro.test(dados$salario[dados$sexo == 1]) #mulher

# > shapiro.test(dados$salario[dados$sexo == 0]) #home
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$sexo == 0]
# W = 0.88172, p-value < 2.2e-16
# 
# > shapiro.test(dados$salario[dados$sexo == 1]) #mulher
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$sexo == 1]
# W = 0.79203, p-value < 2.2e-16

# Conclusão: 
# como para homes o P-value= 2.2e-16 < 0.05, Rejeitamos a normalidade para homens.
# como para mulheres o P-value= 2.2e-16 < 0.05, Rejeitamos a normalidade para mulheres.



# Teste de Levene, para verificar a igualdade das variáncias. Hipoteses:
# h0, as variancias são igual
# h1, as variancias são diferentes

leveneTest(dados$salario, dados$sexo)

# > leveneTest(dados$salario, dados$sexo)
# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value    Pr(>F)    
# group    1  20.747 5.737e-06 ***
#       1287                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Conclusão: Como o p-value(Pr(>F)) 5.737e-06 < 0.05, Rejeiramos H0, e consideramos H1




# teste t amostra idependentes
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

t.test(dados$salario ~ dados$sexo, var.equals=T)

# > t.test(dados$salario ~ dados$sexo, var.equals=T)
# 
# Welch Two Sample t-test
# 
# data:  dados$salario by dados$sexo
# t = 8.2272, df = 1244.4, p-value = 4.784e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   2.684589 4.365857
# sample estimates:
#   mean in group 0 mean in group 1 
# 14.11889        10.59367 

# Considerando o p-value 4.784e-16 < 0.05, logo rejeitamos HO, a media salarial dos homens é diferente das mulheres
# A média do salarial dos homens é de 14.11889 é maior que o das mulheres que é 10.59367



# Teste wilcox, ignora a normalidade
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

wilcox.test(dados$salario~dados$sexo)
# > wilcox.test(dados$salario~dados$sexo)
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  dados$salario by dados$sexo
# W = 267668, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0


# Considerando o p-value 2.2e-16 < 0.05, logo rejeitamos HO, a media salarial dos homens é diferente das mulheres

#############################################################
# b) O Salário médio das pessoas não brancas se difere das pessoas brancas?
#############################################################



# Estatítica descritiva

summary(dados$salario) #Geral
summary(dados$salario[dados$cor == 0]) #branca
summary(dados$salario[dados$cor == 1]) #não-branca

# > summary(dados$salario) #Geral
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.84    6.92   10.08   12.37   15.63   64.08 
# > summary(dados$salario[dados$cor == 0]) #branca
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.84    7.00   11.00   12.79   16.00   64.08 
# > summary(dados$salario[dados$cor == 1]) #não-branca
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.57    6.00    8.00    9.99   12.57   36.05 


# R: Considerando a estatística descritiva a média salarial geral é 12,37, das não-brancas é 12.57 e dos brancas 12.79.


basicStats(dados$salario) #Geral
basicStats(dados$salario[dados$cor == 0]) #branca
basicStats(dados$salario[dados$cor == 1]) #não-branca


# basicStats(dados$salario) #Geral
# X..dados.salario
# nobs             1289.000000
# NAs                 0.000000
# Minimum             0.840000
# Maximum            64.080002
# 1. Quartile         6.920000
# 3. Quartile        15.630000
# Mean               12.365849
# Median             10.080000
# Sum             15939.580003
# SE Mean             0.219938
# LCL Mean           11.934374
# UCL Mean           12.797325
# Variance           62.352348
# Stdev               7.896350
# Skewness            1.845964
# Kurtosis            4.824411
# > basicStats(dados$salario[dados$cor == 0]) #branca
# X..dados.salario.dados.cor....0
# nobs                            1092.000000
# NAs                                0.000000
# Minimum                            0.840000
# Maximum                           64.080002
# 1. Quartile                        7.000000
# 3. Quartile                       16.000000
# Mean                              12.794423
# Median                            11.000000
# Sum                            13971.510004
# SE Mean                            0.246361
# LCL Mean                          12.311029
# UCL Mean                          13.277817
# Variance                          66.277378
# Stdev                              8.141092
# Skewness                           1.809920
# Kurtosis                           4.569325
# > basicStats(dados$salario[dados$cor == 1]) #não-branca
# X..dados.salario.dados.cor....1
# nobs                             197.000000
# NAs                                0.000000
# Minimum                            1.570000
# Maximum                           36.049999
# 1. Quartile                        6.000000
# 3. Quartile                       12.570000
# Mean                               9.990203
# Median                             8.000000
# Sum                             1968.069999
# SE Mean                            0.416211
# LCL Mean                           9.169376
# UCL Mean                          10.811030
# Variance                          34.126654
# Stdev                              5.841802
# Skewness                           1.588035
# Kurtosis                           2.903864


# Temos problemas de assimetria em todos os grupos.


#  Teste shapiro de normalidade, para verificar a normalidade dos dados, considerando:
#  H0 - amostra esta dentro da normalidade
#  H1 - amostra nãoesta dentro da normalidade  

shapiro.test(dados$salario[dados$cor == 0]) 
shapiro.test(dados$salario[dados$cor == 1]) 

# > shapiro.test(dados$salario[dados$cor == 0]) 
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$cor == 0]
# W = 0.84922, p-value < 2.2e-16
# 
# > shapiro.test(dados$salario[dados$cor == 1]) 
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$cor == 1]
# W = 0.85342, p-value = 8.177e-13

# Conclusão: 
# como para homes o P-value= 2.2e-16 < 0.05, Rejeitamos a normalidade para branca
# como para mulheres o P-value= 8.177e-13 < 0.05, Rejeitamos a normalidade para não-branca



# Teste de Levene, para verificar a igualdade das variáncias. Hipoteses:
# h0, as variancias são igual
# h1, as variancias são diferentes

leveneTest(dados$salario, dados$cor)

# > leveneTest(dados$salario, dados$cor)
# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value    Pr(>F)    
# group    1  12.288 0.0004716 ***
#       1287                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Conclusão: Como o p-value(Pr(>F)) 0.0004716 < 0.05, Rejeitamos H0, e consideramos H1




# teste t amostra independentes
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

t.test(dados$salario ~ dados$cor, var.equals=T)

# > t.test(dados$salario ~ dados$cor, var.equals=T)
# 
# Welch Two Sample t-test
# 
# data:  dados$salario by dados$cor
# t = 5.7979, df = 349.69, p-value = 1.501e-08
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   1.852975 3.755465
# sample estimates:
#   mean in group 0 mean in group 1 
# 12.794423        9.990203 

# Considerando o p-value 1.501e-08 < 0.05, logo rejeitamos HO, a media salarial dos Não-branca é diferente das brancas
# A média do salarial dos branca é de 12.794423 é maior que o das não-branca que é 9.990203 



# Teste wilcox, ignora a normalidade
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

wilcox.test(dados$salario~dados$cor)
# > wilcox.test(dados$salario~dados$cor)
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  dados$salario by dados$cor
# W = 131644, p-value = 5.492e-07
# alternative hypothesis: true location shift is not equal to 0

# Considerando o p-value 5.492e-07 < 0.05, logo rejeitamos HO, a media salarial dos Não-branca é diferente das brancas



#############################################################
# C) O Salário médio das pessoas casadas se difere das pessoas solteiras?
#############################################################



# Estatítica descritiva

summary(dados$salario) #Geral
summary(dados$salario[dados$est_civil == 0]) #Solteira
summary(dados$salario[dados$est_civil == 1]) #Casada

# > summary(dados$salario) #Geral
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.84    6.92   10.08   12.37   15.63   64.08 
# > summary(dados$salario[dados$est_civil == 0]) #Solteira
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.84    6.50    9.55   12.01   15.00   64.08 
# > summary(dados$salario[dados$est_civil == 1]) #Casada
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.90   10.00   13.00   14.22   18.00   35.00 


# R: Considerando a estatística descritiva a média salarial geral é 12,37, das solteiros é 12.01 e dos casados 14.22.


basicStats(dados$salario) #Geral
basicStats(dados$salario[dados$est_civil == 0]) #Solteira
basicStats(dados$salario[dados$est_civil == 1]) #Casada


# > basicStats(dados$salario) #Geral
# X..dados.salario
# nobs             1289.000000
# NAs                 0.000000
# Minimum             0.840000
# Maximum            64.080002
# 1. Quartile         6.920000
# 3. Quartile        15.630000
# Mean               12.365849
# Median             10.080000
# Sum             15939.580003
# SE Mean             0.219938
# LCL Mean           11.934374
# UCL Mean           12.797325
# Variance           62.352348
# Stdev               7.896350
# Skewness            1.845964
# Kurtosis            4.824411
# > basicStats(dados$salario[dados$est_civil == 0]) #Solteira
# X..dados.salario.dados.est_civil....0
# nobs                                  1084.000000
# NAs                                      0.000000
# Minimum                                  0.840000
# Maximum                                 64.080002
# 1. Quartile                              6.500000
# 3. Quartile                             15.000000
# Mean                                    12.014880
# Median                                   9.550000
# Sum                                  13024.130002
# SE Mean                                  0.248277
# LCL Mean                                11.527721
# UCL Mean                                12.502039
# Variance                                66.819584
# Stdev                                    8.174325
# Skewness                                 1.985852
# Kurtosis                                 5.175889
# > basicStats(dados$salario[dados$est_civil == 1]) #Casada
# X..dados.salario.dados.est_civil....1
# nobs                                   205.000000
# NAs                                      0.000000
# Minimum                                  2.900000
# Maximum                                 35.000000
# 1. Quartile                             10.000000
# 3. Quartile                             18.000000
# Mean                                    14.221707
# Median                                  13.000000
# Sum                                   2915.450001
# SE Mean                                  0.412172
# LCL Mean                                13.409044
# UCL Mean                                15.034371
# Variance                                34.826597
# Stdev                                    5.901406
# Skewness                                 0.738500
# Kurtosis                                 0.449212


# Temos problemas de assimetria no grupo geral e dos solteiros, no grupo dos 
# casados não temos problemas de assimetria.


#  Teste shapiro de normalidade, para verificar a normalidade dos dados, considerando:
#  H0 - amostra esta dentro da normalidade
#  H1 - amostra nãoesta dentro da normalidade  

shapiro.test(dados$salario[dados$est_civil == 0]) 
shapiro.test(dados$salario[dados$est_civil == 1]) 

# > shapiro.test(dados$salario[dados$est_civil == 0]) 
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$est_civil == 0]
# W = 0.81684, p-value < 2.2e-16
# 
# > shapiro.test(dados$salario[dados$est_civil == 1])
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$est_civil == 1]
# W = 0.96152, p-value = 2.294e-05

# Conclusão: 
# como para homes o P-value= 2.2e-16 < 0.05, Rejeitamos a normalidade para solteiros
# como para mulheres o P-value=2.294e-05 < 0.05, Rejeitamos a normalidade para casados



# Teste de Levene, para verificar a igualdade das variáncias. Hipoteses:
# h0, as variancias são igual
# h1, as variancias são diferentes

leveneTest(dados$salario, dados$est_civil)

# > leveneTest(dados$salario, dados$est_civil)
# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value  Pr(>F)  
# group    1  4.4271 0.03557 *
#       1287                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Conclusão: Como o p-value(Pr(>F)) 0.03557 < 0.05, Rejeitamos H0, e consideramos H1

# teste t amostra independentes
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

t.test(dados$salario ~ dados$est_civil, var.equals=T)

# > t.test(dados$salario ~ dados$est_civil, var.equals=T)
# 
# Welch Two Sample t-test
# 
# data:  dados$salario by dados$est_civil
# t = -4.5863, df = 369.73, p-value = 6.184e-06
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.153006 -1.260648
# sample estimates:
#   mean in group 0 mean in group 1 
# 12.01488        14.22171 

# Considerando o p-value 6.184e-06 < 0.05, logo rejeitamos HO, a media salarial dos casados é diferente dos solteiros
# A média do salarial dos casados é de 14.22171 é maior que o dos solteiros que é 12.01488 



# Teste wilcox, ignora a normalidade
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

wilcox.test(dados$salario~dados$est_civil)
# > wilcox.test(dados$salario~dados$est_civil)
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  dados$salario by dados$est_civil
# W = 77246, p-value = 4.229e-12
# alternative hypothesis: true location shift is not equal to 0

# Considerando o p-value 4.229e-12 < 0.05, logo rejeitamos HO, a media salarial dos casados é diferente dos solteiros



################################################################################
# d) Considerando o sexo das pessoas e seu estado civil conjuntamente, é possível
# afirmar que algumas dos grupos formados possui média salarial diferente dos demais?
################################################################################

dados <- read.csv("Dados.csv", header=T, sep=";", dec = ",")

dados["sexo_est_civil"]<-NA

for(i in 1:length(dados$sexo)){
  if(dados$sexo[i] == 0 & dados$est_civil[i] == 0){
    dados$sexo_est_civil[i] <- 1
  } else if(dados$sexo[i] == 0 & dados$est_civil[i] == 1){
    dados$sexo_est_civil[i] <- 2
  } else if(dados$sexo[i] == 1 & dados$est_civil[i] == 0){
    dados$sexo_est_civil[i] <- 3
  }  else{
    dados$sexo_est_civil[i] <- 4
  }
}


#Definição: sexo_est_civil
# grupo 1 - masculino e solteiro
# grupo 2 - masculino e casado
# grupo 3 - feminino e solteiro
# grupo 4 - feminino e casado


# estatísticas descritivas
summary(dados$salario)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.70   11.28   11.85   11.80   12.43   14.50 


summary(dados$salario[dados$sexo_est_civil == 1]) # grupo 1 - masculino e solteiro
summary(dados$salario[dados$sexo_est_civil == 2]) # grupo 2 - masculino e casado
summary(dados$salario[dados$sexo_est_civil == 3]) # grupo 3 - feminino e solteiro
summary(dados$salario[dados$sexo_est_civil == 4]) # grupo 4 - feminino e casado

# > summary(dados$salario[dados$sexo_est_civil == 1]) # grupo 1 - masculino e solteiro
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.150   7.537  11.410  13.911  17.337  49.460 
# > summary(dados$salario[dados$sexo_est_civil == 2]) # grupo 2 - masculino e casado
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.86   11.00   13.88   15.00   18.00   32.05 
# > summary(dados$salario[dados$sexo_est_civil == 3]) # grupo 3 - feminino e solteiro
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.840   5.817   8.090  10.241  12.985  64.080 
# > summary(dados$salario[dados$sexo_est_civil == 4]) # grupo 4 - feminino e casado
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.90    8.96   12.50   13.03   17.30   35.00  


# Teste de normalidade
#H0 -> a distribuição é normal
#H1 -> a distribuição NÃO é normal

shapiro.test(dados$salario[dados$sexo_est_civil == 1]) # grupo 1 - masculino e solteiro
shapiro.test(dados$salario[dados$sexo_est_civil == 2]) # grupo 2 - masculino e casado
shapiro.test(dados$salario[dados$sexo_est_civil == 3]) # grupo 3 - feminino e solteiro
shapiro.test(dados$salario[dados$sexo_est_civil == 4]) # grupo 4 - feminino e casado


# > shapiro.test(dados$salario[dados$sexo_est_civil == 1]) # grupo 1 - masculino e solteiro
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$sexo_est_civil == 1]
# W = 0.86193, p-value < 2.2e-16
# 
# > shapiro.test(dados$salario[dados$sexo_est_civil == 2]) # grupo 2 - masculino e casado
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$sexo_est_civil == 2]
# W = 0.95456, p-value = 0.0003714
# 
# > shapiro.test(dados$salario[dados$sexo_est_civil == 3]) # grupo 3 - feminino e solteiro
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$sexo_est_civil == 3]
# W = 0.76074, p-value < 2.2e-16
# 
# > shapiro.test(dados$salario[dados$sexo_est_civil == 4]) # grupo 4 - feminino e casado
# 
# Shapiro-Wilk normality test
# 
# data:  dados$salario[dados$sexo_est_civil == 4]
# W = 0.94816, p-value = 0.002512

# conclusão o teste ANOVA, precisa ser analisado com um pouco de cuidade devido a não normalidade dos dados de todos os grupos



plot(dados$salario, dados$sexo_est_civil, xlab="Salário", ylab="Grupos")
abline(v=mean(dados$salario[dados$sexo_est_civil == 1]), col="red")
abline(v=mean(dados$salario[dados$sexo_est_civil == 2]), col="green")
abline(v=mean(dados$salario[dados$sexo_est_civil == 3]), col="blue")
abline(v=mean(dados$salario[dados$sexo_est_civil == 4]), col="orange")



# teste de levene, 
# h0 -> os grupos possuen a mesma variância entre si
# h2 -> os grupos possuen variâncias distintas.

leveneTest(dados$salario, dados$sexo_est_civil)
# > leveneTest(dados$salario, dados$sexo_est_civil)
# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value    Pr(>F)    
# group    3  10.603 6.892e-07 ***
#       1285                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#conclusão: como p-value (pr(>F)) = 6.892e-07 < 0.05 = alfa, rejeita e fica com H1

# conclusão que os grupos possuen variáncias distintas entre si.



# teste ANOVA
# h0 -> os grupos possem médias iguais
# h1 -> existem 2 grupos com média distintas
ANOVA <- aov(dados$salario ~dados$sexo_est_civil)

summary(ANOVA)

# > summary(ANOVA)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# dados$sexo_est_civil    1   2597  2597.1   43.01 7.86e-11 ***
#   Residuals            1287  77713    60.4                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# residuais, variáção dentro dos grupos

# significancia 0 ‘***’ 0.001, então rejeita o H0 e assime H1

# Conclusão: com o p-value = 7.86e-11 < 0.05, então rejeita a H0 e aceita H1


