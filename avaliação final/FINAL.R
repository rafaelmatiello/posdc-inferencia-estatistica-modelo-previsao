###############################
# TRABALHO FINAL - Questão 1  #
###############################


# Questão 1 (3,00) – Considerando os dados apresentados acima, construa os 
setwd('C:\\repositorios\\posdc-inferencia-estatistica-modelo-previsao\\avaliação final');

library(fBasics)
library(car)
library(normtest)

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



##########################################
# e) O tempo de experiência médio é diferente para homens e mulheres?
############################################



# Estatítica descritiva

summary(dados$experiencia) #Geral
summary(dados$experiencia[dados$sexo == 0]) #home
summary(dados$experiencia[dados$sexo == 1]) #mulher

# > summary(dados$experiencia) #Geral
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    9.00   18.00   18.79   27.00   56.00 
# > summary(dados$experiencia[dados$sexo == 0]) #home
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    9.00   18.00   19.05   27.00   52.00 
# > summary(dados$experiencia[dados$sexo == 1]) #mulher
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    9.00   17.00   18.52   27.00   56.00 

# R: Considerando a estatística descritiva a média de experiência geral é 18.79, das mulheres 18.52 e dos homens 19.05.


basicStats(dados$experiencia) #Geral
basicStats(dados$experiencia[dados$sexo == 0]) #home
basicStats(dados$experiencia[dados$sexo == 1]) #mulher

# > basicStats(dados$experiencia) #Geral
# X..dados.experiencia
# nobs                 1289.000000
# NAs                     0.000000
# Minimum                 0.000000
# Maximum                56.000000
# 1. Quartile             9.000000
# 3. Quartile            27.000000
# Mean                   18.789760
# Median                 18.000000
# Sum                 24220.000000
# SE Mean                 0.324846
# LCL Mean               18.152474
# UCL Mean               19.427045
# Variance              136.021758
# Stdev                  11.662837
# Skewness                0.375232
# Kurtosis               -0.675665
# > basicStats(dados$experiencia[dados$sexo == 0]) #home
# X..dados.experiencia.dados.sexo....0
# nobs                                  648.000000
# NAs                                     0.000000
# Minimum                                 0.000000
# Maximum                                52.000000
# 1. Quartile                             9.000000
# 3. Quartile                            27.000000
# Mean                                   19.052469
# Median                                 18.000000
# Sum                                 12346.000000
# SE Mean                                 0.449222
# LCL Mean                               18.170359
# UCL Mean                               19.934579
# Variance                              130.766949
# Stdev                                  11.435338
# Skewness                                0.361129
# Kurtosis                               -0.668325
# > basicStats(dados$experiencia[dados$sexo == 1]) #mulher
# X..dados.experiencia.dados.sexo....1
# nobs                                  641.000000
# NAs                                     0.000000
# Minimum                                 0.000000
# Maximum                                56.000000
# 1. Quartile                             9.000000
# 3. Quartile                            27.000000
# Mean                                   18.524181
# Median                                 17.000000
# Sum                                 11874.000000
# SE Mean                                 0.469683
# LCL Mean                               17.601875
# UCL Mean                               19.446487
# Variance                              141.406055
# Stdev                                  11.891428
# Skewness                                0.392254
# Kurtosis                               -0.690275


# Não temos problemas de Kutose e e assimetria em todos os grupos.


#  Teste shapiro de normalidade, para verificar a normalidade dos dados, considerando:
#  H0 - amostra esta dentro da normalidade
#  H1 - amostra nãoesta dentro da normalidade  


shapiro.test(dados$experiencia[dados$sexo == 0]) #home
shapiro.test(dados$experiencia[dados$sexo == 1]) #mulher

# > shapiro.test(dados$experiencia[dados$sexo == 0]) #home
# 
# Shapiro-Wilk normality test
# 
# data:  dados$experiencia[dados$sexo == 0]
# W = 0.97011, p-value = 3.173e-10
# 
# > shapiro.test(dados$experiencia[dados$sexo == 1]) #mulher
# 
# Shapiro-Wilk normality test
# 
# data:  dados$experiencia[dados$sexo == 1]
# W = 0.96514, p-value = 3.313e-11

# Conclusão: 
# como para homes o P-value= 3.173e-10 < 0.05, Rejeitamos a normalidade para homens.
# como para mulheres o P-value= 3.313e-11 < 0.05, Rejeitamos a normalidade para mulheres.



# Teste de Levene, para verificar a igualdade das variáncias. Hipoteses:
# h0, as variancias são igual
# h1, as variancias são diferentes

leveneTest(dados$experiencia, dados$sexo)

# > leveneTest(dados$experiencia, dados$sexo)
# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value Pr(>F)
# group    1  0.4262  0.514
#       1287  

# Conclusão: Como o p-value(Pr(>F)) 0.514 > 0.05, Aceitamos H0, e rejeitamos H1,
# Logo a variáncia em relação a experiência de homens e mulheres é a mesma




# teste t amostra idependentes
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

t.test(dados$experiencia ~ dados$sexo, var.equals=T)

# > t.test(dados$experiencia ~ dados$sexo, var.equals=T)
# 
# Welch Two Sample t-test
# 
# data:  dados$experiencia by dados$sexo
# t = 0.81284, df = 1283.8, p-value = 0.4165
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.7467444  1.8033208
# sample estimates:
#   mean in group 0 mean in group 1 
# 19.05247        18.52418 

# Considerando o p-value 0.4165 < 0.05, logo rejeitamos HO, a media de experiência dos homens é diferente das mulheres
# A média de experiência dos homens é de 19.05247 é maior que o das mulheres que é 18.52418



# Teste wilcox, ignora a normalidade
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

wilcox.test(dados$experiencia~dados$sexo)
# > wilcox.test(dados$experiencia~dados$sexo)
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  dados$experiencia by dados$sexo
# W = 214429, p-value = 0.3127
# alternative hypothesis: true location shift is not equal to 0


# Considerando o p-value 0.3127 < 0.05, logo rejeitamos HO, a media de experiência dos homens é diferente das mulheres




####################################################################
# f) A idade média dos casados é diferente da idade média dos solteiros?
####################################################################

# Estatítica descritiva

summary(dados$idade) #Geral
summary(dados$idade[dados$est_civil == 0]) #solteira
summary(dados$idade[dados$est_civil == 1]) #casado

# > summary(dados$idade) #Geral
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   29.00   37.00   37.93   47.00   65.00 
# > summary(dados$idade[dados$est_civil == 0]) #solteira
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.00   28.00   36.00   37.15   45.25   65.00 
# > summary(dados$idade[dados$est_civil == 1]) #casado
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 18.0    34.0    42.0    42.1    50.0    65.0  

# R: Considerando a estatística descritiva a média de idade geral é 37.93, das solteiros 37.15 e dos casados 42.1.


basicStats(dados$idade) #Geral
basicStats(dados$idade[dados$est_civil == 0]) #solteira
basicStats(dados$idade[dados$est_civil == 1]) #casado

# > basicStats(dados$idade) #Geral
# X..dados.idade
# nobs           1289.000000
# NAs               0.000000
# Minimum          18.000000
# Maximum          65.000000
# 1. Quartile      29.000000
# 3. Quartile      47.000000
# Mean             37.934833
# Median           37.000000
# Sum           48898.000000
# SE Mean           0.320151
# LCL Mean         37.306758
# UCL Mean         38.562908
# Variance        132.118421
# Stdev            11.494278
# Skewness          0.269270
# Kurtosis         -0.772023
# > basicStats(dados$idade[dados$est_civil == 0]) #solteira
# X..dados.idade.dados.est_civil....0
# nobs                                1084.000000
# NAs                                    0.000000
# Minimum                               18.000000
# Maximum                               65.000000
# 1. Quartile                           28.000000
# 3. Quartile                           45.250000
# Mean                                  37.147601
# Median                                36.000000
# Sum                                40268.000000
# SE Mean                                0.347953
# LCL Mean                              36.464863
# UCL Mean                              37.830340
# Variance                             131.241352
# Stdev                                 11.456062
# Skewness                               0.334386
# Kurtosis                              -0.738434
# > basicStats(dados$idade[dados$est_civil == 1]) #casado
# X..dados.idade.dados.est_civil....1
# nobs                                 205.000000
# NAs                                    0.000000
# Minimum                               18.000000
# Maximum                               65.000000
# 1. Quartile                           34.000000
# 3. Quartile                           50.000000
# Mean                                  42.097561
# Median                                42.000000
# Sum                                 8630.000000
# SE Mean                                0.754550
# LCL Mean                              40.609844
# UCL Mean                              43.585278
# Variance                             116.715925
# Stdev                                 10.803514
# Skewness                               0.023927
# Kurtosis                              -0.690255


# Não temos problemas de Kutose e e assimetria em todos os grupos.


#  Teste shapiro de normalidade, para verificar a normalidade dos dados, considerando:
#  H0 - amostra esta dentro da normalidade
#  H1 - amostra nãoesta dentro da normalidade  


shapiro.test(dados$idade[dados$est_civil == 0]) #solteira
shapiro.test(dados$idade[dados$est_civil == 1]) #casado


# 
# > shapiro.test(dados$idade[dados$est_civil == 0]) #solteira
# 
# Shapiro-Wilk normality test
# 
# data:  dados$idade[dados$est_civil == 0]
# W = 0.97135, p-value = 8.32e-14
# 
# > shapiro.test(dados$idade[dados$est_civil == 1]) #casado
# 
# Shapiro-Wilk normality test
# 
# data:  dados$idade[dados$est_civil == 1]
# W = 0.98779, p-value = 0.07607

# Conclusão: 
# como para solteira o P-value= 8.32e-14 < 0.05, Rejeitamos a normalidade para solteiros.
# como para mulheres o P-value= 0.07607 > 0.05, Não rejeitamos a normalidade para casados



# Teste de Levene, para verificar a igualdade das variáncias. Hipoteses:
# h0, as variancias são igual
# h1, as variancias são diferentes

leveneTest(dados$idade, dados$est_civil)

# > leveneTest(dados$idade, dados$est_civil)
# Levene's Test for Homogeneity of Variance (center = median)
#         Df F value Pr(>F)
# group    1  1.8852   0.17
#       1287 

# Conclusão: Como o p-value(Pr(>F)) 0.17 > 0.05, Aceitamos H0, e rejeitamos H1,
# Logo a variáncia em relação a idade dos casados e solteiros é a mesma




# teste t amostra idependentes
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

t.test(dados$idade ~ dados$est_civil, var.equals=T)

# > t.test(dados$idade ~ dados$est_civil, var.equals=T)
# 
# Welch Two Sample t-test
# 
# data:  dados$idade by dados$est_civil
# t = -5.9572, df = 297.45, p-value = 7.235e-09
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -6.585174 -3.314745
# sample estimates:
#   mean in group 0 mean in group 1 
# 37.14760        42.09756 

# Considerando o p-value 7.235e-09 < 0.05, logo rejeitamos HO, a media de idade dos casados é diferente dos solteiros.
# A média de idade dos casados é de 42.09756  é maior que o dos solteiros que é 37.14760.



# Teste wilcox, ignora a normalidade
# H0 -> as médias são iguais
# H1 -> as médias são diferentes

wilcox.test(dados$idade~dados$est_civil)
# > wilcox.test(dados$idade~dados$est_civil)
# 
# Wilcoxon rank sum test with continuity correction
# 
# data:  dados$idade by dados$est_civil
# W = 82952, p-value = 8.259e-09
# alternative hypothesis: true location shift is not equal to 0


# Considerando o p-value 8.259e-09 < 0.05, logo rejeitamos HO, a media de idade dos casados é diferente dos solteiros.




# Questão 2 (3,00 pontos) – Analisando o banco de dados apresentado, é possível ]
# afirmar que haveria alguma das relações destacadas a seguir? Destaque o grau de
# associação para cada uma das relações apresentadas e verifique se 
# seria significativo.

###############################################################################
# g) Há relação entre Salário e Experiência?
###############################################################################

# correlação
# h0 -> r = 0 (Não a uma associação)
# H1 -> r != 0 (há a uma associação)


shapiro.test(dados$salario) 
shapiro.test(dados$experiencia) 

cor.test(dados$salario, dados$experiencia, method = "pearson")

# > cor.test(dados$salario, dados$experiencia, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  dados$salario and dados$experiencia
# t = 6.3079, df = 1287, p-value = 3.882e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.1197048 0.2256402
# sample estimates:
#       cor 
# 0.1731733

# conclusão: o coeficiênte de correlação foi de 0.1731733 (fraca) com p-value 3.882e-10 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação fraca entre o salario X experiencia




###############################################################################
# h) Há relação entre Salário e tempo de Instrução?
###############################################################################


shapiro.test(dados$salario) 
shapiro.test(dados$instrucao) 

cor.test(dados$salario, dados$instrucao, method = "pearson")

# > cor.test(dados$salario, dados$instrucao, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  dados$salario and dados$instrucao
# t = 18.408, df = 1287, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4121919 0.4986880
# sample estimates:
#      cor 
# 0.456518 


# conclusão: o coeficiênte de correlação foi de 0.456518 (moderado) com p-value 2.2e-16 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação moderada entre o salario X instrução


###############################################################################
# i) Há relação entre Salário e Idade dos indivíduos investigados?
###############################################################################


shapiro.test(dados$salario) 
shapiro.test(dados$idade) 

cor.test(dados$salario, dados$idade, method = "pearson")

# > cor.test(dados$salario, dados$idade, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  dados$salario and dados$idade
# t = 10.767, df = 1287, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2365824 0.3367836
# sample estimates:
#       cor 
# 0.2874694 


# conclusão: o coeficiênte de correlação foi de 0.2874694 (fraca) com p-value 2.2e-16 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação fraca entre o salario X idade


cor.test(dados$salario, dados$idade,  method = "spearman")

# > cor.test(dados$salario, dados$idade,  method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  dados$salario and dados$idade
# S = 235232055, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0.3409942 

# conclusão: o coeficiênte de correlação foi de 0.3409942 (fraca) com p-value 2.2e-16 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação fraca entre o salario X idade



cor.test(dados$salario, dados$idade, method = "kendall")

# > cor.test(dados$salario, dados$idade, method = "kendall")
# 
# Kendall's rank correlation tau
# 
# data:  dados$salario and dados$idade
# z = 12.663, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.2393206 

# conclusão: o coeficiênte de correlação foi de 0.2393206  (fraca) com p-value 2.2e-16 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação fraca entre o salario X idade

###############################################################################
# j) Há relação entre a Experiência e a Idade dos Indivíduos?
###############################################################################


cor.test(dados$experiencia, dados$idade, method = "pearson")

# > cor.test(dados$experiencia, dados$idade, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  dados$experiencia and dados$idade
# t = 144.6, df = 1287, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.9672319 0.9735816
# sample estimates:
#      cor 
# 0.970575 


# conclusão: o coeficiênte de correlação foi de 0.970575 (forte) com p-value 2.2e-16 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação forte entre o experiencia X idade


cor.test(dados$experiencia, dados$idade,  method = "spearman")
# > cor.test(dados$experiencia, dados$idade,  method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  dados$experiencia and dados$idade
# S = 10086682, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.971742 

# conclusão: o coeficiênte de correlação foi de 0.971742 (Forte) com p-value 2.2e-16 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação fraca entre o experiencia X idade



cor.test(dados$experiencia, dados$idade, method = "kendall")

# > cor.test(dados$experiencia, dados$idade, method = "kendall")
# 
# Kendall's rank correlation tau
# 
# data:  dados$experiencia and dados$idade
# z = 46.335, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.8823579 

# conclusão: o coeficiênte de correlação foi de 0.8823579  (Forte) com p-value 2.2e-16 < 0.05 = Alfa, rejeitamos h0.
#Logo, a associação fraca entre o experiencia X idade


##############################################################################
#Questão 3 (4,00 pontos) – O banco de dados descrito acima, foi utilizado para 
#investigar o poder preditivo dos fatores destacados na base sobre o salário. 
#Desta forma apresente o modelo de regressão que ajude a entender a forma como 
#cada uma destas variáveis explicativas explica o salário das pessoas 
#investigadas. Considere o seguinte modelo e responda as questões que seguem:
#  𝑠𝑎𝑙𝑎𝑟𝑖𝑜𝑖= 𝛽0+𝛽1𝑠𝑒𝑥𝑜𝑖
#                    +𝛽2𝑐𝑜𝑟𝑖
#                    +𝛽3𝑒𝑠𝑡𝑐𝑖𝑣𝑖𝑙𝑖
#                    +𝛽4𝑖𝑛𝑠𝑡𝑟𝑢𝑐𝑎𝑜𝑖
#                    +𝛽5𝑒𝑥𝑝𝑒𝑟𝑖𝑒𝑛𝑐𝑖𝑎𝑖
#                    +𝛽6𝑖𝑑𝑎𝑑𝑒𝑖
#                    +𝜀𝑖
#
################################################################################

dados["sexo_mas"]<-NA

for(i in 1:length(dados$sexo)){
  if(dados$sexo[i] == 0 ){
    dados$sexo_mas[i] <- 1
  } else {
    dados$sexo_mas[i] <- 0
  }
}

dados["sexo_fem"]<-NA

for(i in 1:length(dados$sexo)){
  if(dados$sexo[i] == 1 ){
    dados$sexo_fem[i] <- 1
  } else {
    dados$sexo_fem[i] <- 0
  }
}


dados["cor_bra"]<-NA

for(i in 1:length(dados$cor)){
  if(dados$cor[i] == 0 ){
    dados$cor_bra[i] <- 1
  } else {
    dados$cor_bra[i] <- 0
  }
}


dados["cor_nao_bra"]<-NA

for(i in 1:length(dados$cor)){
  if(dados$cor[i] == 1 ){
    dados$cor_nao_bra[i] <- 1
  } else {
    dados$cor_nao_bra[i] <- 0
  }
}


dados["est_civil_sol"]<-NA

for(i in 1:length(dados$est_civil)){
  if(dados$est_civil[i] == 0 ){
    dados$est_civil_sol[i] <- 1
  } else {
    dados$est_civil_sol[i] <- 0
  }
}

dados["est_civil_cas"]<-NA

for(i in 1:length(dados$est_civil)){
  if(dados$est_civil[i] == 1 ){
    dados$est_civil_cas[i] <- 1
  } else {
    dados$est_civil_cas[i] <- 0
  }
}

salario_mod <- lm(dados$salario ~ dados$sexo_mas+dados$sexo_fem +dados$cor_bra+dados$cor_nao_bra +dados$est_civil_sol + dados$est_civil_cas + dados$instrucao + dados$experiencia + dados$idade)

# summary(salario_mod)
# > summary(salario_mod)
# 
# Call:
#   lm(formula = dados$salario ~ dados$sexo_mas + dados$sexo_fem + 
#        dados$cor_bra + dados$cor_nao_bra + dados$est_civil_sol + 
#        dados$est_civil_cas + dados$instrucao + dados$experiencia + 
#        dados$idade)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -20.781  -3.760  -1.044   2.418  50.414 
# 
# Coefficients: (4 not defined because of singularities)
# Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)         -10.72755    1.14623  -9.359  < 2e-16 ***
#   dados$sexo_mas        3.07488    0.36462   8.433  < 2e-16 ***
#   dados$sexo_fem             NA         NA      NA       NA    
#   dados$cor_bra         1.56531    0.50919   3.074  0.00216 ** 
#   dados$cor_nao_bra          NA         NA      NA       NA    
#   dados$est_civil_sol  -1.09598    0.50608  -2.166  0.03052 *  
#   dados$est_civil_cas        NA         NA      NA       NA    
#   dados$instrucao       1.37030    0.06590  20.792  < 2e-16 ***
#   dados$experiencia     0.16661    0.01605  10.382  < 2e-16 ***
#   dados$idade                NA         NA      NA       NA    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.508 on 1283 degrees of freedom
# Multiple R-squared:  0.3233,	Adjusted R-squared:  0.3207 
# F-statistic: 122.6 on 5 and 1283 DF,  p-value: < 2.2e-16




# k) Sabendo que as variáveis 𝛽1, 𝛽2 e 𝛽3 são todas variáveis dummies
# (dicotômicas), qual deveria ser a interpretação dada destes coeficientes 
# sobre o salário?
################################################################################

################################################################################
# l) Qual é o poder explicativo do modelo (percentual de variância explicada)?
################################################################################

################################################################################
# m) De modelo geral, o modelo foi significativo?
################################################################################

################################################################################ 
# n) É possível afirmar que o grau de instrução do indivíduo afeta 
# significativamente no seu salário? A que nível de significância?
################################################################################


################################################################################
# o) É possível afirmar que o estado civil do indivíduo afeta significativamente
# no seu salário? A que nível de significância?
################################################################################


################################################################################
# p) É possível afirmar que a experiência do indivíduo afeta significativamente 
# no seu salário? A que nível de significância?
################################################################################

################################################################################
#q) É possível afirmar que o sexo do indivíduo afeta significativamente no seu 
# salário? A que nível de significância?
################################################################################

################################################################################
# r) Considere o modelo de modo geral, principalmente os sinais de cada um dos 
# coeficientes do modelo de regressão, os resultados fazem sentido 
# conceitualmente para você? Justifique.
################################################################################

