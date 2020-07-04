# correlação

setwd('C:\\repositorios\\posdc-inferencia-estatistica-modelo-previsao\\aula3')

peso <- read.csv("peso_altura.csv", header=T, sep=";", dec = ",")


# correlção
# h0 -> r = 0 (Não a uma associação)
# H1 -> r != 0 (há a uma associação)

cor.test(peso$Estatura, peso$Peso, method = "pearson")

# conclusão: o coeficiênte de correlação foi de 0.7270887 (forte)  com p-value 0.05 = Alfa, rejeitamos h0.
#Logo, a associação signoficativa entre o peso X altrula

# > cor.test(peso$Estatura, peso$Peso, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  peso$Estatura and peso$Peso
# t = 4.4932, df = 18, p-value = 0.0002812
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.4195631 0.8848934
# sample estimates:
#       cor 
# 0.7270887 

# quando mais longe de Zero, melhor, mais relacionado.


# correlção
# h0 -> r = 0 (Não a uma associação)
# H1 -> r != 0 (há a uma associação)

cor.test(peso$Estatura, peso$Peso, method = "spearman")
# > cor.test(peso$Estatura, peso$Peso, method = "spearman")
# 
# Spearman's rank correlation rho
# 
# data:  peso$Estatura and peso$Peso
# S = 399.6, p-value = 0.000597
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#      rho 
# 0.699548 

# conclusão: o coeficiênte de correlação foi de 0.699548 (moderada)  com p-value 0.05, rejeitamos h0
#Logo, a associação signoficativa entre o peso X altrula

# correlção
# h0 -> r = 0 (Não a uma associação)
# H1 -> r != 0 (há a uma associação)

cor.test(peso$Estatura, peso$Peso, method = "kendall")
# > cor.test(peso$Estatura, peso$Peso, method = "kendall")
# 
# Kendall's rank correlation tau
# 
# data:  peso$Estatura and peso$Peso
# z = 3.2163, p-value = 0.001299
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
#       tau 
# 0.5414695 



# conclusão: o coeficiênte de correlação foi de 0.5414695 (moderada)  com p-value 0.05, rejeitamos h0
#Logo, a associação signoficativa entre o peso X altrula



# modelo de regressão
# peso = b0 + b1 * estatura + e;

modelo <- lm(peso$Peso ~ peso$Estatura)
# > lm(peso$Peso ~ peso$Estatura)
# 
# Call:
#   lm(formula = peso$Peso ~ peso$Estatura)
# 
# Coefficients:
#   (Intercept)  peso$Estatura  
# -49.4160         0.6719  

# cálculo dos pesos, igual do excel


summary(modelo)
# > summary(modelo)
# 
# Call:
#   lm(formula = peso$Peso ~ peso$Estatura)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6.135 -2.271 -1.119  3.260  7.881 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   -49.4160    24.3047  -2.033 0.057049 .  
# peso$Estatura   0.6719     0.1495   4.493 0.000281 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.403 on 18 degrees of freedom
# Multiple R-squared:  0.5287,	Adjusted R-squared:  0.5025 
# F-statistic: 20.19 on 1 and 18 DF,  p-value: 0.0002812


# residual, estimou para mais porque esta negativo
# standard error: 4.403, calcular o  intervalo de confiança

# R² - percentual de variância explicada
# - quando a variável consegue explicar em relação a variavel dependente

# R² (R-squared) ->  0.528 ( statura explica 52% da variância do peso com base na estatura)
# *** Adjusted R-squared:  0.5025 => se adicionar um novo item vai impactar na relação
# Anova Analytics of variance
# testa a significancia do modelo como um todo.
# H0 -> todo os coeficiêntes são igual a 0
# H1 -> pelo menos um dos coefientes é diferente de 0

# conclusão: como p-value 0.0002812 < 0.05 nivel alfa, rejeitamos h0, assumimos H1,
# Logo, o modelo possui pelo menos 1 coeficiente que ajuda explicar a variável dependente(peso)

#  A Significancia dos betas, 
# H0 - o beta é igual a 0
# H1 - o beta é diferente de 0.
# Conclusões: o beta0 (intercepto) teve p-value Pr(>|t|) 0.057049 .   > 0.05., logo Não rejeitamos H0.
#   Logo beta0 não pode ser assumido diferente de 0.
#   O beta1, teve p-value Pr(>|t|) 0.000281 .   > 0.05., logo rejeitamos H0.
#   Logo beta1 pode ser assumido estatisticamente diferente de 0, indicando existência de efeito diretamente proporcional da estatura no peso.
#   ( cada cm, as mulherem ganham 0.6719 no peso.)




# adl -> grau de dependência
# men -> nível de memória
# cog -> capacidade cognitiva
# kbi = b0(i) + b1_adl(i) + b2_mem(i) + b3_cog(i) + U(i) (erro)

# kbi = varias perguntas e somou as respostas.

idoso <- read.csv("Exemplo_Reg_Mult.csv", header=T, sep=";", dec = ",")


# Analise das corelações

cor.test(idoso$KBI, idoso$ADL, method = "pearson")
# > cor.test(idoso$KBI, idoso$ADL, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  idoso$KBI and idoso$ADL
# t = 4.1001, df = 98, p-value = 8.53e-05
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.2013644 0.5385867
# sample estimates:
#       cor 
# 0.3826482 

# p-value = 8.53e-05
# cor = 0.3826482 


cor.test(idoso$KBI, idoso$MEM, method = "pearson")

# > cor.test(idoso$KBI, idoso$MEM, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  idoso$KBI and idoso$MEM
# t = 5.8104, df = 98, p-value = 7.757e-08
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.3439684 0.6390669
# sample estimates:
#       cor 
# 0.5061876 

# p-value: 7.757e-08
# cor:  0.5061876 


cor.test(idoso$KBI, idoso$COG, method = "pearson")

# > cor.test(idoso$KBI, idoso$COG, method = "pearson")
# 
# Pearson's product-moment correlation
# 
# data:  idoso$KBI and idoso$COG
# t = -2.8447, df = 98, p-value = 0.005413
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.44827947 -0.08433577
# sample estimates:
#       cor 
# -0.276179 

# p-value = 0.005413
# cor = -0.276179 

# negativa, se preservou a capacidade, menor o grau de ocupação.

cor.test(idoso$ADL, idoso$MEM, method = "pearson")
cor.test(idoso$ADL, idoso$COG, method = "pearson")


cor.test(idoso$MEM, idoso$COG, method = "pearson")
# perderam a memória, tende a ter capacidade cognitiva menor


# modelo regração

# variável independente KBI
idoso_mod <- lm(idoso$KBI ~ idoso$ADL+idoso$MEM+idoso$COG)


summary(idoso_mod)


# > summary(idoso_mod)
# 
# Call:
#   lm(formula = idoso$KBI ~ idoso$ADL + idoso$MEM + idoso$COG)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -42.072 -10.441  -1.420   9.261  43.215 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  40.0641    10.0426   3.989 0.000129 ***
#   idoso$ADL     0.2185     0.1161   1.882 0.062847 .  
# idoso$MEM     0.5588     0.1292   4.324 3.74e-05 ***
#   idoso$COG     0.1292     0.2985   0.433 0.666043    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 17.16 on 96 degrees of freedom
# Multiple R-squared:  0.2868,	Adjusted R-squared:  0.2645 
# F-statistic: 12.87 on 3 and 96 DF,  p-value: 3.9e-07


# analise do modelo
# r² - coeficiênte de determinação
# r² -  0.2868, (28% da variação do grau de ocupação é explicado pelas condições fisicas do idoso)

# teste ANOVA - Significancia do modelo como um todo.
#   conclusão: como o p-value = 3.9e-07 (0.0000) é menor que 0.05 = alfa, rejeitamos H0, modelo não significativo.
#               Aceitamos H1 ( modelo significativo)
#             Logo: existe uma coerência, nas variáveis explicativas frente a variável dependente.


# analise dos coeficiêntes
#   Conclusões: 
#             beta0, é sinificativamente diferente de 0, porque  pvalue(Pr(>|t|) ) = 0.000129  que é menor que 0.05
#             beta1, é significativo apenas para alfa igual a 0,1 (10%, legenda ","), logo existe um efeito da pendência do idoso no grau de pependencia do idoso
#             beta2, é significativo (3.74e-05) alfa de 0,05, logo existe efeito de comprometimento da memoria sobre o grau do cuidador
#             beta3, Não significativo, logo não influencia no cuidador. Cognitiva

