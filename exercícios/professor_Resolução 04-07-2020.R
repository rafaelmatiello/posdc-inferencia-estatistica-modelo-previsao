# -----------------------------------------------
#  Corre��o dos Exerc�cios de Fixa��o
#  Data: 04/07/2020
# -----------------------------------------------

# Mudando o diret�rio
setwd("C:/Users/Usuario/OneDrive - FURB/FURB - Especializa��es/Data Science/Infer�ncia Estat�stica e Modelos de Previs�o/Exerc�cios Teste de M�dias")

#Pacotes
library(fBasics) # Estat�stica B�sicas
library(car) # Teste de Levene
library(normtest) #Testes de Normalidade

# Quest�o 1

# Banco de Dados
formacao <- read.csv("Avaliacao Formacao.csv", header = T, sep = ";", dec = ",")

#Estat�stica Descritiva
summary(formacao$Conhecimento_Antes) # N�vel de conhecimento Antes
summary(formacao$Conhecimento_Depois) # N�vel de conhecimento Depois

basicStats(formacao$Conhecimento_Antes) # N�vel de conhecimento Antes
basicStats(formacao$Conhecimento_Depois) # N�vel de conhecimento Depois

#Pressupostos
#   Normalidade
#     H0 - A distribui��o dos dados � Normal
#     H1 - A distribui��o dos dados n�o � Normal
shapiro.test(formacao$Conhecimento_Antes)
shapiro.test(formacao$Conhecimento_Depois)
dif <- formacao$Conhecimento_Depois-formacao$Conhecimento_Antes
shapiro.test(dif)
#     Como todos os teste rejeitaram a normalidade, a princ�pio dever�amos fazer um teste N�o-Param�trico.
#     Por�m, o conjunto de dados possui 283 observa��es, podem ser usado a flexibilia��o do pressuposto pelo Teorema Central do Limite.

#Teste t - 1 amostra
#     H0 - As m�dia antes e depois s�o iguais.
#     H1 - As m�dia antes e depois s�o diferentes.
t.test(formacao$Conhecimento_Depois,formacao$Conhecimento_Antes, paired = T)
# Conclus�o: Como p-value = 0,0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, h� diferen�a entre o n�vel de conhecimento antes e depois da forma��o.

# Teste N�o-Param�trico - Wilcoxon
wilcox.test(formacao$Conhecimento_Depois,formacao$Conhecimento_Antes, paired = T)
# Conclus�o: Como o p-value = 0.0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1. Confirmando o teste t.

# Quest�o 2
# Banco de Dados
clima <- read.csv("Clima Organizacional.csv", header = T, sep = ";", dec = ",")

# Item a
#   An�lise do Sal�rio de Homens e Mulheres

#Estat�sticas Descritivas
summary(clima$Salario)
summary(clima$Salario[clima$Sexo==0]) #Sal�rio Homens
summary(clima$Salario[clima$Sexo==1]) #Sal�rio Mulheres

basicStats(clima$Salario)
basicStats(clima$Salario[clima$Sexo==0]) #Sal�rio Homens
basicStats(clima$Salario[clima$Sexo==1]) #Sal�rio Mulheres

#Pressupostos
#   Normalidade
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(clima$Salario[clima$Sexo==0]) #Sal�rio Homens
shapiro.test(clima$Salario[clima$Sexo==1]) #Sal�rio Mulheres
#     Conclus�o: Para ambos os grupos, as distribui��o s�o normais

#   Homodedasticidade - Levene
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(clima$Salario, clima$Sexo)
#     Conclus�o: As vari�ncias dos grupos s�o iguais.

#Teste t - 2 amostras Independentes
#   H0 - As m�dias dos grupos s�o iguais.
#   H1 - As m�dias dos grupos s�o diferentes.
t.test(clima$Salario~clima$Sexo, var.equal=T)
#   Conclus�o: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, h� diferen�a significativa no sal�rio de homens e mulheres
#   Os homens, em m�dia, possuem um sal�rio maior do que as mulheres.

# Item b
#   An�lise do Stress de Homens e Mulheres

#Estat�sticas Descritivas
summary(clima$Stress)
summary(clima$Stress[clima$Sexo==0]) #Sal�rio Homens
summary(clima$Stress[clima$Sexo==1]) #Sal�rio Mulheres

basicStats(clima$Stress)
basicStats(clima$Stress[clima$Sexo==0]) #Sal�rio Homens
basicStats(clima$Stress[clima$Sexo==1]) #Sal�rio Mulheres

#Pressupostos
#   Normalidade
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(clima$Stress[clima$Sexo==0]) #Sal�rio Homens
shapiro.test(clima$Stress[clima$Sexo==1]) #Sal�rio Mulheres
#     Conclus�o: Para ambos os grupos, as distribui��o s�o normais

#   Homodedasticidade - Levene
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(clima$Stress, clima$Sexo)
#     Conclus�o: Pr(>F) (p-value) = 0.9064 > 0.05, N�o Rejeitamos H0.
#     As vari�ncias dos grupos s�o iguais.

#Teste t -2 amostras Independentes
#   H0 - As m�dias dos grupos s�o iguais.
#   H1 - As m�dias dos grupos s�o diferentes.
t.test(clima$Stress~clima$Sexo, var.equal=T)
#   Conclus�o: Como p-value = 0,0000 < 0,05 = alpha, Rejeitamos H0 e aceitamos H1.
#   Logo: O n�vel de Stress entre homens e mulhere � diferentes e em m�dia as mulheres possuem maior n�vel de Stress do que os Homens.

# Testes N�o-Param�tricos da Quest�o
wilcox.test(clima$Salario~clima$Sexo) # Teste de Mann-Whitney
wilcox.test(clima$Stress~clima$Sexo) # Teste de Mann-Whitney

# Quest�o 3

# Banco de Dados
comida <- read.csv("Preco Comida.csv", header = T, sep = ";", dec = ",")

#Estat�sticas Descritivas
summary(comida$Preco_Kg)
summary(comida$Preco_Kg[comida$Comida==1]) #Comida Italiana
summary(comida$Preco_Kg[comida$Comida==2]) #Comida Japonesa
summary(comida$Preco_Kg[comida$Comida==3]) #Churrascaria

basicStats(comida$Preco_Kg)
basicStats(comida$Preco_Kg[comida$Comida==1]) #Comida Italiana
basicStats(comida$Preco_Kg[comida$Comida==2]) #Comida Japonesa
basicStats(comida$Preco_Kg[comida$Comida==3]) #Churrascaria

#Pressupostos
#   Normalidade
#     H0 - A distribui��o � normal
#     H1 - A distribui��o n�o � normal
shapiro.test(comida$Preco_Kg[comida$Comida==1]) #Comida Italiana
shapiro.test(comida$Preco_Kg[comida$Comida==2]) #Comida Japonesa
shapiro.test(comida$Preco_Kg[comida$Comida==3]) #Churrascaria
#     Conclus�o: Para todos os grupos, as distribui��es s�o normais

#   Homodedasticidade - Levene
#     H0 - As vari�ncias s�o iguais
#     H1 - As vari�ncias s�o diferentes
leveneTest(comida$Preco_Kg, comida$Comida)
#     Conclus�o: Pr(>F) (p-value) = 0.2782 > 0.05, N�o Rejeitamos H0.
#     As vari�ncias dos grupos s�o iguais.

#Teste ANOVA
#   H0 - Todas as m�dia s�o iguais
#   H1 - H� pelo menos dois grupos com m�dias diferentes.
anova <- aov(comida$Preco_Kg~comida$Comida)
summary(anova)
#   Conclus�o: Como o p-value (PR(>F))= 0,0000 < 0,05, Rejeitamos H0 e Aceitamos H1.
#   Logo, h�, pelo menos, um tipo de comida que se diferencia no pre�o dos demais tipos.

boxplot(comida$Preco_Kg~comida$Comida)
