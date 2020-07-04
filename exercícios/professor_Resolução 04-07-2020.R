# -----------------------------------------------
#  Correção dos Exercícios de Fixação
#  Data: 04/07/2020
# -----------------------------------------------

# Mudando o diretório
setwd("C:/Users/Usuario/OneDrive - FURB/FURB - Especializações/Data Science/Inferência Estatística e Modelos de Previsão/Exercícios Teste de Médias")

#Pacotes
library(fBasics) # Estatística Básicas
library(car) # Teste de Levene
library(normtest) #Testes de Normalidade

# Questão 1

# Banco de Dados
formacao <- read.csv("Avaliacao Formacao.csv", header = T, sep = ";", dec = ",")

#Estatística Descritiva
summary(formacao$Conhecimento_Antes) # Nível de conhecimento Antes
summary(formacao$Conhecimento_Depois) # Nível de conhecimento Depois

basicStats(formacao$Conhecimento_Antes) # Nível de conhecimento Antes
basicStats(formacao$Conhecimento_Depois) # Nível de conhecimento Depois

#Pressupostos
#   Normalidade
#     H0 - A distribuição dos dados é Normal
#     H1 - A distribuição dos dados não é Normal
shapiro.test(formacao$Conhecimento_Antes)
shapiro.test(formacao$Conhecimento_Depois)
dif <- formacao$Conhecimento_Depois-formacao$Conhecimento_Antes
shapiro.test(dif)
#     Como todos os teste rejeitaram a normalidade, a princípio deveríamos fazer um teste Não-Paramétrico.
#     Porém, o conjunto de dados possui 283 observações, podem ser usado a flexibiliação do pressuposto pelo Teorema Central do Limite.

#Teste t - 1 amostra
#     H0 - As média antes e depois são iguais.
#     H1 - As média antes e depois são diferentes.
t.test(formacao$Conhecimento_Depois,formacao$Conhecimento_Antes, paired = T)
# Conclusão: Como p-value = 0,0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1.
# Logo, há diferença entre o nível de conhecimento antes e depois da formação.

# Teste Não-Paramétrico - Wilcoxon
wilcox.test(formacao$Conhecimento_Depois,formacao$Conhecimento_Antes, paired = T)
# Conclusão: Como o p-value = 0.0000 < 0.05 = alpha, Rejeitamos H0 e Aceitamos H1. Confirmando o teste t.

# Questão 2
# Banco de Dados
clima <- read.csv("Clima Organizacional.csv", header = T, sep = ";", dec = ",")

# Item a
#   Análise do Salário de Homens e Mulheres

#Estatísticas Descritivas
summary(clima$Salario)
summary(clima$Salario[clima$Sexo==0]) #Salário Homens
summary(clima$Salario[clima$Sexo==1]) #Salário Mulheres

basicStats(clima$Salario)
basicStats(clima$Salario[clima$Sexo==0]) #Salário Homens
basicStats(clima$Salario[clima$Sexo==1]) #Salário Mulheres

#Pressupostos
#   Normalidade
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(clima$Salario[clima$Sexo==0]) #Salário Homens
shapiro.test(clima$Salario[clima$Sexo==1]) #Salário Mulheres
#     Conclusão: Para ambos os grupos, as distribuição são normais

#   Homodedasticidade - Levene
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(clima$Salario, clima$Sexo)
#     Conclusão: As variâncias dos grupos são iguais.

#Teste t - 2 amostras Independentes
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(clima$Salario~clima$Sexo, var.equal=T)
#   Conclusão: Como p-value - 0,0000 < 0,05 = alpha, Rejeitamos H0 e Aceitamos H1.
#   Logo, há diferença significativa no salário de homens e mulheres
#   Os homens, em média, possuem um salário maior do que as mulheres.

# Item b
#   Análise do Stress de Homens e Mulheres

#Estatísticas Descritivas
summary(clima$Stress)
summary(clima$Stress[clima$Sexo==0]) #Salário Homens
summary(clima$Stress[clima$Sexo==1]) #Salário Mulheres

basicStats(clima$Stress)
basicStats(clima$Stress[clima$Sexo==0]) #Salário Homens
basicStats(clima$Stress[clima$Sexo==1]) #Salário Mulheres

#Pressupostos
#   Normalidade
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(clima$Stress[clima$Sexo==0]) #Salário Homens
shapiro.test(clima$Stress[clima$Sexo==1]) #Salário Mulheres
#     Conclusão: Para ambos os grupos, as distribuição são normais

#   Homodedasticidade - Levene
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(clima$Stress, clima$Sexo)
#     Conclusão: Pr(>F) (p-value) = 0.9064 > 0.05, Não Rejeitamos H0.
#     As variâncias dos grupos são iguais.

#Teste t -2 amostras Independentes
#   H0 - As médias dos grupos são iguais.
#   H1 - As médias dos grupos são diferentes.
t.test(clima$Stress~clima$Sexo, var.equal=T)
#   Conclusão: Como p-value = 0,0000 < 0,05 = alpha, Rejeitamos H0 e aceitamos H1.
#   Logo: O nível de Stress entre homens e mulhere é diferentes e em média as mulheres possuem maior nível de Stress do que os Homens.

# Testes Não-Paramétricos da Questão
wilcox.test(clima$Salario~clima$Sexo) # Teste de Mann-Whitney
wilcox.test(clima$Stress~clima$Sexo) # Teste de Mann-Whitney

# Questão 3

# Banco de Dados
comida <- read.csv("Preco Comida.csv", header = T, sep = ";", dec = ",")

#Estatísticas Descritivas
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
#     H0 - A distribuição é normal
#     H1 - A distribuição não é normal
shapiro.test(comida$Preco_Kg[comida$Comida==1]) #Comida Italiana
shapiro.test(comida$Preco_Kg[comida$Comida==2]) #Comida Japonesa
shapiro.test(comida$Preco_Kg[comida$Comida==3]) #Churrascaria
#     Conclusão: Para todos os grupos, as distribuições são normais

#   Homodedasticidade - Levene
#     H0 - As variâncias são iguais
#     H1 - As variâncias são diferentes
leveneTest(comida$Preco_Kg, comida$Comida)
#     Conclusão: Pr(>F) (p-value) = 0.2782 > 0.05, Não Rejeitamos H0.
#     As variâncias dos grupos são iguais.

#Teste ANOVA
#   H0 - Todas as média são iguais
#   H1 - Há pelo menos dois grupos com médias diferentes.
anova <- aov(comida$Preco_Kg~comida$Comida)
summary(anova)
#   Conclusão: Como o p-value (PR(>F))= 0,0000 < 0,05, Rejeitamos H0 e Aceitamos H1.
#   Logo, há, pelo menos, um tipo de comida que se diferencia no preço dos demais tipos.

boxplot(comida$Preco_Kg~comida$Comida)
