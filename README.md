# posdc-inferencia-estatistica-modelo-previsao

Disciplina: Inferência estatística e modelo de previsão.
Data: 06/06/2020
Professor: Moacir Manoel Rodrigues Junior

## 
- taxa média dos últimos 5 dias
- plato, patamar superior da curva

## Inferências estatística (indução estatística)

 - O processo de cálculo e controle do erro da estimativa
 - pegar a população, processar uma amostra, gerar um parâmetro estimado, 
	- Exemplo: população do brasil, pegar um amostra, chega na média salarial.
 - Indução, seria partir de uma parte, para tirar conclusão com base no todo.
 - (inferencia)[aula1/inferencia.png]
 - Quanto mais aleatoriedade na captação de dados, melhor o resultado, qualquer direção para alterar.
 - Estimador ou estatística
	- função
	- qualquer variável aleatória, função dos elementos amostrais.
	- estimados da média, todos os elementos(valores) usandos para calcular somados divido pela quantidade., é o estimador da média.
 - Estimativa
	- Estimativa pontual
		- uma valor para representar a população.
		- ex.: media salarial brasileira.
	- Estimativa intervalar
		- intervalo que contenha o valor  referencia da amostra.
		- com probabilidade de % o valor esta dentro deste intervalo
		- nunca será o valor pontual, vai estar próximo.
		- ex.: intervalor que esta a média salarial
		- Significancia, propabilidade de erro
		- Confiação: propabilidade de acerto.
		- erro estimado é s / raid(n)
		-  (inferencia1)[aula1/inferencia1.png]
	- (estimativa)[aula1/estimativa.png]
	- Nível de erro é igual a margem de erro.
	- Tabela distribuição t de student, é a margem confiaça que deseja
		- p^ maior erro é 50%
		- erro padrão máxima em uma estimativa de 50%
			- Nível de confiança
			- O t é um valor calculado com o nível de confiança que tu quer
			- Tu faz a conta do nível de confiança que tu quer pra tua pesquisa, exemplo se for 5%, é 5% dividido por 2. Que dá 0,025 que é igual ao módulo de 0,975
			- Aí tu consulta na tabela o t calculado para 0,975
			- Eu não entendi a parte que ele pega o último valor só da coluna, eu não sei o que são os de cima
			- O 1,96 que ele tá pegando é justamente relacionado a 5% de confiança
			- isso, porque a última coluna, é a quantidade de amostras ou população
			- Entendi, faz sentido
			- Ele pegou o último, pq era um cálculo infinito
	- Verificar a amostra esta em um tamanho coerente
		- qualidade da água, quantos frascos devem ser coletados para garantir.
 - Teste de hipótese
	- Questionamentos para indentificar o que é alguma coisa?
	- exemplo pó branco na mesa
	- parametricos, tipo de testes utilizados para validar a amostra
		- pó branco, ser veneno? cocaina? sal? açucar?
		- deve seguir um distribuição normal
		- quando há normalidade nos dados.
	- Não parâmetricos, quando não consegue colocar parâmentros para identificar
		- Não segue um distribuição normal
		- quando não há normalidade nos dados.
	- O objetivo do teste de hipóteses é fornecer um método que permita verificar se os dados amostrais trazem evidências que apoiam ou não uma hipótese formulada
	- Hipótese nula
		- ex.: pingou o regeagente de cocaida, e não ficou azul, afasta a hipotese de cocaina.
		- uma afirmação possível de ser afastada.
		- parâmetro hipostético
			- imc para fazer a cirurgia
	- parâmetro real
			- media de pesos de todos os professores.
	- hipótese alternativa.
			- será utilizada se a nula for rejeitada.
			- teste bi-caudal, indiferente se esta acima ou a abaixa
	- Hipótese
			- Se é maior ou menor que a hipotese
			- ex.: peso medio é diferente de 90 kilos.
- Erros
	- opções: 
			- rejeitar
				- rejeitou mas era verdadeira, tipo 1
			- Não rejeitar
				- Não rejeito e era falsa
				- Não pode estimar com precisão
				- Erro diminiu conforme aumenta a amostra.
				- diminiu conforme alfa diminui
- Tesde de hipóteses
	- Passos para um teste de hipótese computacionamente
		- 1 Definir a hipótese Nula a ser testada e a Alternativa (Teste bi-caudal ou unicaldal).
		- 2 Nivél de significancia alfa	
			- Erro tipo 1 permitido. ex.: 5%
		- 3 - Escolher a estatística de teste
			- escolher o teste
		- 4 - Retirar uma amostra e calcular o valor
		- 5 - determinar o p-value (valor de probabilidade), corresponde a propabilidade em relação a amostra
		- 6 se o p-value-> se menor que o alfa passo 2, regeitar H0 e assumir h1, como verdadeira, caso contrário rejeira
			ex.: a media de peso na furb, é de 90 kg, 
				h0 -> média de 90 kb com 5% de tolerancia.
				

## Inferência Estatística

	- Paramétricos e Não paramétricos -> distribuição dos dados é normal ou não?
		- se for normal usa testes parâmetricos
		- se não for normal usar Não parâmetricos
	- Teste de normalizadade de kolmogorov-smornov - KS
		- Associada a normal
		- compara se a observação é similar a teórica.
		- hipotese nula, provem de um população normal.
			o que pode afastar
		- hi, alternativa, não vem de uma normal
		- Segue um estatística própria.
		- problema:
			- Número de observações menor que 30
			- Múmero de observações muito grande mais de 100, 150
			- até uns 70 a 80 casos
	- TEste de shapiro-wilk
		- Melhor para amostras de até 30 casos
		- h0 -> provem da normal
		- h1 -> provem da não normal
		- estatatística própria
	- Tes de normalidade de Jarque-bera
		- compara a assimetria e Curtose
		- assimetrica, media dos lados iguais
		- curtose, media muito densa ou pouco densa
	- Teste do Levene de homogeneidade de variâncias.
		- comparação a esta variáncia, formato da curva mais fechada ou aberta
		- h0 -> todas as veriáncias homogeneas
		- h1 -> ao meno 1 não homogeneas
		- 
		
	- The p-value is a number between 0 and 1 and interpreted in the following way:
		- A small p-value (typically ≤ 0.05) indicates strong evidence against the null hypothesis, so you reject the null hypothesis.
		- A large p-value (> 0.05) indicates weak evidence against the null hypothesis, so you fail to reject the null hypothesis.
		- p-values very close to the cutoff (0.05) are considered to be marginal (could go either way). Always report the p-value so your readers can draw their own conclusions.
		
## aula 2
20-06-2020

Teste de hipóteses 
 - Amostras pareadas
	- amostra antes do evento
	- amostra depois do evento.
	
## Analytisis of Variance (anova)
	- teste a diferente enter as médias populacionais de k grupos
		- ex.: k >= 3
	- Hipóteses:
		- h0: média dos groups são iguais
		- h1: pelo menos uma população com média diferente.
		- Não tem unicaudal
	- asimir alfa
	- exige-se normalidade para as duas populações, 
	- homogeniedade das variâncias
	
# não paramétricos
	- Não há suposição de ciência quando a distribuição de probabilidade da população.
	
## teste Qui-quadrado
	- Qualitativa X qualitativa, de duas ou mais categorias.
	- 

