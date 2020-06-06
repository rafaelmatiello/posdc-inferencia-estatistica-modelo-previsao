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
		




