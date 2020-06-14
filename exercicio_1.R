
# Entre no seguinte link:
# https://pt.wikipedia.org/wiki/Eleição_presidencial_no_Brasil_em_2002
# Vá até o tópico RESUMO DAS ELEICOES
# Crie um vetor com o nome dos seis candidatos a presidência

candidatos <- c( "Luiz Inácio Lula da Silva", "José Serra", "Anthony Garotinho", 
                 "Ciro Gomes", "José Maria de Almeida", "Rui Costa Pimenta" )

# Crie um vetor com a sigla do partido de cada candidato

partido <- c( "PT", "PSDB", "PSB", "PPS", "PSTU", "PCO" )

# Crie um vetor com o total de votos de cada candidato
  
votos_candidatos <- c( 39455233, 19705445, 15180097, 
                       10170882, 402236, 38619 )

# Crie um objeto calculando a soma do votos dos candidatos no 1o turno
  
total_votos <- sum( votos_candidatos )

# Crie um vetor com a porcentagem de votos de cada candidato
# fazendo uma operação aritmética entre o objeto votos_candidatos
# e o objeto total_votos

porcentagem_votos <- ( votos_candidatos / total_votos )

# Crie uma matriz que conste uma coluna com o total de votos de cada candidato
# e outra com a porcentagem dos votos de cada candidato

matriz_votos <- matrix( c( votos_candidatos, porcentagem_votos ), byrow = FALSE, nrow = 6 )

# Nomeie as linhas da matriz com o nome dos candidatos

rownames( matriz_votos ) <- candidatos

# Nomeie também as colunas

colnames( matriz_votos ) <- c( "Votos Totais", "Percentual de Votos" )

# Crie um dataframe com o nome dos candidatos, o partido,
# a quantidade de votos e o percentual

eleicao <- data.frame( candidatos, partido, votos_candidatos, porcentagem_votos )

# Crie um vetor lógico, indicado TRUE ou FALSE, com a informacao se
# o candidato foi para o segundo turno

segundo_turno <- c( TRUE, TRUE, FALSE, FALSE, FALSE, FALSE )

# Adicione esta coluna no dataframe

eleicao$segundo_turno = segundo_turno

# Calcule a soma da porcentagem dos dois candidatos que obtiveram mais votos

soma_favoritos <- eleicao$porcentagem_votos[ 1 ] + eleicao$porcentagem_votos[ 2 ]

soma_favoritos

# Exiba as informações do dataframe dos dois candidatos com mais votos

eleicao[ 1:2, ]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# [1] 24 18 31

q <- c(47, 24, 18, 33, 31, 15)
q[ c( 2, 3, 5 ) ]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# Out Nov
#  24   2

x <- c(5, 4, 24, 2)
y <- c("Ago", "Set", "Out", "Nov")
names(x) <- y

x[ c( "Out", "Nov" ) ]

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
# 'data.frame':	2 obs. of  2 variables:
# $ x: Factor w/ 2 levels "d","e": 1 2
# $ y: num  1 4

df <- data.frame( x = factor( c( "d", "e" ) ), y = c( 1, 4 ) )

str(df)

###############################################################################

# Crie a seguinte matriz
#
#       [,1] [,2] [,3]
# [1,]   19   22   25
# [2,]   20   23   26
# [3,]   21   24   27

matriz <- matrix( c( c( 19, 22, 25 ), c( 20, 23, 26 ), c( 21, 24, 27 ) ), byrow = TRUE, nrow = 3 )

###############################################################################

# Se Z é uma matriz 4 por 4, qual é o resultado de Z[1,4] ?

RESPOSTA: o resultado é o elemento na primeira linha, quarta coluna da matriz Z

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#  W3 W4 W1 W2 
#  20 69  5 88 

y <- c(20, 69, 5, 88)
q <- c("W3", "W4", "W1", "W2")

names( y ) <- q

y

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#
#       [,1] [,2]
# [1,]    4    6
# [2,]    3    7
# [3,]    1    8


cbind( c( 4, 3, 1 ), c( 6, 7, 8 ) )

###############################################################################

# Substitua o símbolo de interrogação por um 
# código que retorne o seguinte resultado:
#       [,1] [,2] [,3] [,4]
# [1,]    1    3   13   15
# [2,]    2    4   14   16

x <- 1:4
y <- 13:16

matrix( c( c( x[ c( 1, 2 ) ] ), c( x[ c( 3, 4 ) ] ),
           c( y[ c( 1, 2 ) ] ), c( y[ c( 3, 4 ) ] ) ), 
        byrow = FALSE, nrow = 2 )



###############################################################################

# Crie o seguinte dataframe df
#
# df
#    x  y    z
# 1 17  A  Sep
# 2 37  B  Jul
# 3 12  C  Jun
# 4 48  D  Feb
# 5 19  E  Mar

x <- c( 17, 37, 12, 48, 19 )
y <- c( "A", "B", "C", "D", "E" )
z <- c( "Sep", "Jul", "Jun", "Feb", "Mar" )

df <- data.frame( x, y, z )

# Ainda utilizando o dataframe df,
# qual código produziria o seguinte resultado?
#
#    x  y
# 1 17  A
# 2 37  B
# 3 12  C

df[ 1:3, 1:2 ]

###############################################################################

# Responder o exercício teórico abaixo

Pobreza causa menor rendimento escolar.

Hipótese: uma menor renda impede o acesso à materiais e escolas de qualidade.
          Isso diminui o rendimento e capacidade de aprendizado dos alunos.
          
Operacionalização: variáveis renda, escolaridade e notas

Relaçãp: renda é variável independente. Escolaridade e Notas são variáveis
         dependentes