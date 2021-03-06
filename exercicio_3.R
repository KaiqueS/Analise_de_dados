install.packages( "tidyverse" )
install.packages( "poliscidata" )
install.packages( "scales" )
install.packages( "ggbeeswarm" )

library( tidyverse )
library( poliscidata )
library( scales )
library( ggbeeswarm )

# Utilizando o banco world do pacote poliscidata, fa�a um  
# histograma que tamb�m indique a m�dia  e um boxplot 
# da vari�vel gini10
# Descreva o que voc� pode observar a partir deles.

banco <- world

ggplot( banco, aes( gini10 ) ) +
        geom_histogram( )      +
        geom_vline(aes(xintercept = mean( gini10, na.rm = T ) ) )

# HISTOGRAMA: nos mostra a frequ�ncia de observa��es. Mais especificamente, mostra a frequ�ncia com que cada
#             com que cada valor ou categoria de observa��o acontece. E.g., neste histograma, gini10 = 50
#             teve um n�mero de observa��es inferior � 5. A linha pr�xima ao valor de 40 representa a m�dia
#             no histograma

ggplot( banco, aes( gini10 ) ) +
        geom_boxplot( )

# BOXPLOT: � um tipo de gr�fico que demonstra uma distribui��o de valores. Nele, s�o apresentados as observa��es
#          com valor m�ximo e m�nimo, representadas pelos pontos inicial e final localizados nas extremidades do
#          gr�fico, e a mediana, representada pela linha vertical. Ao lado esquerdo da mediana, na primeira linha
#          vertical, temos a demarca��o do primeiro quartil da distribui��o, onde est�o localizados o final do
#          primeiros 25% de observa��es. Similarmente, na terceira linha, a que vem ap�s a linha mediana, temos o
#          terceiro quartil, marcando os �ltimos 25% de observa��es. Entre o quartil 1 e o quartil 3, temos a
#          regi�o entre quartis, a qual condensa 50% das observa��es. E essa regi�o concentra as observa��es entre
#          valores de gini10 maiores que 30 e menores que 50, tendo uma maior concentr��o de valores acima da
#          mediana.


# Utilizando as fun��es de manipula��o de dados da aula passada,
# fa�a uma tabela que sumarize a media (fun��o mean), 
# mediana (funcao median) e o desvio padr�o (fundao sd) da 
# renda per capta (vari�vel gdppcap08), agrupada por tipo de regime 
# (vari�vel democ).
# Explique a diferen�a entre valores das m�dias e medianas.
# Ilustre a explica��o com gr�fico de boxplot.
# Os dados corroboram a hip�tese da rela��o entre democracia
# e desempenho economico?

banco %>% group_by( democ ) %>%
          summarise( media = mean( gdppcap08, na.rm = T ),
                     mediana = median( gdppcap08, na.rm = T ),
                     desvio = sd( gdppcap08, na.rm = T ) )

# M�DIAS: As m�dias demonstram a soma dos valores dividida pelo n�mero total de observa��es, para cada categoria.
#         Aqui, a categoria No tem m�dia de 9243, a Yes tem m�dia 16351 e a NA tem m�dia 30881. Isso significa que
#         pa�ses democr�ticos tem m�dia de renda per capta maior que pa�ses n�o-democr�ticos

# MEDIANAS: Representam o valor localizado exatamente no meio de todos as observa��es, assumindo-se, � claro, que
#           as observa��es estejam ordenadas. A mediana da renda per caprt, aqui, � maior para pa�ses democr�ticos
#           do que a mediana dos pa�ses n�o democr�ticos.

# Os pontos em Azul representam a m�dia de GDPPCAP08
ggplot( banco, aes( x = gdppcap08, y = democ ) ) +
        geom_boxplot( ) + 
        stat_summary( fun = mean, colour = "blue", geom = "point", 
                     shape = 18, size = 3, show.legend = FALSE )

# PERGUNTA: existe rela��o entre democracia e desempenho econ�mico?
# RESPOSTA: De acordo com os dados, sim, existe. Os pa�ses para os quais a resposta foi Yes, i.e., os pa�ses de-
#           mocr�ticos, tem m�dias e medianas de renda per capta maiores que pa�ses n�o democr�ticos. Al�m de ter,
#           no boxplot, distribui��es com concentra��es maiores de observa��es dentro de valores de renda per
#           capta mais alta.

# Carregue o banco states que est� no pacote poliscidata 
# Mantenha apenas as vari�veis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

banco_states <- states %>% select( obama2012, conpct_m, hs_or_more, prcapinc,
                                   blkpct10, south, religiosity3, state )

glimpse( banco_states )
summary( banco_states )
head( banco_states )
tail( banco_states )
str( banco_states )

# Carregue o banco nes que est� no pacote poliscidata
# Mantenha apenas as vari�veis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state

banco_nes <- nes %>% select( obama_vote, ftgr_cons, dem_educ3, income5, 
                             black, south, relig_imp, sample_state )

glimpse( banco_nes )
summary( banco_nes )
head( banco_nes )
tail( banco_nes )
str( banco_nes )

# As vari�veis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educa��o, renda, cor, norte-sul, 
# religiosidade e estado. A diferen�a � que o nes � um banco de
# dados com surveys individuais e o states � um banco de dados
# com informa��es por estado
#
# Fa�a um gr�fico para cada banco representando o n�vel de
# conservadorismo. Comente as conclus�es que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, voc�s podem ter mais informa��es sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descri��o das
# vari�veis

?states

ggplot( banco_states, aes( x = conpct_m, y = ..count.. ) ) +
        geom_histogram( binwidth = 0.75 )

ggplot( banco_states, aes( x = conpct_m ) ) +
        geom_boxplot( )

?nes
ggplot( banco_nes, aes( x = ftgr_cons, y = ..count.. ) ) +
        geom_histogram( binwidth = 4 )

ggplot( banco_nes, aes( x = ftgr_cons ) ) +
        geom_boxplot( )

# CONCLUS�ES: Segundo os dados, o perfil do eleitorado estadunidense � razoavelmente conservador.
#             Existe uma concentra��o maior de estadunidenses em medidas mais altas de conserva-
#             dorismo.

# Qual � o tipo de gr�fico apropriado para descrever a vari�vel
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gr�ficos

?states
ggplot( banco_states, aes( x = obama2012, y = ..count.. ) ) +
        geom_histogram( )

?nes
ggplot( banco_nes, aes( x = obama_vote, y = ..count.. ) ) +
        geom_bar( )

ggplot( banco_nes, aes( x = obama_vote, y = ..count.. ) ) +
        geom_histogram( )

# RESPOSTA: Para o banco banco_states, o melhor tipo de gr�fico � o histograma, dado que ele apresenta
#           as propor��es de voto para cada estado. J� no banco banco_nes, tanto um histograma quanto
#           um gr�fico de barras seriam apropriados, dado que representariam a mesma informa��o, qual
#           seja, a propor��o de eleitores que votaram ou que n�o votaram em Obama. Por�m, o gr�fico
#           de barras cobriria o espa�o vazio que ocorre no histograma como consequ�ncia da codifica��o
#           da vari�vel, ent�o, talvez seja visualmente melhor.

# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com n�o-negros. A partir disso, fa�a
# dois gr�ficos com a propor��o de votos no obama.
# O que voc� pode afirmar a partir dos gr�ficos?
# Voc� diria que existe uma rela��o entre voto em Obama e cor?

banco_nes_black <- banco_nes %>% filter( black == "Yes" )
banco_nes_not_black <- banco_nes %>% filter( black == "No" )

summary( banco_nes_black )
summary( banco_nes_not_black )

ggplot( banco_nes_black, aes( x = obama_vote, y = ..count.. ) ) +
        geom_bar( )

ggplot( banco_nes_not_black, aes( x = obama_vote, y = ..count.. ) ) +
        geom_bar( )

# RESPOSTA: O Banco de dados que n�o inclui pessoas negras demonstra have uma propor��o equilibrada
#           entre o n�mero de pessoas que votaram e o n�mero de pessoas que n�o votaram em Obama.
#           Por�m, no Banco de dados que inclui pessoas negras, percebemos um desequil�brio enorme
#           entre a propor��o de pessoas que votaram e as pessoas que n�o votaram em Obama. No caso,
#           no banco de dados que inclui pessoas negras, percebemos uma quantidade absurda de pessoas
#           que votaram em Obama. Ent�o, sim existe uma rela��o entre cor e votar ou n�o em Obama.

# A partir do banco de dados states, fa�a uma compara��o semelhante.
# Fa�a um gr�fico com as porcentagens de votos em Obama para estados
# que est�o acima da mediana da porcentagem de popula��o negra nos estados,
# e outro gr�fico com as porcentagens de votos em Obama para os estados
# que est�o abaixo da mediana de popula��o negra.
# O que voc� pode afirmar a partir dos gr�ficos?
# Podemos chegar a mesma conclus�o anterior?

# Banco e Grafico Acima da Mediana
banco_acima_mediana <- banco_states %>% filter( blkpct10 > median( blkpct10 ) )

ggplot( banco_acima_mediana, aes( obama2012, y = ..count.. / sum( ..count.. ) ) ) +
        geom_histogram( )

ggplot( banco_acima_mediana, aes( obama2012 ) ) +
        geom_boxplot( )

summary( banco_acima_mediana )

# Banco e Grafico Abaixo da Mediana
banco_abaixo_mediana <- banco_states %>% filter( blkpct10 < median( blkpct10 ) )

ggplot( banco_abaixo_mediana, aes( obama2012, y = ..count.. / sum( ..count.. ) ) ) +
        geom_histogram( )

ggplot( banco_abaixo_mediana, aes( obama2012 ) ) +
        geom_boxplot( )

summary( banco_abaixo_mediana )

# RESPOSTA: Aqui n�o podemos manter a mesma afirma��o que na situa��o anterior. Isto se d� pois
#           houve uma aproxima��o muito forte de comportamento entre as popula��es, de modo que
#           tanto no banco de dados com porcentagem de negros acima da mediana, quanto no banco
#           com porcentagem abaixo da mediana, os valores, ou o n�mero de pessoas que votaram
#           em Obama foi muito similar, de modo que n�o d� para afirmar que h� uma rela��o muito
#           grande entre cor e votar ou n�o em Obama.

# A partir da var�avel X do banco df abaixo

df <- data.frame( x = cos( seq( -50, 50, 0.5 ) ) )

# x � uma vari�vel n�mero cont�nua
str( cos( seq( -50, 50, 0.5 ) ) )

# Fa�a os tipos de gr�ficos que representem esse tipo de vari�vel
# comentando as diferen�as entre elas e qual seria a mais adequada

ggplot( df, aes( x ) ) +
        geom_histogram( )

ggplot( df, aes( x ) ) +
        geom_density( )

ggplot( df, aes( x ) ) +
        geom_boxplot( )

ggplot( df, aes( x ) ) +
        geom_bar( )

ggplot( df, aes( x, "" ) ) +
        geom_violin( )
        
ggplot( df, aes( "", x ) ) +
        geom_beeswarm( )

# RESPOSTA: Como mostrado em str( cos( seq( -50, 50, 0.5. ) ) ), x � uma vari�vel cont�nua, i.e.,
#           ela representa n�meros com decimais. Tanto o histograma quanto o gr�fico de barras e
#           o beeswarm contam a frequ�nciade ocorr�ncia de certos valores, enquanto que boxplot,
#           violin e density apresentam de maneira distinta a densidade da distribui��o desses
#           valores. density() apresenta uma curva que descreve a distribui��o dos valores. Enquanto
#           o boxplot e violin apresentam como est�o concentrados esses valores na distribui��o.
#           Neste caso, dado o tipo dos dados, i.e., n�meros reais/continuos, a curva de densidade,
#           beeswarm() e um histograma seriam os melhores representantes visuais dos dados, j� que
#           exibem de maneira clara n�o s� os valores dos mesmos, como tamb�m a concentra��o, i.e.,
#           distribui��o de cada valor em rela��o aos outros.

# responsa as quest�es te�ricas abaixo