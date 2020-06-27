install.packages( "tidyverse" )
install.packages( "poliscidata" )
install.packages( "scales" )
install.packages( "ggbeeswarm" )

library( tidyverse )
library( poliscidata )
library( scales )
library( ggbeeswarm )

# Utilizando o banco world do pacote poliscidata, faça um  
# histograma que também indique a média  e um boxplot 
# da variável gini10
# Descreva o que você pode observar a partir deles.

banco <- world

ggplot( banco, aes( gini10 ) ) +
        geom_histogram( )      +
        geom_vline(aes(xintercept = mean( gini10, na.rm = T ) ) )

# HISTOGRAMA: nos mostra a frequência de observações. Mais especificamente, mostra a frequência com que cada
#             com que cada valor ou categoria de observação acontece. E.g., neste histograma, gini10 = 50
#             teve um número de observações inferior à 5. A linha próxima ao valor de 40 representa a média
#             no histograma

ggplot( banco, aes( gini10 ) ) +
        geom_boxplot( )

# BOXPLOT: É um tipo de gráfico que demonstra uma distribuição de valores. Nele, são apresentados as observações
#          com valor máximo e mínimo, representadas pelos pontos inicial e final localizados nas extremidades do
#          gráfico, e a mediana, representada pela linha vertical. Ao lado esquerdo da mediana, na primeira linha
#          vertical, temos a demarcação do primeiro quartil da distribuição, onde estão localizados o final do
#          primeiros 25% de observações. Similarmente, na terceira linha, a que vem após a linha mediana, temos o
#          terceiro quartil, marcando os últimos 25% de observações. Entre o quartil 1 e o quartil 3, temos a
#          região entre quartis, a qual condensa 50% das observações. E essa região concentra as observações entre
#          valores de gini10 maiores que 30 e menores que 50, tendo uma maior concentrção de valores acima da
#          mediana.


# Utilizando as funções de manipulação de dados da aula passada,
# faça uma tabela que sumarize a media (função mean), 
# mediana (funcao median) e o desvio padrão (fundao sd) da 
# renda per capta (variável gdppcap08), agrupada por tipo de regime 
# (variável democ).
# Explique a diferença entre valores das médias e medianas.
# Ilustre a explicação com gráfico de boxplot.
# Os dados corroboram a hipótese da relação entre democracia
# e desempenho economico?

banco %>% group_by( democ ) %>%
          summarise( media = mean( gdppcap08, na.rm = T ),
                     mediana = median( gdppcap08, na.rm = T ),
                     desvio = sd( gdppcap08, na.rm = T ) )

# MÉDIAS: As médias demonstram a soma dos valores dividida pelo número total de observações, para cada categoria.
#         Aqui, a categoria No tem média de 9243, a Yes tem média 16351 e a NA tem média 30881. Isso significa que
#         países democráticos tem média de renda per capta maior que países não-democráticos

# MEDIANAS: Representam o valor localizado exatamente no meio de todos as observações, assumindo-se, é claro, que
#           as observações estejam ordenadas. A mediana da renda per caprt, aqui, é maior para países democráticos
#           do que a mediana dos países não democráticos.

# Os pontos em Azul representam a média de GDPPCAP08
ggplot( banco, aes( x = gdppcap08, y = democ ) ) +
        geom_boxplot( ) + 
        stat_summary( fun = mean, colour = "blue", geom = "point", 
                     shape = 18, size = 3, show.legend = FALSE )

# PERGUNTA: existe relação entre democracia e desempenho econômico?
# RESPOSTA: De acordo com os dados, sim, existe. Os países para os quais a resposta foi Yes, i.e., os países de-
#           mocráticos, tem médias e medianas de renda per capta maiores que países não democráticos. Além de ter,
#           no boxplot, distribuições com concentrações maiores de observações dentro de valores de renda per
#           capta mais alta.

# Carregue o banco states que está no pacote poliscidata 
# Mantenha apenas as variáveis obama2012, conpct_m, hs_or_more,
# prcapinc, blkpct10, south, religiosity3, state

banco_states <- states %>% select( obama2012, conpct_m, hs_or_more, prcapinc,
                                   blkpct10, south, religiosity3, state )

glimpse( banco_states )
summary( banco_states )
head( banco_states )
tail( banco_states )
str( banco_states )

# Carregue o banco nes que está no pacote poliscidata
# Mantenha apenas as variáveis obama_vote, ftgr_cons, dem_educ3,
# income5, black, south, relig_imp, sample_state

banco_nes <- nes %>% select( obama_vote, ftgr_cons, dem_educ3, income5, 
                             black, south, relig_imp, sample_state )

glimpse( banco_nes )
summary( banco_nes )
head( banco_nes )
tail( banco_nes )
str( banco_nes )

# As variáveis medem os mesmos conceitos, voto no obama em 2012, 
# conservadorismo, educação, renda, cor, norte-sul, 
# religiosidade e estado. A diferença é que o nes é um banco de
# dados com surveys individuais e o states é um banco de dados
# com informações por estado
#
# Faça um gráfico para cada banco representando o nível de
# conservadorismo. Comente as conclusões que podemos ter
# a partir deles sobre o perfil do eleitorado estadunidense.
# Para ajudar, vocês podem ter mais informações sobre os bancos
# de dados digitando ?states e ?nes, para ter uma descrição das
# variáveis

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

# CONCLUSÕES: Segundo os dados, o perfil do eleitorado estadunidense é razoavelmente conservador.
#             Existe uma concentração maior de estadunidenses em medidas mais altas de conserva-
#             dorismo.

# Qual é o tipo de gráfico apropriado para descrever a variável
# de voto em obama nos dois bancos de dados?
# Justifique e elabore os gráficos

?states
ggplot( banco_states, aes( x = obama2012, y = ..count.. ) ) +
        geom_histogram( )

?nes
ggplot( banco_nes, aes( x = obama_vote, y = ..count.. ) ) +
        geom_bar( )

ggplot( banco_nes, aes( x = obama_vote, y = ..count.. ) ) +
        geom_histogram( )

# RESPOSTA: Para o banco banco_states, o melhor tipo de gráfico é o histograma, dado que ele apresenta
#           as proporções de voto para cada estado. Já no banco banco_nes, tanto um histograma quanto
#           um gráfico de barras seriam apropriados, dado que representariam a mesma informação, qual
#           seja, a proporção de eleitores que votaram ou que não votaram em Obama. Porém, o gráfico
#           de barras cobriria o espaço vazio que ocorre no histograma como consequência da codificação
#           da variável, então, talvez seja visualmente melhor.

# Crie dois bancos de dados a partir do banco nes, um apenas com
# respondentes negros e outro com não-negros. A partir disso, faça
# dois gráficos com a proporção de votos no obama.
# O que você pode afirmar a partir dos gráficos?
# Você diria que existe uma relação entre voto em Obama e cor?

banco_nes_black <- banco_nes %>% filter( black == "Yes" )
banco_nes_not_black <- banco_nes %>% filter( black == "No" )

summary( banco_nes_black )
summary( banco_nes_not_black )

ggplot( banco_nes_black, aes( x = obama_vote, y = ..count.. ) ) +
        geom_bar( )

ggplot( banco_nes_not_black, aes( x = obama_vote, y = ..count.. ) ) +
        geom_bar( )

# RESPOSTA: O Banco de dados que não inclui pessoas negras demonstra have uma proporção equilibrada
#           entre o número de pessoas que votaram e o número de pessoas que não votaram em Obama.
#           Porém, no Banco de dados que inclui pessoas negras, percebemos um desequilíbrio enorme
#           entre a proporção de pessoas que votaram e as pessoas que não votaram em Obama. No caso,
#           no banco de dados que inclui pessoas negras, percebemos uma quantidade absurda de pessoas
#           que votaram em Obama. Então, sim existe uma relação entre cor e votar ou não em Obama.

# A partir do banco de dados states, faça uma comparação semelhante.
# Faça um gráfico com as porcentagens de votos em Obama para estados
# que estão acima da mediana da porcentagem de população negra nos estados,
# e outro gráfico com as porcentagens de votos em Obama para os estados
# que estão abaixo da mediana de população negra.
# O que você pode afirmar a partir dos gráficos?
# Podemos chegar a mesma conclusão anterior?

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

# RESPOSTA: Aqui não podemos manter a mesma afirmação que na situação anterior. Isto se dá pois
#           houve uma aproximação muito forte de comportamento entre as populações, de modo que
#           tanto no banco de dados com porcentagem de negros acima da mediana, quanto no banco
#           com porcentagem abaixo da mediana, os valores, ou o número de pessoas que votaram
#           em Obama foi muito similar, de modo que não dá para afirmar que há uma relação muito
#           grande entre cor e votar ou não em Obama.

# A partir da varíavel X do banco df abaixo

df <- data.frame( x = cos( seq( -50, 50, 0.5 ) ) )

# x é uma variável número contínua
str( cos( seq( -50, 50, 0.5 ) ) )

# Faça os tipos de gráficos que representem esse tipo de variável
# comentando as diferenças entre elas e qual seria a mais adequada

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

# RESPOSTA: Como mostrado em str( cos( seq( -50, 50, 0.5. ) ) ), x é uma variável contínua, i.e.,
#           ela representa números com decimais. Tanto o histograma quanto o gráfico de barras e
#           o beeswarm contam a frequênciade ocorrência de certos valores, enquanto que boxplot,
#           violin e density apresentam de maneira distinta a densidade da distribuição desses
#           valores. density() apresenta uma curva que descreve a distribuição dos valores. Enquanto
#           o boxplot e violin apresentam como estão concentrados esses valores na distribuição.
#           Neste caso, dado o tipo dos dados, i.e., números reais/continuos, a curva de densidade,
#           beeswarm() e um histograma seriam os melhores representantes visuais dos dados, já que
#           exibem de maneira clara não só os valores dos mesmos, como também a concentração, i.e.,
#           distribuição de cada valor em relação aos outros.

# responsa as questões teóricas abaixo