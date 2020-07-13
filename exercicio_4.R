
## Faça todos os gráficos utilizando um tema que você ache mais adequado
## e nomeie os eixos x e y da maneira adequada

## Carregue o banco world do pacote poliscidata

install.packages( "poliscidata" )
install.packages( "tidyverse" )
install.packages( "ggthemes" )

library( poliscidata )
library( tidyverse )
library( ggthemes )
library( graphics )
library( vcd )

banco <- world

## Observe o banco de dados com as funções adequadas

summary( banco )
glimpse( banco )
str( banco )
head( banco )
tail( banco )

## A variável democ_regime08 indica se um país é democrático.
## Usando as ferramentas de manipulacao de bancos de dados, verifique
## quantos paises sao democraticos ou nao, e apresente esta variável 
## graficamente

democraticos <- banco %>% filter( democ_regime08 == "Yes" )
nao_democraticos <- banco %>% filter( democ_regime08 == "No" )

count( democraticos )
count( nao_democraticos )

ggplot( banco, aes( democ_regime08 ) ) +
        geom_bar( )

## Teste a relação entre a variável democ_regime08 e a variável
## muslim (que indica se um país é muçulmano ou não). E represente
## visualmente as variáveis para visualizar se esta religião
## aumenta ou diminui a chance de um país ser democrático
## Qual seria sua conclusão com relação a associação destas duas
## variáveis?

ggplot( banco, aes( democ_regime08, fill = muslim ) ) +
        geom_bar( position = "fill" )

tabela <- table( banco$democ_regime08, banco$muslim )

chisq.test( tabela )

mosaicplot( tabela, shade = TRUE )

assoc( tabela, shade = TRUE )

# RESPOSTA: De acordo com o chi-teste, existe uma relação positiva entr
#          ser mulçumano e não ser democrático, e existe uma relação
#          negativa entre ser mulçumano e ser democrático. Ou seja,
#          ser mulçumano DIMINUI as chances do país ser democrático

## A variável gdppcap08 possui informação sobre o PIB per capta
## dos países. Faça uma representação gráfica desta variável

ggplot( banco, aes( gdppcap08 ) ) +
        geom_histogram( )

## Faça um sumario com a média, mediana e desvio padrão do pib per capta
## para cada tipo de regime politico, represente a associação destas
## variáveis graficamente, e faça o teste estatístico adequado para
## chegar a uma conclusão. Existe associaçào entre as variáveis?

banco %>% filter( !is.na( gdppcap08 ),
                  !is.na( democ_regime08 ) ) %>%
          group_by( democ_regime08 ) %>%
          summarise( media = mean( gdppcap08 ),
                     mediana = median( gdppcap08 ),
                     desvio = sd( gdppcap08 ),
                     n = n( ) )

ggplot( banco, aes( democ_regime08, gdppcap08 ) ) +
        geom_boxplot( )

ggplot( banco, aes( democ_regime08, gdppcap08 ) ) +
        geom_violin( )

t.test( gdppcap08 ~ democ_regime08, data = banco )

# RESPOSTA: Existe sim uma relação positiva entre regime e renda per capta.
#           Mais especificamente, existe uma relação entre o fato do país
#           ser democrático e ele ter uma renda per capta mais alta. Tanto
#           há uma concentração de países democráticos em pontos de renda
#           per capta mais alta, quanto a mediana da renda desses países
#           se encontra em uma faixa de renda per capta mais alta. 

# COMENTÁRIO: Professor, aqui, ao invés de colocar Tipo de Regime Político,
#             seria mais explícito colocar democ_regime08, pois também existe
#             uma variável chamada regime_type3 a qual representa justamen-
#             regimes políticos. Porém, e eu não sei se isso influenciaria,
#             essa última variável não é dicotô- mica! Ou seja, não podemos
#             usar o t.test() nela!

## Por fim, ao invés de utilizar uma variável binária de democracia,
## utilize a variável dem_score14 para avaliar se existe uma associação
## entre regime político e desenvolvimento econômico. Represente
## a associação graficamente, faça o teste estatístico e explica sua
## conclusão

ggplot( banco, aes( dem_score14, gdppcap08 ) ) +
        geom_point( )

scatter.smooth( x = banco$dem_score14, y = banco$gdppcap08, main = "Dem Score x GDPC",
                xlab = "Democratic Score", ylab = "GD Per Capta" )

cor.test( banco$dem_score14, banco$gdppcap08 )

reg_lin <- lm( gdppcap08 ~ dem_score14, data = banco )

summary( reg_lin )

plot( reg_lin )

# RESPOSTA: Existe sim uma relação positiva entre nível de democracia e renda per
#           capta. Podemos afirmar que o nível da democracia influencia sim o nível
#           de desenvolvimento econônomico. Porém, tal relação é mais aparente em
#           níveis mais altos de democracia. Em níveis mais baixos ela não é tão
#           forte assim.

## Teste a associação entre renda perca capta e religiao (com a variável
## muslim) e represente graficamente. Qual é sua conclusão? 

ggplot( banco, aes( muslim, gdppcap08 ) ) +
        geom_boxplot( )

t.test( gdppcap08 ~ muslim, data = banco )

# RESPOSTA: Existe sim uma relação entre religião e renda. Mais especificamente,
#           tal relação é de que não-mulçumanos estao associados a rendas mais
#           altas do que mulçumanos. Ou seja, não-mulçumanos são mais economicamente
#           desenvolvidos.

## Comparando suas conclusões anteriores, é possível afirmar qual
## das duas variáveis possui maior impacto no desenvolvimento economico?
## Por que? 

# RESPOSTA: Eu diria que a relação entre democracia tem maior impacto
#           no desenvolvimento econômico, mas a diferença entre o
#           impacto da democracia no desenvolvimento econômico e o 
#           impacto da religião no desenvolvimento econômico é extre-
#           mamente pequena. Ela existe, mas é muito pequena.

##########################################################################

## Exercício teórico
## Levando em consideração as variáveis de seu trabalho final,
## qual dos 3 testes estatísticos utilizados seria adequado utilizar?
