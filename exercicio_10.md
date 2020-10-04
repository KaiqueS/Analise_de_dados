Exercicio 10
================

### Continuaremos com a utilização dos dados do ESEB2018. Carregue o banco da mesma forma que nos exercicios anteriores

``` r
library( tidyverse )
library( haven )
library( margins )
library( InformationValue )

link <- "https://github.com/MartinsRodrigo/Analise-de-dados/blob/master/04622.sav?raw=true"

download.file(link, "04622.sav", mode = "wb")

banco <- read_spss("04622.sav") 

banco <- banco %>%
         mutate( D10 = as_factor( D10 ) ) %>%
         filter( Q18 < 11,
                 D9 < 9999998,
                 Q1501 < 11,
                 Q12P2_B < 3 ) %>%
         mutate( Q12P2_B = case_when( Q12P2_B == 1 ~ 0,  # Quem votou em Haddad = 0
                                      Q12P2_B == 2 ~ 1 ) ) # Quem votou em Bolsonaro = 1
```

### Crie a mesma variável de religião utilizada no exercício anterior

``` r
Outras <- levels( banco$D10 )[ -c( 3, 5, 13 ) ]

banco <- banco %>%
         mutate( religiao = case_when( D10 %in% Outras ~ "Outras",
                                       D10 == "Católica" ~ "Católica",
                                       D10 == "Evangélica" ~ "Evangélica",
                                       D10 == "Não tem religião" ~ "Não tem religião" ) )
```

### Faça uma regressão linear utilizando as mesmas variáveis do exercício 9 - idade(D1A\_ID), educação (D3\_ESCOLA),

### renda (D9), nota atribuída ao PT (Q1501), auto-atribuição ideológica (Q18), sexo (D2\_SEXO) e religião

### (variável criada no passo anterior) - explicam o voto em Bolsonaro (Q12P2\_B).

``` r
regressao_lin <- lm( Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao, data = banco )
```

### Interprete o resultado dos coeficientes

``` r
summary( regressao_lin )
```

    ## 
    ## Call:
    ## lm(formula = Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + 
    ##     D2_SEXO + religiao, data = banco)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.05532 -0.19854  0.01565  0.16182  0.96682 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               7.067e-01  6.469e-02  10.924  < 2e-16 ***
    ## D1A_ID                    1.140e-03  7.539e-04   1.512  0.13074    
    ## D3_ESCOLA                 5.547e-03  5.226e-03   1.061  0.28873    
    ## D9                       -9.837e-07  3.196e-06  -0.308  0.75832    
    ## Q1501                    -7.728e-02  2.799e-03 -27.610  < 2e-16 ***
    ## Q18                       2.651e-02  3.093e-03   8.570  < 2e-16 ***
    ## D2_SEXO                  -5.286e-02  2.089e-02  -2.530  0.01154 *  
    ## religiaoEvangélica        7.684e-02  2.363e-02   3.251  0.00118 ** 
    ## religiaoNão tem religião -2.746e-03  4.238e-02  -0.065  0.94835    
    ## religiaoOutras           -7.263e-02  3.678e-02  -1.975  0.04855 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3489 on 1138 degrees of freedom
    ## Multiple R-squared:  0.5028, Adjusted R-squared:  0.4989 
    ## F-statistic: 127.9 on 9 and 1138 DF,  p-value: < 2.2e-16

INTERPRETAÇÃO: O intercepto, valor que a variável dependente assume
quando todas as variáveis independentes são iguais à zero, tem valor de
7.067e-01, o qual é estatisticamente significante, i.e., não é igual à
0, dado que o p-valor é baixíssimo, de 2e-16. D1A\_ID tem coeficiente
com valor de 1.140e-03, o qual não é estatisticamente significante,
i.e., não há garantias de que seja diferente de 0, dado que o p-valor é
alto, de 0.13074. D3\_ESCOLA tem coeficiente angular com valor de
5.547e-03, o qual também não é estatisticamente significantes, i.e., não
podemos afirmar que difere de 0, já que o p-valor é alto, de 0.28873. D9
tem coeficiente com valor de -9.837e-07, o qual não é estatisticamente
significante, i.e., não podemos rejeitar a hipótese nula, segundo a qual
tal coeficiente difere de 0, já que o p-valor é altíssimo, de 0.75832.
Q1501 tem valor de -7.728e-02, o qual é estatisticamente significante,
i.e., podemos rejeitar a hipótese nula de que tal coeficiente difere de
0, dado que o p-valor é baixíssimo, de 2e-16. O coeficiente de Q18 tem
valor de 2.651e-02, o qual é estatisticamente significante, i.e.,
podemos rejeitar a possibilidade de ser 0, dado que o p-valor é
baixíssimo, de 2e-16. D2\_SEXO é uma variável categórica, onde a
categoria de referência é o Sexo Masculino. Isso signfica que, neste
modelo, D2\_SEXO verifica o impacto que ser mulher tem na variável
dependente, quando em comparação com ser homem. D2\_SEXO tem valor de
-5.286e-02, o qual é estatisicamente significante, i.e., permite-nos
rejeitar a hipótese nula de que é igual à 0, dado que o p-valor é baixo,
de 0.01154. Religião é outra variável categórica, onde a categoria de
referência é a religião Católica. Ser Evangélico, quando em comparação
com ser Católico, tem coeficiente com valor de 7.684e-02, o qual é
estatisticamente significante, tendo em mente a categoria de referência,
dado que o p-valor é de 0.00118. Não ter Religião, quando em comparação
com a categoria de referência, tem valor de -2.746e-03, o qual não é
estatisticamente significante, dado o p-valor altíssimo de 0.94835, de
modo que não podemos rejeitar a hipótese nula de que este coeficiente é
igual à zero. Por último, quem é de Outras Religiões, quando em
comparação com a categoria de referência, tem o coeficiente com valor
de -7.263e-02, o qual é estatisticamente significante, dado o p-valor de
0.04855, de modo que podemos rejeitar a hipótese nula de que tal
coeficiente é igual a zero. O erro residual padrão é de 0.3489, e o
valor do R quadrado é de 0.5028, o que indica que tal modelo consegue
explicar 50% da variação nos valores da variável dependente. O
coeficiente de cada variável independente indica o quanto a mudança em
uma unidade no valor de cada coeficiente impacta na variação da variável
dependente. Além do mais, como temos duas variáveis categóricas, o zero,
usado como base para o valor do intercepto, é calculado em termos das
categorias de referência de cada uma dessas variáveis categóricas, i.e.,
temos que o zero usado para calcular o alfa é medido em termos da
categoria Homens Católicos.

### O resultado difere dos encontrados anteriormente, quando a variavel dependente era a aprovação de Bolsonaro?

RESPOSTA: No outro modelo, as variáveis com significância estatística
são D3\_ESCOLA, Q1501, Q18 e D2\_SEXO, além do intercepto, enquanto no
presente modelo, as variáveis com significância são Q1501, Q18,
D2\_SEXO, ser de religião Evangélica e ser de Outras Religiões, onde,
para estas duas últimas, temos que levar em consideração que são
pensadas em termos da categoria de referência, que é ser de religião
Católica. Só temos, então três variáveis em comum com significância
estatística. No modelo anterior, Q1501 teve valor de -3.956e-01, o qual
difere do valor no presente modelo, que é de -7.728e-02. Q18 teve valor
de 3.150e-01, enquanto no presente modelo tem valor de 2.651e-02. E, por
último, D2\_SEXO teve valor de -6.115e-01, enquanto no presente modelo o
valor é de -5.286e-02, onde a categoria de referência em ambos os
modelos é Homem. Além dessas variáveis, o Erro Residual no modelo
anterior era de 3.297, enquanto que no modelo atual é bem menor, de
0.3489. O R quadrado no modelo antigo era de 0.3028, i.e., 30%, enquanto
que no modelo atual é de 0.5028, i.e., 50%, indicando que o modelo atual
explica mais a variação na variável dependente do que o modelo anterior.
Ou seja, o resultado difere bastante entre os modelos, já que o modelo
anterior erra mais e explica menos que o atual.

### Faça uma regressão logistica com as mesmas variaveis

``` r
regressao_log <- glm( Q12P2_B ~ D1A_ID + D3_ESCOLA + D9 + Q1501 + Q18 + D2_SEXO + religiao,
                      data = banco, family = "binomial" )
```

### Transforme os coeficientes estimados em probabilidade

``` r
margins( regressao_log )
```

    ##    D1A_ID D3_ESCOLA         D9    Q1501     Q18 D2_SEXO religiaoEvangélica
    ##  0.001171  0.006589 -5.421e-07 -0.05471 0.02622 -0.0526            0.07346
    ##  religiaoNão tem religião religiaoOutras
    ##                 -0.002521       -0.08172

``` r
summary( margins( regressao_log ) )
```

    ##                    factor     AME     SE        z      p   lower   upper
    ##                    D1A_ID  0.0012 0.0007   1.5849 0.1130 -0.0003  0.0026
    ##                   D2_SEXO -0.0526 0.0202  -2.6078 0.0091 -0.0921 -0.0131
    ##                 D3_ESCOLA  0.0066 0.0051   1.2949 0.1953 -0.0034  0.0166
    ##                        D9 -0.0000 0.0000  -0.1935 0.8466 -0.0000  0.0000
    ##                     Q1501 -0.0547 0.0009 -57.9079 0.0000 -0.0566 -0.0529
    ##                       Q18  0.0262 0.0030   8.8434 0.0000  0.0204  0.0320
    ##        religiaoEvangélica  0.0735 0.0235   3.1280 0.0018  0.0274  0.1195
    ##  religiaoNão tem religião -0.0025 0.0417  -0.0605 0.9517 -0.0842  0.0791
    ##            religiaoOutras -0.0817 0.0379  -2.1574 0.0310 -0.1560 -0.0075

### Quais foram as diferenças no resultado entre usar a regressão linear e a logistica?

RESPOSTA: No caso da regressão logística, após a transformar os
coeficientes estimados em probabilidades, temos valores que indicam o
quanto cada coeficiente afeta as chances, ou probabilidades, de obtermos
a variável dependente. No caso do presente exercício, isso significa o
quanto cada variável independente afeta as chances de termos ou não um
voto em Bolsonaro, i.e., a variável Q12P2\_B. Comparando os resultados,
vemos que em ambas as regressões, a variável D1A\_ID não tem
significância estatística. Isto se dá pois o p-valor da variável em
ambas as regressões é alto, sendo 0.1130 na regressão logística, o que
sugere a possbilidade do valor da mesma ser igual à zero. A variável
D2\_SEXO é estatisticamente significante em ambas as regressões, com
p-valor de 0.0091 na regressão logística. D3\_ESCOla não é
estatisticamente significante em nenhuma das regressões, tendo p-valor
alto, de 0.1953 no modelo logístico, o que não nos permite rejeitar a
hipótese nula. D9 também não é estatisticamente significante em nenhum
dos modelos, tendo p-valor de 0.8466 no modelo logístico. Q1501 é
estatisticamente significante em ambos, valendo o mesmo para Q18. Quanto
à variável categórica religião, a qual tem como referência a categoria
Católicos, temos que ser Evangélico é estatisticamente significante em
ambos os modelos, Não Ter Religião não é estatisticamente significante
em nenhum modelo e ser de Outras Religiões é estatisticamente
significante em ambos os modelos. De forma geral, ambos os modelos de
regressão suportam os mesmos resultados, reforçando-se, de modo que não
há grandes diferenças entre os mesmos.

### Verifique a quantidade de classificações corretas da regressao logistica e avalie o resultado

``` r
predicoes_reg_log <- predict( regressao_log, type = "response" )

opt_cutoff <- optimalCutoff( banco$Q12P2_B, predicoes_reg_log )

1 - misClassError( banco$Q12P2_B, predicoes_reg_log, threshold = 0.556687512770529 )
```

    ## [1] 0.8362

``` r
confusionMatrix( banco$Q12P2_B, predicoes_reg_log, threshold = opt_cutoff )
```

    ##     0   1
    ## 0 393 105
    ## 1  83 567

``` r
prop.table( confusionMatrix( banco$Q12P2_B, predicoes_reg_log, threshold = opt_cutoff ) )
```

    ##            0          1
    ## 0 0.34233449 0.09146341
    ## 1 0.07229965 0.49390244

RESPOSTA: Como misClassError nos retorna a proporção de classificações
incorretas, usamos 1 - misClassError justamente para descobrir a
proporção de respostas corretas. Como um dos argumentos utilizados em
tal cálculo é threshold, usamos optimalCutoff para descobrir qual valor
maximiza a correteza do modelo, sendo tal valor 0.55667…. O resultado
demonstra que o modelo acerta 0.8362, ou 83% das classificações. Segundo
prop.table(), podemos ver que o modelo acerta 0.34233449 das ocasiões
onde as pessoas não votaram em Bolsonaro, i.e., quando o valor da
variável dependente é zero, e acerta 0.49390244 das ocasiões onde o
valor da variável dependente é 1, i.e., quando votaram em Bolsonaro.
Esses valores, em probabilidade, indicam 34% de acertos para quando o
valor da variável dependente é 0 e 49% de acertos para quando o valor da
mesma é 1.
