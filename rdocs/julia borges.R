source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #

setwd('D:/Downloads/ESTAT')
library(readxl)
library(tidyverse)
library(MASS)
library(caret)

banco <- read_excel('gilmar.xlsx')
summary(banco)

banco<- banco %>%  
  mutate(Manejo = case_when(
    Manejo %>% str_detect("Selvagem") ~ "Selvagem",
    Manejo %>% str_detect("Cultivado") ~ "Cultivado",
    Manejo %>% str_detect("Desconhecido") ~ "Desconhecido"))

dados1 <- subset(banco, Manejo %in% c('Selvagem', 'Cultivado')) # dados de teste

dados2 <- subset(banco, Manejo == 'Desconhecido') # o que estamos interessados

summary(dados1)
summary(dados2)
' apresentam valores próximos, tudo ok '

###### FUNÇÃO DISCRIMINANTE LINEAR ######

' "PRESSUPOSTOS":
Assumindo uma distribuição Gaussiana multivariada!
LDA: assumimos igual covariância entre as classes; a fronteira de
decisão é linear; as matrizes de covariância da população 1 e da população 2 são iguais '

' O QUE É: 
Essencialmente, o LDA compara distâncias entre as médias dos dados e as
médias das classes, e os classifica junto àquela que apresenta a média mais
próxima. '

' PROBLEMAAAAS: 
Se o número de observações em qualquer classe for muito pequeno, a LDA pode não ser confiável. 
Além disso, deve ter ranK completo (n>p)'

attach(banco)
attach(dados1)

(slda <- lda(Manejo ~ `Cr mg/kg`+	`Mn mg/kg` +	`Fe mg/kg` +	`Co mg/kg`+	`Cu mg/kg`+	`ZN mg/kg`+	`Sr mg/kg`+	`Mo mg/kg`+`Cs µg/kg`+	`Ba mg/kg`+	`Hg mg/kg`+	`d 13C`+	`d 15N`, 
             data=dados1, na.action = "na.omit"))

plot(slda)

' o gráfico não chega a apresentar intersecção, o modelo não aparenta possuir uma boa acurácia'

### resultados ###

# PROBABILIDADES INICIAIS (antes da estimação)

' Prior probabilities of groups:
  Cultivado  Selvagem 
0.106383  0.893617  '

# MÉDIAS DOS GRUPOS
# a gente observa se os valores médios entre os grupos apresentam grandes diferenças, determinar quais variáveis são mais úteis
# para discriminar os grupos

' Group means:
          `Cr mg/kg` `Mn mg/kg` `Fe mg/kg` `Co mg/kg` `Cu mg/kg` `ZN mg/kg` `Sr mg/kg` `Mo mg/kg` `Cs µg/kg` `Ba mg/kg` `Hg mg/kg`
Cultivado   3.766281  1.0423930   60.68141 0.07792480  0.8681049   64.39071  0.5133821 0.10244802  139.91297  0.1156036   0.895497
Selvagem    1.090985  0.6948964   22.29430 0.01900199  0.3708376   25.80469  0.3654789 0.01371003   88.06927  0.1800210   0.310248
            `d 13C`  `d 15N`
Cultivado -18.09575 8.164928
Selvagem  -32.81170 9.358952 '

# COEFICIENTES DO LDA 

' Coefficients of linear discriminants:
                      LD1
`Cr mg/kg`  0.07098200926
`Mn mg/kg` -0.81990375376
`Fe mg/kg`  0.00009101107
`Co mg/kg` -8.32343793849
`Cu mg/kg` -2.14276406199
`ZN mg/kg`  0.00926176082
`Sr mg/kg` -1.64647088800
`Mo mg/kg` -2.34035866679
`Cs µg/kg`  0.02298587902
`Ba mg/kg`  6.94775490002
`Hg mg/kg` -3.48939770477
`d 13C`    -0.83763333441
`d 15N`     0.72062102114 '

#-------------# PREDIÇÃO #-------------#

# mostra as probabilidades a posteriori, pra qual classe está cada um e os valores da função discriminante,
# ou seja, o resultado da equação com os coeficientes encontrados

novo <- slda %>% predict(dados2) # decisão para os desconhecidos com base no modelo proposto

' to com impressão de OVERFITTING, pois: '

### adeuqabilidade do modelo ###

(l<-mean(conferindo$class==dados1$Manejo))

conferindo <- slda %>% predict(dados1) # mesmos dados que usei pra estimar, usei para predizer aqui
table(conferindo$class, dados1$Manejo)
'             Cultivado Selvagem
  Cultivado         5        0
  Selvagem          0       42 ' # nenhum falso positivo ou falso negativo (matriz de confusão)

(m<- confusionMatrix(conferindo$class, as.factor(dados1$Manejo)))


###### FUNÇÃO DISCRIMINANTE QUADRÁTICA ######

(sqda <- qda(Manejo ~ `Cr mg/kg`+	`Mn mg/kg` +	`Fe mg/kg` +	`Co mg/kg`+	`Cu mg/kg`+	`ZN mg/kg`+	`Sr mg/kg`+	`Mo mg/kg`+`Cs µg/kg`+	`Ba mg/kg`+	`Hg mg/kg`+	`d 13C`+	`d 15N`, 
             data=dados1, na.action = "na.omit"))
' Error in qda.default(x, grouping, ...) : 
  some group is too small for qda '
plot(sqda)

#-------------# PREDIÇÃO #-------------#

