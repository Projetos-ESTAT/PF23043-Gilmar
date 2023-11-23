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


#carregando o banco------------
library(readxl)

banco <- read_excel("banco/Tabela após análises- SVS2 e Catos - Tabela geral com todas amostras e resultados_08-09-2023 - Dados preliminares.xlsx")
banco <- banco[, c("Manejo", "d13C", "d15N")]

#retirando NAs e desconhecidos
banco <- subset(banco, !is.na(Manejo) & Manejo != "Desconhecido")
 

#Testes-----

#verificando a presença de outliers depois (não esquecer, to só presumindo)

#Dividindo os dados em dois grupos com base na categoria 'Manejo' (Vou descartar essa parte)-------------
cultivado <- as.numeric(banco$d13C[banco$Manejo == "Cultivado"])
shapiro.test(cultivado) #rejeita normalidade #menor que 0,01 0.004925
selvagem <-  as.numeric(banco$d13C[banco$Manejo == "Selvagem"])

#will
wilcox.test(cultivado,selvagem, exact = TRUE) #rejeitou a HO, existe diferença




#Correção - d13c----
shapiro.test(banco$d13C) #rejeita a HO (normal)
cultivado <- as.numeric(banco$d13C[banco$Manejo == "Cultivado"])
selvagem <-  as.numeric(banco$d13C[banco$Manejo == "Selvagem"])
wilcox.test(cultivado,selvagem, exact = T) #rejeita a HO tb


#Descartando também (para d15N)------
cultivado_d15N <- as.numeric(banco$d15N[banco$Manejo == "Cultivado"])
shapiro.test(cultivado_d15N) #rejeita normalidade #menor que 0,01 0.1635
selvagem_d15N <-  as.numeric(banco$d15N[banco$Manejo == "Selvagem"])
shapiro.test(selvagem_d15N) #rejeita 0,3306


#Correção - d15N----
shapiro.test(banco$d15N) #rejeita a HO (normal)
cultivado <- as.numeric(banco$d15N[banco$Manejo == "Cultivado"])
selvagem <-  as.numeric(banco$d15N[banco$Manejo == "Selvagem"])
wilcox.test(cultivado,selvagem, exact = T) #rejeita a HO tb




#descartar os seguintes testes também------------
#ANOVA

summary(aov(d15N~Manejo, data=banco)) #estou observando o p-valor 0,000382 


#will
t.test(cultivado_d15N,selvagem_d15N, 
       exact = FALSE) #rejeitou a HO, existe diferença

#p-valor baixíssimo (again). Rejeita a HO



