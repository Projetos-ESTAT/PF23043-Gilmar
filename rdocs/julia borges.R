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
library(car)
library(agricolae)
library(gridExtra)
library(grid)
library(ExpDes.pt)
library(easyanova)
library(xtable)
library(stats)
library(olsrr)
require(lmtest)
require(lawstat)
library(EnvStats)
require(leaps)
library(caret)
pacman::p_load(cluster,mclust,andrews,graphics,foreign,gplots,heatmap.plus,proxy)
pacman::p_load(tidyverse,MASS,klaR,ggplot2,knitr,cowplot,Rchoice,AICcmodavg,questionr,mdscore,nlme)

banco <- read_excel('gilmar.xlsx')
summary(banco)

dados1 <- subset(banco, Manejo %in% c('Selvagem - AM', 'Selvagem - TO', 'Cultivado - TO'))
dados2 <- subset(banco, Manejo == 'Desconhecido')

###### FUNÇÃO DISCRIMINANTE LINEAR ######

attach(banco)
attach(dados1)

(slda <- lda(Manejo ~ `Cr mg/kg`+	`Mn mg/kg` +	`Fe mg/kg` +	`Co mg/kg`+	`Cu mg/kg`+	`ZN mg/kg`+	`Sr mg/kg`+	`Mo mg/kg`+`Cs µg/kg`+	`Ba mg/kg`+	`Hg mg/kg`+	`d 13C`+	`d 15N`, 
             data=dados1, na.action = "na.omit"))
print(slda)

' as matrizes de covariância da população 1 e da população 2 são iguais '

###### FUNÇÃO DISCRIMINANTE QUADRÁTICA ######

# (salmonp <- predict(slda))
# (ctable <- table(salmon$Origin, salmonp$class))
# (diag(prop.table(ctable,1))) # prop de classif. correta no grupo
# (sum(diag(prop.table(ctable)))) # prop total de classf. correta 
# 
# plot(salmon$Freshwater,salmon$Marine,
#      col = salmon$Origin, pch=20, cex=1.5,
#      xlim = c(0,250), ylim = c(300,550),
#      main = "Salmon Data",
#      xlab = "Freshwater diameter",
#      ylab = "Marine diameter")
# 
# salmon.new <- data.frame(cbind(rep(0:250,each=200),
#                                rep(300:550,200)))
# colnames(salmon.new) <- c("Freshwater","Marine")
# head(salmon.new)
# 
# psalmon.new <- predict(object=slda,newdata=as.list(salmon.new))
# 
# salmon.new <- cbind(salmon.new,psalmon.new$class)
# head(salmon.new)
# 
# plot(salmon.new$Freshwater,salmon.new$Marine,
#      col = psalmon.new$class, pch=20, cex=1.5,
#      xlim = c(0,200), ylim = c(300,500),
#      main = "Salmon Data",
#      xlab = "Freshwater diameter",
#      ylab = "Marine diameter")
# 
# plot(slda, dimen = 1)
# 
# partimat(Origin ~ Marine + Freshwater, data=salmon, method="lda")
# 
# 
