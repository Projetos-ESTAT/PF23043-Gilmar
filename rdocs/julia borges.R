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
summary(dados)

#### (1) Salmao - J&W - Exemplo 11.8 ####

salmon <- read.table("C:/001-dados/cursos/Salmao-T11-2.dat",
                     header = F)
colnames(salmon) <- c("Origin", "Gender", "Freshwater", "Marine")
head(salmon)
dim(salmon)

salmon$Origin <- factor(salmon$Origin, level=c(1,2),
                        labels=c("Alaskan","Canadian"))
salmon$Gender <- factor(salmon$Gender, level=c(1,2),
                        labels=c("Female","Male"))
head(salmon)
table(salmon$Origin,salmon$Gender)



(slda <- lda(Origin ~ Freshwater + Marine, 
             data=salmon, na.action = "na.omit"))

(salmonp <- predict(slda))
(ctable <- table(salmon$Origin, salmonp$class))
(diag(prop.table(ctable,1))) # prop de classif. correta no grupo
(sum(diag(prop.table(ctable)))) # prop total de classf. correta 

plot(salmon$Freshwater,salmon$Marine,
     col = salmon$Origin, pch=20, cex=1.5,
     xlim = c(0,250), ylim = c(300,550),
     main = "Salmon Data",
     xlab = "Freshwater diameter",
     ylab = "Marine diameter")

salmon.new <- data.frame(cbind(rep(0:250,each=200),
                               rep(300:550,200)))
colnames(salmon.new) <- c("Freshwater","Marine")
head(salmon.new)

psalmon.new <- predict(object=slda,newdata=as.list(salmon.new))

salmon.new <- cbind(salmon.new,psalmon.new$class)
head(salmon.new)

plot(salmon.new$Freshwater,salmon.new$Marine,
     col = psalmon.new$class, pch=20, cex=1.5,
     xlim = c(0,200), ylim = c(300,500),
     main = "Salmon Data",
     xlab = "Freshwater diameter",
     ylab = "Marine diameter")

plot(slda, dimen = 1)

partimat(Origin ~ Marine + Freshwater, data=salmon, method="lda")


