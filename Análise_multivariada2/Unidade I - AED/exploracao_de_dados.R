## ------
## Analise Multivariada e Aprendizado nao-Supervisionado
## Analise Exploratoria dos Dados
## Data: 09/09/2022
## Autor: Dionisio Neto
## ------

## ---
## Pacotes
## ---

if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(corrplot, readxl, PerformanceAnalytics, ggplot2,GGally, 
       lattice, graphics, sm, gmodels)

## ---
## Banco de dados
## ---
setwd("C:/Users/NetoDavi/Downloads")

dados = read_excel("HATCO.xlsx", sheet = 2)

head(dados)
rownames(dados) = dados$id
dados[,'id'] = NULL

head(dados)
quantitativas = c("X1","X2","X3","X4","X5","X6","X7","X9","X10")

summary(dados[, quantitativas])

## vetor de medias

apply(dados[, quantitativas], 2, mean)

## matrix de covariancias
cov(dados[, quantitativas])


## matrix de correlacoes
cor(dados[, quantitativas])

corrplot(cor(dados[, quantitativas]), type = 'lower',
         method = 'number')

## Tabelas de frequências absolutas e relativas

## Para X8
table(dados[, "X8"])
prop.table(table(dados[, "X8"]))

## Para X11
table(dados[, 'X11'])
prop.table(table(dados[, 'X11']))

## Para X12
table(dados[, 'X12'])
prop.table(table(dados[, 'X12']))

## Para X13
table(dados[, 'X13'])
prop.table(table(dados[, 'X13']))

## Para X14
table(dados[, 'X14'])
prop.table(table(dados[, 'X14']))


## Gráfico de Dipersão multivariado
pairs(dados[, quantitativas], pch = 16, main = "Mtriz de Dispersão")
ggpairs(dados[, quantitativas])


chart.Boxplot(dados[,quantitativas])
chart.Correlation(dados[,quantitativas])

library(psych)

corPlot(dados[,quantitativas], cex = 1.2)


## grafico de coordenadas paralelas
## Verificar a correlacao entre variveis pelo cruzamento
parallel(dados[,quantitativas])


## grafico de estrelas
## Uma estrela par cada observacao
stars(dados[,quantitativas], key.loc = c(11,2))


## ------
## English tutorial
## ------

## Univariate Analysis

## --- Get a High level overview of our dataset --- ##

## continuous variable
str(dados)
summary(dados)
class(dados)

head(dados)

## central tendency 
summary(dados$X6)
boxplot(dados$X6)

## spread
hist(dados$X6)
hist(dados$X6, freq = F)
plot(density(dados$X6), main = "Density of X6")

## categorical variable

summary(dados$X14)
table(dados$X14)
prop.table(table(dados$X14))

barplot(table(dados$X14))


## Multivariate Analysis

## Investigating how much a continuous variable is affected bya a factor

by(dados$X6, dados[, "X13"], summary)
by(dados$X6, dados[, "X13"], mean)
by(dados$X6, dados[, "X13"], median)

## boxplot

boxplot(dados$X6~dados$X13, notch = T, col = c("yellow", "grey60"))
sm.density.compare(dados$X6, dados$X14, xlab = "Imagem na forca de vendas")

## Analysing categorical & categorical

xtabs(~X13 + X14, dados)
plot(xtabs(~X13 + X14, dados))

xtabs(~X14 + X8, dados)
plot(xtabs(~X14 + X8, dados))

## Crosstable
CrossTable(dados$X14, dados$X8, chisq = F, prop.t = F)


scatter.smooth(dados$X1, dados$X2)


