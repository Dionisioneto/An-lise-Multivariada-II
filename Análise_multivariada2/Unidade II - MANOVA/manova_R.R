## ------
## MANOVA
## Manipulacao e Tetes
## Autor: Dionisio Neto
## ------ 

## Pacotes
if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(readxl, ggplot2)

## Dados
setwd("D:/")
dados = read_excel("teste1_MANOVA.xlsx")

head(dados)

apply(dados,MARGIN = 2,FUN = mean)
cov(dados[,c(1,2)])
cor(dados[,c(1,2)])

summary(dados)

plot(dados$P, dados$GV)

ggplot() +
  geom_point(data = dados, aes(x = P, y = GV,
                 colour = factor(Tratamento)), size = 2) + 
  scale_color_brewer(palette="Dark2") +
  theme_classic()
  
Y = cbind(dados$P, dados$GV)
is.matrix(Y)

Y = as.matrix(Y)

mF = manova(Y~dados$Tratamento)
mF

## Testes Multivariados de Normalidade Multivariada


## Testes para a verificacao de igualdade de tratamentos na Manova

manova_testes = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")

summary.manova(mF, test = manova_testes[1]) ## Teste de Pillai
summary.manova(mF, test = manova_testes[2]) ## Teste de Wilks
summary.manova(mF, test = manova_testes[3]) ## Teste Hotelling-Lawley
summary.manova(mF, test = manova_testes[4]) ## Roy















