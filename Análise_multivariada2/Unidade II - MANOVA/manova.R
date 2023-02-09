## ------
## MANOVA com uma via no R
## Data: 11/09/2022
## ------

## ------
## Bibliotecas
## ------

if(!require(pacman)) install.packages("pacman"); library(pacman)
p_load(dplyr, car, rstatix, ggplot2, MVN, GGally, emmeans, psych)

## ------
## Banco de dados
## ------

# Efeito do Álcool na memória e a latência

setwd('E:/')

dados = read.csv2("BD6.csv")

head(dados)
describe(dados)
summary(dados)

## ------
## Verificacao  da normailidade multivariada, por grupo.
## ------

# Extensão do teste de shapiro-wilk
# Fazer um teste 

dados %>% select(2:4) %>% group_by(Alcool) %>% 
  doo(~mshapiro_test(.)) 

## A ultima categoria nao atende uma distribuicao normal multivariada

## Testes do pacote MVN

mvn(data = dados[,2:4], subset = "Alcool", mvnTest = 'hz')

mvn(data = dados[,2:4], subset = "Alcool", mvnTest = 'dh')

mvn(data = dados[,2:4], subset = "Alcool", mvnTest = 'energy')

mvn(data = dados[,2:4], subset = "Alcool", mvnTest = 'mardia')

mvn(data = dados[,2:4], subset = "Alcool", mvnTest = 'royston')


## ------
## Identificar outliers
## Pela distancia de Mahalanobis (outlier = p<0,001)
## ------

outliers_m = dados %>% select(2:4) %>% group_by(Alcool) %>% 
  doo(~mahalanobis_distance(.))

##  Verificar se tem outliers
outliers_m %>% filter(is.outlier == TRUE)


## Identificar os outliers por grupos

boxplot(dados$Memoria~dados$Alcool)
boxplot(dados$Memoria~dados$Latencia)

dados %>% group_by(Alcool) %>%
  identify_outliers(Memoria)

dados %>% group_by(Alcool) %>%
  identify_outliers(Latencia)


## ------
## Homogenenidade de Variancias (matriz de variancias e covariancias)
## ------

# Se rompido e n iguais por grupo: Pillai e Hotelling sao confiavaeis
# Caso os n sejam diferentes, uma opcao e usar uma manova robusta
# Alfa: 0,001

# H0: Existe homogeneidade das variancias
# H1: Nao Existe homogeneidade de variancias

box_m(dados[, c("Memoria", "Latencia")], dados$Alcool)

# Veficacao da homogeneidade de variancias - teste de Levene no pacote car

leveneTest(Memoria ~ Alcool, dados, center = mean) # nao sao homogeneas

leveneTest(Latencia ~ Alcool, dados, center = mean) # sao homogeneas


## ------
## Verificacao d ausencia de multicolinearidade
## ------

# baixa correlacao indica ausencia de multicolinearidade
ggpairs(dados[,3:4])
cor(dados[,3:4])

## ------
## Verificacao da relacao linear das variaveis indpendentes para cada grupo
## ------

pairs(dados[,3:4], pch = 19,
      col = factor(dados$Alcool))

## graficos de dipersao separados por grupos


graph_ln = dados %>% select(2:4) %>% group_by(Alcool) %>% 
  doo(~ggpairs(.), results = 'plots')


graph_ln$plots

## ------
## Modelo MANOVA
## ------

# construcao do modelo
modelo = manova(cbind(Latencia, Memoria) ~ Alcool, data = dados)

# analise dos resultados
summary(modelo)

summary(modelo, test = "Wilks")

summary(modelo, test = "Pillai") # mais robusto

# ANOVA Univariada

summary.aov(modelo)

## ------
## Analisando a diferenca entre grupos pos-hoc
## ------

dados %>% emmeans_test(Memoria ~ Alcool, p.adjust.method = "bonferroni")

dados %>% emmeans_test(Latencia ~ Alcool, p.adjust.method = "bonferroni")

TukeyHSD(x = aov(Memoria ~ Alcool, data = dados), "Alcool", conf.level = 0.95)






















