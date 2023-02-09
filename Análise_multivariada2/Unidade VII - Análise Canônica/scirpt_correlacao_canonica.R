## ------
## Analise Multivariada II e 
## Aprendizagem nao-supervisionada
## Analise de correlacao canonica
## ------

## A analise de Correlacao canonica permite mensurar o relacionamento
## linear entre dois conjuntos de variaveis

## Um conjunto de variaveis pode ser definido como variaveis dependentes
## e o outro como variaveis dependentes 

## pacotes
if(!(require(pacman))) install.packages("pacman"); library(pacman)

p_load(knitr, CCA, #3 analise de correlacao canonica
       CCP) ## teste de significancia das cargas canonicas

## dados
setwd("C:/Users/NetoDavi/Downloads")

hatco = readxl::read_excel('HATCO.xlsx', sheet = 2)

head(hatco)

# Teremos dois grupos de avriaveis:
#   
# Grupo X:
#   
# X1: Velocidade de entrega;
# X2: Nivel de preco;
# X3: Flexibilidade de preco;
# X4: Imagem do fabricante;
# X5: Servico geral;
# X6: Imagem da equipe de vendas;
# X7: Qualidade do produto.

X = as.matrix(hatco[,2:8])

# Grupo Y:
#   
# X9: Nivel de uso;
# X10: Nivel de satisfacao. 

Y = as.matrix(hatco[,10:11])


## Suposicao 

## O vetor de variaveis independentes e dependentes sejam correlacionados

correlacao = matcor(X,Y)

# dentro do grupo X
correlacao$Xcor

# dentro do grupo Y
correlacao$Ycor

## cruzada em X e Y
correlacao$XYcor

## visualizando as correlacoes
img.matcor(correlacao, type = 2)

## Correlacao canonica
ccxy = cc(X, Y)

names(ccxy)

## A dimensao e dada pelo conjunto com menos variveis 

valor.ca = ccxy$cor

print(valor.ca[1]) ## C(U1, V1)
print(valor.ca[2]) ## C(U2, v2)


barplot(ccxy$cor, xlab = "Dimensão", 
        ylab = "Correlação canônica", names = 1:2,
        ylim = c(0,1))

## Testes multivariados de significancia para as ambas
## funcoes canonicas

rho = ccxy$cor

## Defina o número de observações "n"
## o número de variáveis no primeiro conjunto de dados "p"
## o número de variáveis no primeiro conjunto de dados "q"

n = dim(hatco)[1]
p = dim(X)[2]
q = dim(Y)[2]

## H0: A corrrelacao nao e significtiva
## H1: A corrrelacao  e significtiva

## estatistica de teste
## p -valor

## Lambda de Wilks

p.asym(rho, n, p, q, tstat = 'Wilks')

## Traço de Pillai

p.asym(rho, n, p, q, tstat = 'Pillai')

## Traço de Hotelling

p.asym(rho, n, p, q, tstat = 'Hotelling')


## ------------------------------------------------------------
## Proporcao da variancia total explicada

## Precisaremos das cargas canonicas

loadings = comput(X, Y, ccxy)

## cargas canonicas
## corelacao entre a variaveis canonicas e os bancos
loadings[3:6]

## pvte uk: independentes 
pvte.u = (colSums((loadings$corr.X.xscores)^2)) / (dim(X)[2])*100

## pvte vk: dependentes 
pvte.v = (colSums((loadings$corr.Y.yscores)^2)) / (dim(Y)[2])*100


print(pvte.u)

## PVTE U1 = 27.516544 ==> primeira variavel canonica U1 explica 27,61%
## da variancia original do grupo de variaveis independnetes

## PVTE U2 = 8.224648 ==> segunda variavel canonica U1 explica 8,22%
## da variancia original do grupo de variaveis independnetes


print(pvte.v)

## PVTE V1 = 85.4406 ==> primeira variavel canonica V1 explica 85,47% da
## variancia original do grupo de variaveis independnetes

## PVTE V2 = 14.55938 ==> segunda variavel canonica V1 explica 14,56% da
## variancia original do grupo de variaveis independnetes

## ------------------------------------------------------------
## Indice de Redudancia

## Sintetiza a PVTE eo R² canonico em um unico indicador 

## R² canonico: indica o quanto da variancia da variavel canonica 
## dependenete e explicada pela variavel canonica independnete. E
## o quadrado da correlacao canonica.

# Podemos obter o R² canonico
r2.c = ccxy$cor^2

r2.c

# R²1 = 0.8778527 e  R²2 = 0.2610223, ou seja, 87,79% da variancia
# da primeira variavel canonica dependnete V1 e explicada pela primeira
# variavel canonica independnete U1.

# Para a segunda variavel canonica dependnete V2, temos que a segunda variavel
# independente explica 26,01% de sua variancia.

## Calculando o Indice de Redudancia (IR) para as variaveis canonicas Uk
ir.x = (colSums((loadings$corr.X.xscores)^2))/(dim(X)[2])*(ccxy$cor^2)
ir.x

## Calculando o Indice de Redudancia (IR) para as variaveis canonicas vk
ir.y = (colSums((loadings$corr.Y.yscores)^2))/(dim(Y)[2])*(ccxy$cor^2)
ir.y


## Coeficinetes padronizados (pesos canonicos)
## A magnitude dos pesos canonicos (coeficientes padronizados) representa
## a contribuicao relativa de cada variavel para com a respectiva variavel
## estatistic latente (variavel canonica)

sx = diag(sqrt(diag(cov(X))))
sx %*% ccxy$xcoef 


sy = diag(sqrt(diag(cov(Y))))
sy %*% ccxy$ycoef 


## ------------------------------------------------------------
## Cargas canonicas

# Correlacao entre as variaveis independentes e suas variaveis canonicas
ccxy$scores$corr.X.xscores

# Correlacao entre as variaveis dependentes e suas variaveis canonicas
ccxy$scores$corr.Y.yscores

# cargas canonicas para a variavel estatistica independente
kable(ccxy$scores$corr.X.xscores,
      col.names = c("Carga Canonica U1", "Carga Canonica U2"))


# cargas canonicas para a variavel estatistica dependente
kable(ccxy$scores$corr.Y.xscores,
      col.names = c("Carga Canonica V1", "Carga Canonica V2"))
