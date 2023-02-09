#Análise Fatorial-Dados AF1-adjetivos de colegas
#Matriz de correlação
R=cor(AF1[,-1])
print(R, digits = 3)
#Autovalores de R
autovalores = eigen(R)
autovalores
#scree plot
plot(autovalores$values, xlab = 'Autovalor', ylab = 'Tamanho do autovalor', main = 'Scree Plot', type = 'b', xaxt = 'n')
axis(1, at = seq(1, 4, by = 1)) 
#Estimação das cargas - Método componente principal
#Computacionalmente, para estimar as cargas fazemos a decomposição espectral,
#Fatoramos S=CDC', onde C é uma matriz ortogonal dos autovetores
#normalizados de S e D é uma matriz diagonal, cuja diagonal contém
#os autovalores de S
C = as.matrix
C
D = matrix(0, dim(C)[2], dim(C)[2])
diag(D) = autovalores$values[1:2]
#Cargas fatoriais
cargas = C %*% sqrt(D)
cargas
#Comunalidades
h2 <- rowSums(cargas^2)
h2
#Variância específica
u2 <- diag(R) - h2
u2
#Proporção das variâncias total das cargas
prop.cargas <- colSums(cargas^2)
prop.cargas
prop.var <- cbind(prop.cargas[1] / sum(autovalores$values), prop.cargas[2] / sum(autovalores$values))
prop.var
#proporção da variância explicada pelos fatores
prop.exp <- cbind(prop.cargas[1] / sum(prop.cargas), prop.cargas[2] / sum(prop.cargas))
prop.exp
#Estimação das cargas - Método fator principal
#Como a matriz de correlação é singular, será substuído na diagonal principal
#o maior valor de cada linha de R. A matriz será chamada AF2
R2 = as.matrix(AF2)
R2
#Autovalores de R2
autovalores2 = eigen(R2)
autovalores2
#Estimação das cargas
C2 = as.matrix(autovalores2$vectors[,1:2])
D2 = matrix(0, dim(C2)[2], dim(C2)[2])
diag(D2) = autovalores2$values[1:2]
#Cargas fatoriais
cargas2 = C2 %*% sqrt(D2)
cargas2

#Estimação com o pacote psych
Library(psych)
#Método componente principal
af.res = principal(AF1[,-1], nfactors = 2, rotate = 'none', cor = TRUE)
af.res
res.CP = principal(R, nfactors = 2, rotate = 'none', covar = FALSE)
res.CP
res.CP.rot = principal(R, nfactors = 2, rotate = 'varimax', covar = FALSE)
res.CP.rot
scorcp = factor.scores(AF1[,-1], f=res.CP.rot)
scorcp

#estimação máxima verossimilhança
res.versim <- fa(R,rotate="none",fm="ml")
