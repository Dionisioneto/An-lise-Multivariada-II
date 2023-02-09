#Importe os dados RTE_CEREAL.txt


#Instalando o pacote dplyr para nomear as variáveis
install.packages("dplyr")

library(dplyr)

cereal2 <- select(cereal, identidade=X1,  numero=X2,  satisfaz=X3,
                natural=X4,  fibra=X5,  doce=X6,    facil=X7,
                sal=X8,         gratificante=X9,    energia=X10,
                divertido=X11,  criancas=X12,       encharcado=X13,
                economico=X14,  saude=X15,          familia=X16,
                calorias=X17,   simples=X18,        crocante=X19,
                regular=X20,    acucar=X21,         fruta=X22,     
                processo=X23,   qualidade=X24,      prazer=X25,    
                chato=X26,      nutritivo=X27
                )

names(cereal2)

#################################################################

install.packages("psych")
library(psych)
install.packages("GPArotation", dependencies = T)
library(GPArotation)

#fa(r,nfactors=1,n.obs = NA,n.iter=1, rotate="oblimin", scores="regression",
##residuals=FALSE, SMC=TRUE, covar=FALSE,missing=FALSE,impute="median",
##min.err = 0.001, max.iter = 50,symmetric=TRUE, warnings=TRUE, fm="minres",
##alpha=.1,p=.05,oblique.scores=FALSE,np.obs=NULL,use="pairwise",cor="cor",
##correct=.5,weight=NULL,...)

#r - Banco de dados ou matriz de correlação ou covariância
#nfactors - número de fatores a serem extraídos
#rotate - método de rotação. 
#       "oblimin" para oblíqua (default);
#       "varimax" para rotação ortononal;
#       "none" para nenhuma rotação;
#scores  - método de estimação dos escores fatoriais
#         "regression" (default) - extrai os fatores usando uma regressão
#         "Bartlett"  - cálculo via componentes principais
#fm -   método de estimação das cargas fatoriais
#       "pa" - implementar a estimação por componentes principais
#       "ml" - estimativa via máxima verossimilhança



#Estimando o modelo, com quatro fatores, sem rotação
pf<-fa(cereal2[,3:27],nfactors=4, rotate = "none")
#Estimando o modelo com quatro fatores, com rotação ortogonal varimax
pf.v<-fa(cereal2[,3:27],nfactors=4, rotate = "varimax")

#----------------------------------------------
#Visualizando os resultados

print(pf)

print(pf.v)

#Exibe apenas as cargas fatoriais (factor loadings) maiores do que 0,4
print(pf$loadings,cutoff = 0.4)
print(pf.v$loadings,cutoff = 0.4)
#Segundo Hair et al (2005), cargas fatoriais acima de 0,3 podem
# ser consideradas significativas para uma amostra de pelo menos
# 850 observações.
#-----------------------------------------------
#Interpretando a saida 

#MR1, MR2, ... são os fatores
#h2 a cumunalidade
#u2 o erro (unicidade + resíduos)

#Podemos analisar a adequação dos dados ao modelo

##A raiz da média dos quadrados dos resíduos (RMSR) é __
### O valor aceitável é aquele mais próximo de zero

##O indice para a raiz do quadrado médio da aproximação (RMSEA) foi igual a ___
### Devemos ter um valor abaixo de 0,05 para termos um bom ajustamento

##O índice Tucker-Lewis Index (TLI) foi igual a ___
### Um valor aceitável deve estar acima de 0.9.

#----------------------------------------------------
#Teste para o número de fatores

#Apresenta gráfico com os testes:
# Very Simple Structure Fit
# Compexity
# Empirical BIC
#Root Mean Residual
nfactors(cereal2[,3:27], 8, rotate = "none")


#Parallel analysis
#https://journals.sagepub.com/doi/pdf/10.1177/1536867X0900900207
#O critério de Horn corresponde ao ponto onde os autovalores 
#ajustados cruzam a linha horizontal em y=1, que é equivalente
# ao ponto onde os autovalores não ajustados cruzam  a curva da
# média dos autovalores aleatórios
fa.parallel(cereal2[,3:27],n.obs=235,fm="pa",fa="both",nfactors=1,
            main="Parallel Analysis Scree Plots",
            n.iter=20, show.legend=T, ylab="Autovalores da análise
            de componentes principais  e análise fatorial")

#----------------------------------------------------
#A decisão de quantos fatores k devem ser mantidas é uma decisão crítica.
#Devem ser escolhidos um numero de fatores de forma que tenhamos uma
##representação adequada da matriz de covarância ou correlação.
#Uma solução com k=m e k=m+1 resultadará em diferentes "factor loadings" 
##(corefientes da regressão) para todos os fatores, ao contrário da 
## análise de componentes principais, em que o primeiro componente irá
## ser idêntico em cada solução.
#Podemos usar o teste U
##Ho: k componentes são suficientes para descrever os dados
##Ha: a matriz de covariância não tem restrições
##Se U não é significativo, o valor de k é aceitável
#Testando quantos fatores devem ser mantidos

sapply(1:10, function(nf)
  factanal((cereal2[,3:27]), factors = nf,
           method = "mle")$PVAL)

#'factanal' é a função;
#"covmat" é matriz de correlação;
#"factors=nf" especifica o número de fatores. "nf" foi especificado acima
#'method="mle"' é o método de extraçao dos fatores;
#"n.obs=1634" é o número de observações que deram origem à matriz de correlçaõ.
##Devemos usar essa opção quando usamos a matriz de correlação.
#"PVAL" especifica que queremos o p-valor.
#-----------------------------------------------------------

#Gráficos

#Na análise fatorial (assim como na análise de componentes),
# estamos interessados em interpretar os resultados em termpos
# da correlação das variáveis com os fatores. 
#Na AF essa correlação são as cargas fatoriais.
#Essa relação pode ser representada  por uma tabela ou
# graficamente, onde todas as cargas fatoriais, em valor 
# absoluto acima de um dado valor de corte (opção cut)
# são representados por retas.

#Para representar o gráfico dos escores rotacionados
fa.diagram(pf.v)

#Salvando as cargas fatoriais
f3l <- pf$loadings

plot(pf,cut=0.3)


#
model <- fa(cereal2[,3:27],5)
windows(7,7)
par(mfrow=c(2,2))
plot(loadings(model)[,1],loadings(model)[,2],pch=16,xlab="Fator 1",
     ylab="Fator 2",col="blue", ylim = c(0,1))
text((loadings(model)[,1]),(loadings(model)[,2]+0.06), (rownames(cereal)), cex = 0.8)

plot(loadings(model)[,1],loadings(model)[,3],pch=16,xlab="Fator 1",
     ylab="Fator 3",col="red")
plot(loadings(model)[,1],loadings(model)[,4],pch=16,xlab="Fator 1",
     ylab="Fator 4",col="green")
plot(loadings(model)[,1],loadings(model)[,5],pch=16,xlab="Fator 1",
     ylab="Fator 5",col="brown")


#---------------------------------------------------
#Comando para estimar os escores fatoriais
#factor.scores(x, f, Phi = NULL, method = c("Thurstone", "tenBerge", 
#"Anderson","Bartlett", "Harman","components"),rho=NULL,impute="none")

#Os escores fatoriais são as estimativas para a parte comum das 
## variáveis e não são os fatores reais, apenas estimativas.

#Salvando os escores não rotacionados no objeto scores
scores <- pf$scores

#Exportando os escores fatoriais
#write.infile(scores, "escores.csv", sep = ";")
write.csv(scores, "escores.csv")
#
write.csv(scores, "C:/Users/monik/OneDrive/Documentos/Mult_II/Aula 3 - FA/escores.csv")

#A opção type="n" retira os pontos que demarcam os valores
cex <- 0.8
#Protando o gráfico dos escores 1 x 2
plot(scores[,1], scores[,2], type = "n", xlab = "Fator 1", ylab = "Fator 2")
text(scores[,1], scores[,2], abbreviate(rownames(cereal2[,3:27]), 5), cex = cex)

