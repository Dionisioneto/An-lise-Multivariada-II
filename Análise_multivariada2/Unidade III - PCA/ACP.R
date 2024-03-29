#APC com o pacote FactorMineR e factorextra
#Matriz de covari�ncias das vari�veis originais
S = cov(DadosCP1)
#visualizando a matriz S
S
#Matriz de correla��o das vari�veis originais
R = cor(DadosCP1)
#visualizando a matriz R
R
#padronizando os dados - padroniza��o para vari�ncia 1 e m�dia qualquer
dadoscp=scale(DadosCP1, center = FALSE, scale = apply(DadosCP1, 2, sd, na.rm = TRUE))
#visualizando a matriz padronizada
dadoscp
#fazendo a ACP
pcexemplo <- PCA(dadoscp, graph = FALSE)
#visualizando o resultado
summary(pcexemplo)
#Visualiza��o somente dos autovalores
auto.val <- get_eigenvalue(pcexemplo)
auto.val
#Extraindo os resultados do acp para vari�veis
var <- get_pca_var(pcexemplo)
var
#Correla��es entre as vari�veis e os PCs
var$cor
#coordenadas das vari�veis - correla��o da vari�vel com o CP
var$coord
#Contribui��o das vari�veis nos PCs
var$contrib
#Quadrado das correla��es (cos2)- mede a qualidade da representa��o das var�veis no mapa de calor
var$cos2
#Gr�ficos
#scree plot
fviz_eig(pcexemplo, addlabels = TRUE, ylim = c(0, 50))
#Gr�fico de cargas fatoriais
fviz_pca_var(pcexemplo, col.var = "blue")
#Gr�fico de cargas fatoriais inserindo a qualidade das vari�veis como cores
fviz_pca_var(pcexemplo, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping
#Gr�fico de contribui��o das vari�veis no CP1
fviz_contrib(pcexemplo, choice = "var", axes = 1, top = 10)
#Gr�fico de contribui��o das vari�veis no CP2
fviz_contrib(pcexemplo, choice = "var", axes = 2, top = 10)
#Gr�fico dos escores individuais
fviz_pca_ind(pcexemplo)
#Gr�fico dos escores individuais com qualidade dos indiv�duos nas cores
fviz_pca_ind(pcexemplo, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
#Biplot
fviz_pca_biplot(pcexemplo, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" # Individuals color
)

