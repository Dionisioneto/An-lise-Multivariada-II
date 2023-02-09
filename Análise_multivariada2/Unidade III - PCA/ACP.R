#APC com o pacote FactorMineR e factorextra
#Matriz de covariâncias das variáveis originais
S = cov(DadosCP1)
#visualizando a matriz S
S
#Matriz de correlação das variáveis originais
R = cor(DadosCP1)
#visualizando a matriz R
R
#padronizando os dados - padronização para variância 1 e média qualquer
dadoscp=scale(DadosCP1, center = FALSE, scale = apply(DadosCP1, 2, sd, na.rm = TRUE))
#visualizando a matriz padronizada
dadoscp
#fazendo a ACP
pcexemplo <- PCA(dadoscp, graph = FALSE)
#visualizando o resultado
summary(pcexemplo)
#Visualização somente dos autovalores
auto.val <- get_eigenvalue(pcexemplo)
auto.val
#Extraindo os resultados do acp para variáveis
var <- get_pca_var(pcexemplo)
var
#Correlações entre as variáveis e os PCs
var$cor
#coordenadas das variáveis - correlação da variável com o CP
var$coord
#Contribuição das variáveis nos PCs
var$contrib
#Quadrado das correlações (cos2)- mede a qualidade da representação das varáveis no mapa de calor
var$cos2
#Gráficos
#scree plot
fviz_eig(pcexemplo, addlabels = TRUE, ylim = c(0, 50))
#Gráfico de cargas fatoriais
fviz_pca_var(pcexemplo, col.var = "blue")
#Gráfico de cargas fatoriais inserindo a qualidade das variáveis como cores
fviz_pca_var(pcexemplo, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # Avoid text overlapping
#Gráfico de contribuição das variáveis no CP1
fviz_contrib(pcexemplo, choice = "var", axes = 1, top = 10)
#Gráfico de contribuição das variáveis no CP2
fviz_contrib(pcexemplo, choice = "var", axes = 2, top = 10)
#Gráfico dos escores individuais
fviz_pca_ind(pcexemplo)
#Gráfico dos escores individuais com qualidade dos indivíduos nas cores
fviz_pca_ind(pcexemplo, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
#Biplot
fviz_pca_biplot(pcexemplo, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969" # Individuals color
)

