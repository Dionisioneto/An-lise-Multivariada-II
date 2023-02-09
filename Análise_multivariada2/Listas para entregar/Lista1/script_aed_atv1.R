## ------
## Script da atividade 1 de multivariada
## ------

## ---
## Pacotes
## ---

if(!require(pacman)) install.packages("pacman")

p_load(readxl, dplyr, rstatix, corrplot, GGally,
       psych, PerformanceAnalytics, lattice, graphics, sm, gmodels,
       MVN, ggplot2, ggExtra,openxlsx)

## ---
## Banco de dados
## ---

setwd("D:/")

dir()

## ---
## leitura dos dados
## ---

## dados com os cabecalhos e as duas medicoes
data <- read.xlsx(xlsxFile = "dados_long_wide.xlsx", 
                  fillMergedCells = TRUE, colNames = T)

## tamanho dos dados com as medicoes  
n = dim(data)[1]
head(data)  # visualizando o cabecalho
tail(data)  # visualizando as ultimas linhas

jejum = data[-1,1:3] ## tirando a primeira linha com o nome das variaveis (y1, y2, e y3)
apos = data[-1,4:6]  ## tirando a primeira linha com o nome das variaveis (x1, x2, e x3)

## colocando o mesmo cabecalho para juntar as linhas
colnames(jejum) = c("y1", "y2", "y3")
colnames(apos) = c("y1", "y2", "y3")

## colocando os labels para identificar as medicoes
## 0: no jejum
## 1: apos 1 hora

jejum["ingestao"] = rep(0,n - 1) ## retirando uma linha pois a primeira, pois identifica
apos["ingestao"] = rep(1, n - 1) ## as variaveis

## juntando as linhas
dados = rbind(jejum, apos)
head(dados)

## ajustando o index
rownames(dados) = 1:(dim(dados)[1])


dados

## Transformando os dados 
dados$y1 = as.numeric(dados$y1)
dados$y2 = as.numeric(dados$y2)
dados$y3 = as.numeric(dados$y3)
dados$ingestao = as.factor(dados$ingestao)

write.xlsx(dados, file = "tabela_long.xlsx")

## ---
## Analise descritiva
## ---

colMeans(dados[,1:3]) # vetor de medias 
cov(dados[,1:3])      # matriz de covariancias
cor(dados[,1:3])      # matriz de correlacoes

## Vetores de medias por ingestao
dados %>% group_by(ingestao) %>% 
  summarise(n = n(), mean_y1 = mean(y1),
            mean_y2 = mean(y2), mean_y3 = mean(y3))

## Matrizes de covariancias por ingestao

dados %>% group_by(ingestao) %>% 
  do(data.frame(cov = t(cov(.[,1:3]))))

## Matrizes de correlacoes por ingestao
dados %>% group_by((ingestao)) %>% 
  do(data.frame(cov = t(cor(.[,1:3]))))

## ---
## Construcao de graficos
## ---

# grafico qqplot


g1 = ggplot(data = dados, aes(sample= y1)) +
  stat_qq() + 
  stat_qq_line(color = 'red') + theme_minimal()

g1 + scale_x_continuous("Teórico") +
  scale_y_continuous("Observado") +
  theme(axis.text.x = element_text(color="black", 
                                   size=14),
        axis.text.y = element_text(color="black", 
                                   size=14),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed")
  ) 

ggsave("qqplot_y1.jpeg",  width = 15, height = 10,units = "cm",dpi = 300)


g2 = ggplot(data = dados, aes(sample= y2)) +
  stat_qq() + 
  stat_qq_line(color = 'red') + theme_minimal()
  
g2 + scale_x_continuous("Teórico") +
  scale_y_continuous("Observado") +
  theme(axis.text.x = element_text(color="black", 
                                   size=14),
        axis.text.y = element_text(color="black", 
                                   size=14),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed")
        ) 

ggsave("qqplot_y2.jpeg",  width = 15, height = 10,units = "cm",dpi = 300)

g3 = ggplot(data = dados, aes(sample= y3)) +
  stat_qq() + 
  stat_qq_line(color = 'red') + theme_minimal()

g3 + scale_x_continuous("Teórico") +
  scale_y_continuous("Observado") +
  theme(axis.text.x = element_text(color="black", 
                                   size=14),
        axis.text.y = element_text(color="black", 
                                   size=14),
        axis.title=element_text(size=14,face="bold"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed")
  ) 

ggsave("qqplot_y3.jpeg",  width = 15, height = 10,units = "cm",dpi = 300)


## grafico de correlacoes

jpeg("correlacao.jpeg", units="in", width=5, height=5, res=300)
corrplot(cor(dados[,1:3]), method = 'color',type = 'lower',
         cl.pos = 'b', addCoef.col = 'white', xlab ='hhh')
dev.off()


## graficos de dispersao multivariados

jpeg("dispersao_pares.jpeg", units="in", width=4, height=4, res=400)
pairs(dados[,1:3], pch = 16, col = c('red', 'blue')[dados$ingestao])

dev.off()

ggpairs(dados[,1:3],
        ggplot2::aes(color = factor(dados$ingestao), alpha = 0.7)) +
  theme_minimal()

jpeg("paineis_pares.jpeg", units="in", width=4, height=4, res=400)
pairs.panels(dados[,1:3],
             pch = 21,
             bg = c("red","blue")[dados$ingestao],
             lm = T)
dev.off()

chart.Correlation(dados[,1:3]) ## mlgs, histogramas e correlacoes
chart.Boxplot(dados[,1:3])

## grafico de estrela
stars(dados[,1:3],key.loc = c(11,2))



## boxplots
boxplot(dados[,1:3])

boxplot(dados$y1~factor(dados$ingestao))
boxplot(dados$y2~factor(dados$ingestao))
boxplot(dados$y3~factor(dados$ingestao))

## y1: primeira semana
b1 = ggplot() + aes(dados,
               x = dados$ingestao,
               y = dados$y1,
               fill = dados$ingestao)  + 
  geom_boxplot(width = 0.5) + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") + 
  scale_x_discrete("Situação", labels = c("Jejum", "1 Hora após")) +
  scale_y_continuous("Medição de glicose na primeira semana") + 
  theme_minimal()

b1 + theme(axis.text.x = element_text(color="black", 
                                      size=22),
           axis.text.y = element_text(color="black", 
                                      size=22),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20)) 

ggsave("boxplot_y1.jpeg", width = 20, height = 10, dpi = 800)

## y2: segunda semana

b2 = ggplot() + aes(dados,
               x = dados$ingestao,
               y = dados$y2,
               fill = dados$ingestao)  + 
  geom_boxplot(width = 0.5) + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") + 
  scale_x_discrete("Situação", labels = c("Jejum", "1 Hora após")) +
  scale_y_continuous("Medição de glicose na segunda semana") + 
  theme_minimal()

b2 + theme(axis.text.x = element_text(color="black", 
                                      size=22),
           axis.text.y = element_text(color="black", 
                                      size=22),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20))

ggsave("boxplot_y2.jpeg", width = 20, height = 10, dpi = 800)
## y3: terceira semana

b3 = ggplot() + aes(dados,
               x = dados$ingestao,
               y = dados$y3,
               fill = dados$ingestao)  + 
  geom_boxplot(width = 0.5) + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") + 
  scale_x_discrete("Situação", labels = c("Jejum", "1 Hora após")) +
  scale_y_continuous("Medição de glicose na terceira semana") + 
  theme_minimal()

b3 + theme(axis.text.x = element_text(color="black", 
                                      size=22),
           axis.text.y = element_text(color="black", 
                                      size=22),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20))

ggsave("boxplot_y3.jpeg", width = 20, height = 10, dpi = 800)

## grafico de violino

# primeira semana
v1 = ggplot() + aes(y = dados$y1,
               x = dados$ingestao,
               fill = dados$ingestao) +
  geom_violin(alpha = 0.7) + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") + 
  scale_y_continuous("Medição de glicose na primeira semana") +
  scale_x_discrete("Situação", labels = c("Jejum", "1 Hora após")) + 
  theme_minimal()

v1 + theme(axis.text.x = element_text(color="black", 
                                      size=22),
           axis.text.y = element_text(color="black", 
                                      size=22),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20))
ggsave("violino_y1.jpeg", width = 20, height = 10, dpi = 800)

# segunda semana
v2 = ggplot() + aes(y = dados$y2,
               x = dados$ingestao,
               fill = dados$ingestao) +
  geom_violin(alpha = 0.7) + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") + 
  scale_y_continuous("Medição de glicose na segunda semana") +
  scale_x_discrete("Situação", labels = c("Jejum", "1 Hora após")) + 
  theme_minimal()

v2 + theme(axis.text.x = element_text(color="black", 
                                      size=22),
           axis.text.y = element_text(color="black", 
                                      size=22),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20))
ggsave("violino_y2.jpeg", width = 20, height = 10, dpi = 800)

# terceira semana
v3 = ggplot() + aes(y = dados$y3,
               x = dados$ingestao,
               fill = dados$ingestao) +
  geom_violin(alpha = 0.7) + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") + 
  scale_y_continuous("Medição de glicose na terceira semana") +
  scale_x_discrete("Situação", labels = c("Jejum", "1 Hora após")) + 
  theme_minimal()

v2 + theme(axis.text.x = element_text(color="black", 
                                 size=22),
      axis.text.y = element_text(color="black", 
                                 size=22),
      axis.title=element_text(size=26,face="bold"),
      panel.grid.major = element_line(linetype = "dashed"),
      panel.grid.minor = element_line(linetype = "dashed"),
      legend.title=element_text(size=22), 
      legend.text=element_text(size=20))
ggsave("violino_y3.jpeg", width = 20, height = 10, dpi = 800)
## grafico de densidade

## y1: primeira semana

d1 = dados %>% ggplot() + aes(x = y1, fill = ingestao) +
  geom_density(alpha =0.7) +
  scale_x_continuous("Medição de glicose na primeira semana") +
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") +
  scale_y_continuous("Densidade") +
  theme_minimal()

d1 + theme(axis.text.x = element_text(color="black", 
                                      size=16),
           axis.text.y = element_text(color="black", 
                                      size=16),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20)
) 
ggsave("densidade_y1.jpeg", width = 20, height = 10, dpi = 800)



  


## y2: segunda semana
d2 = dados %>% ggplot() + aes(x = y2, fill = ingestao) +
  geom_density(alpha =0.7) +
  scale_x_continuous("Medição de glicose na segunda semana") +
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") +
  scale_y_continuous("Densidade") +
  theme_minimal()

d2 + theme(axis.text.x = element_text(color="black", 
                                     size=16),
          axis.text.y = element_text(color="black", 
                                     size=16),
          axis.title=element_text(size=26,face="bold"),
          panel.grid.major = element_line(linetype = "dashed"),
          panel.grid.minor = element_line(linetype = "dashed"),
          legend.title=element_text(size=22), 
          legend.text=element_text(size=20)
) 
ggsave("densidade_y2.jpeg", width = 20, height = 10, dpi = 800)
  
  
## y3: terceira semana
d3 = dados %>% ggplot() + aes(x = y3, fill = ingestao) +
  geom_density(alpha =0.7) +
  scale_x_continuous("Medição de glicose na terceira semana") +
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") +
  scale_y_continuous("Densidade") +
  theme_minimal()

d3 + theme(axis.text.x = element_text(color="black", 
                                      size=16),
           axis.text.y = element_text(color="black", 
                                      size=16),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20)
) 
ggsave("densidade_y3.jpeg", width = 20, height = 10, dpi = 800)

## dipersao com densidades marginais

# primeira e segunda semana

p = ggplot() + aes(dados, x = dados$y1,
               y = dados$y2,
               colour = dados$ingestao) + theme_minimal()

p = p + geom_point(size = 1.5)  +
  scale_color_discrete(name = "Situação", 
                       labels = c("Jejum", '1 Hora após')) +
  scale_y_continuous("Medição de glicose na primeira semana") +
  scale_x_continuous("Medição de glicose na segunda semana") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(color="black", 
                                                               size=12),
        axis.text.y = element_text(color="black", 
                                   size=12),
        axis.title=element_text(size=16,face="bold"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10))

jpeg("dispersao_densidade_marginal_y1_y2.jpeg", units="in", width=8, height=5, res=300)
ggMarginal(p, type = 'histogram', bins = 10)
dev.off()

#ggMarginal(p , type = 'density')
#ggMarginal(p , type = 'boxplot')

# primeira e terceira semana
p2 = ggplot() + aes(dados, x = dados$y1,
                   y = dados$y3,
                   colour = dados$ingestao) + theme_minimal()

p2 = p2 + geom_point(size = 1.5)  +
  scale_color_discrete(name = "Situação", 
                       labels = c("Jejum", '1 Hora após')) +
  scale_y_continuous("Medição de glicose na primeira semana") +
  scale_x_continuous("Medição de glicose na terceira semana") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(color="black", 
                                                               size=12),
        axis.text.y = element_text(color="black", 
                                   size=12),
        axis.title=element_text(size=16,face="bold"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10))

jpeg("dispersao_densidade_marginal_y1_y3.jpeg", units="in", width=8, height=5, res=300)
ggMarginal(p2, type = 'histogram', bins = 10)
dev.off()
# segunda e terceira semana
p3 = ggplot() + aes(dados, x = dados$y2,
                    y = dados$y3,
                    colour = dados$ingestao) + theme_minimal()

p3 = p3 + geom_point(size = 1.5)  +
  scale_color_discrete(name = "Situação", 
                       labels = c("Jejum", '1 Hora após')) +
  scale_y_continuous("Medição de glicose na segunda semana") +
  scale_x_continuous("Medição de glicose na terceira semana") + 
  theme(legend.position = 'bottom', axis.text.x = element_text(color="black", 
                                                               size=12),
        axis.text.y = element_text(color="black", 
                                   size=12),
        axis.title=element_text(size=16,face="bold"),
        panel.grid.major = element_line(linetype = "dashed"),
        panel.grid.minor = element_line(linetype = "dashed"),
        legend.title=element_text(size=10), 
        legend.text=element_text(size=10))

jpeg("dispersao_densidade_marginal_y2_y3.jpeg", units="in", width=8, height=5, res=300)
ggMarginal(p3, type = 'histogram', bins = 10)
dev.off()



# histograma
h1 = dados %>% 
  ggplot(aes(x = y1, fill = ingestao)) +
  geom_histogram(position = 'identity', bins = 10,
                 alpha = 0.6) + 
  theme_minimal() + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                                        name = "Situação") +
  scale_x_continuous("Medição de glicose na primeira semana") +
  scale_y_continuous("Frequência")

h1 + theme(axis.text.x = element_text(color="black", 
                                      size=16),
           axis.text.y = element_text(color="black", 
                                      size=16),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20)
) 

ggsave("histograma_y1.jpeg", width = 20, height = 10, dpi = 800)



h2 = dados %>% 
  ggplot(aes(x = y2, fill = ingestao)) +
  geom_histogram(position = 'identity', bins = 10,
                 alpha = 0.6) + 
  theme_minimal() + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") +
  scale_x_continuous("Medição de glicose na segunda semana") +
  scale_y_continuous("Frequência")

h2 + theme(axis.text.x = element_text(color="black", 
                                 size=16),
      axis.text.y = element_text(color="black", 
                                 size=16),
      axis.title=element_text(size=26,face="bold"),
      panel.grid.major = element_line(linetype = "dashed"),
      panel.grid.minor = element_line(linetype = "dashed"),
      legend.title=element_text(size=22), 
      legend.text=element_text(size=20)
) 

ggsave("histograma_y2.jpeg", width = 20, height = 10, dpi = 800)

h3 = dados %>% 
  ggplot(aes(x = y3, fill = ingestao)) +
  geom_histogram(position = 'identity', bins = 10,
                 alpha = 0.6) + 
  theme_minimal() + 
  scale_fill_discrete(labels = c("Jejum", '1 Hora após'),
                      name = "Situação") +
  scale_x_continuous("Medição de glicose na terceira semana") +
  scale_y_continuous("Frequência")

h3 + theme(axis.text.x = element_text(color="black", 
                                      size=16),
           axis.text.y = element_text(color="black", 
                                      size=16),
           axis.title=element_text(size=26,face="bold"),
           panel.grid.major = element_line(linetype = "dashed"),
           panel.grid.minor = element_line(linetype = "dashed"),
           legend.title=element_text(size=22), 
           legend.text=element_text(size=20)
) 

ggsave("histograma_y3.jpeg", width = 200, height = 100, dpi = 800)



## ---
## Teste de normalidade bidimensional
## ---

colnames(dados)

## para y1 e y2
 
mvn(cbind(dados$y1, dados$y2), mvnTest = 'royston')# nao

mvn(cbind(dados$y1, dados$y2), mvnTest = 'mardia') # nao
 
mvn(cbind(dados$y1, dados$y2), mvnTest = 'dh')     # nao

mvn(cbind(dados$y1, dados$y2), mvnTest = 'hz')     # nao

mvn(cbind(dados$y1, dados$y2), mvnTest = 'energy') # nao

## para y2 e y3

mvn(cbind(dados$y3, dados$y2), mvnTest = 'royston')# nao 

mvn(cbind(dados$y3, dados$y2), mvnTest = 'mardia') # nao
 
mvn(cbind(dados$y3, dados$y2), mvnTest = 'dh')     # nao

mvn(cbind(dados$y3, dados$y2), mvnTest = 'hz')     # nao

mvn(cbind(dados$y3, dados$y2), mvnTest = 'energy') # nao

## para y1 e y3

mvn(cbind(dados$y3, dados$y1), mvnTest = 'royston')# nao 

mvn(cbind(dados$y3, dados$y1), mvnTest = 'mardia') # nao

mvn(cbind(dados$y3, dados$y1), mvnTest = 'dh')     # nao

mvn(cbind(dados$y3, dados$y1), mvnTest = 'hz')     # nao

mvn(cbind(dados$y3, dados$y1), mvnTest = 'energy') # nao











