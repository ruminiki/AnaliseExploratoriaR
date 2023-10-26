library(factoextra)
library(ggplot2)
library(viridis)
library(hrbrthemes)

#####################################
# Carregar arquivo de dados
#####################################
df <- read.csv("data/notas_calculo.csv")

#cria variável para atribuir o status
df <- df %>% mutate(status = case_when(media_final < 5 ~ "REP",
                                       media_final >= 5 & media_final < 7 ~ "REC",
                                       media_final >= 7 ~ "APR"))

################################################
set.seed(1)
# Elaboração da clusterização não hieráquica k-means
cluster_kmeans <- kmeans(scale(df[,colnames(select_if(df, is.numeric))]), centers = 4)

#visualização dos clusters
fviz_cluster(cluster_kmeans, scale(df[,colnames(select_if(df, is.numeric))]), geom = "point", ellipse.type = "norm", repel=TRUE)

# Criando variável categórica para indicação do cluster no banco de dados
df$cluster <- cluster_kmeans$cluster

################################################
# status em cada cluster
ggplot(as.data.frame(table(df$status, df$cluster)),
       aes(x=Var2, y=Freq, fill=Var1)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("Cluster") + ylab("Frequência") + 
  theme(legend.position = c(0.5, -0.1), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.title = element_blank())

#estatísticas nos clusters
table(df$cluster)
table(df$pais_origem, df$cluster)
table(df$sexo, df$cluster)

df %>% group_by(cluster) %>% 
  summarise(mean_idade = mean(idade_ingresso_curso),
            mean_ira = mean(ira),
            mean_nota = mean(media_final),
            mean_numero_faltas = mean(numero_faltas))
