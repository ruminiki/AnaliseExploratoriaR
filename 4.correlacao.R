# Load the necessary libraries
library(base)
library(stats)
library(corrplot)
library(Hmisc)
library(PerformanceAnalytics)

#################################
# Carregaro arquivo de dados
#################################
df <- read.csv("data/notas_calculo.csv")

#TRATAR os NAs
#+existem algumas formas de tratar dados ausentes
#+ 1. remover as observações ou variável (depende do contexto e da quantidade)
#+ 2. imputar valores, como média, mediana
#+ 3. usar algum método para prever o valor, exemplo de correlação linear, árvore de decisão

#contar quantos NAs
colSums(is.na(df))
df$media_final[is.na(df$media_final)] <- mean(df$media_final, na.rm = TRUE)
df$numero_faltas[is.na(df$numero_faltas)] <- mean(df$numero_faltas, na.rm = TRUE)

#confere se ainda existem NAs
colSums(is.na(df)) 

##################################################################################
#                         ANÁLISE GRÁFICA                                        #
##################################################################################

#Estatísticas univariadas
summary(df)

# gráfico de dispersão Mortalidade x Saneamento
ggplot(df, aes(x = numero_faltas, y = media_final)) +
  geom_point(color = "#39568CFF", size = 1.2) +
  labs(x = "Ira", y = "Média Final")+
  scale_color_manual("Legenda:", values = "grey50") +
  theme_classic()

df %>% 
  ggplot(aes(x = carga_horaria_sucesso, group=situacao, fill=situacao)) + 
  geom_density(adjust=1.5, alpha = 0.2) + 
  scale_x_continuous(trans="log2") 

#################### CORRELAÇÃO ####################
#+ O Coeficiente de Correlação indica a força e a direção 
#+ da relação entre duas variáveis. Em geral, 
#+ a característica é boa se tiver uma forte correlação com a variável resposta 
#+ e não tenha forte correlação com outras características.
###################################################

#gera um dataframe somente com as variáveis numéricas
df <- df[,c(1,2,4,8,9,10,11)]

# Cria a matriz de correlação
df.cor <- cor(df)
#plota o mapa das correlações
corrplot(df.cor, method = 'square', sig.level = 0.05)

#outro método de apresentar a correlação, nesse caso a função 
#chart.Correlation calcula a correlação e plota o gráfico
chart.Correlation(df, histogram=FALSE, method = "pearson", pch=25)

##################################################
