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
