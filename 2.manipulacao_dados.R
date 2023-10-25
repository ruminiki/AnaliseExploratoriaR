library(dplyr)

#################################
# Carregar arquivo de dados
#################################
#+ O dataset é uma amostra de estudantes que cursaram a disciplina de Cálculo I
#+ não deve ser utilizado para inferências sobre todo o universo de estudantes.

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

#------------------- Conceitos básicos de manipulação de dados -----------------

# A manipulação dos dados consiste em organizar as variáveis e observações
# Na grande maioria dos casos, as bases de dados precisam ser preparadas

#Visualizar dataframe
#str | glimpse | head | nrow | colnames | dim | names | table
str(df) #estrutura do dataset
glimpse(df) #visão geral, tipos de dados e primeiros registros
head(df, n=10) #exibe os n primeiros registros
nrow(df) #número de linhas do dataset
ncol(df) #número de colunas do dataset
colnames(df) # =names(df)
dim(df) #dimensões do dataset
table(df$sexo) 
table(df$raca, df$sexo)
summary(df) #resumo estatístico do dataset
boxplot(df$idade_ingresso_curso) #outlier
unique(df$pais_origem) #lista os valores únicos de uma coluna

####### ACESSANDO VARIÁVEIS DO CONJUNTO DE DADOS #######
df$periodo_atual #retorna um vetor
df['periodo_atual'] #retorna um dataframe
df[2] #pela posição da coluna, retorna dataframe

#primeira linha, terceira coluna
df[1,3]

#quinta linha, todas as colunas
df[5, ]

#média de notas das observações no intervalo
df[c(15:20), 'media_final']

#a seleção pode ser feita por exclusão
df[,-2]

# Também é possível filtrar observações por meio dos operadores:
# Alguns operadores úteis para realizar filtros:

"== igual"
"> maior"
">= maior ou igual"
"< menor"
"<= menor ou igual"
"!= diferente"
"& indica e"
"| indica ou"

df1 <- df[df$media_final >= 8,]

#percentual de alunos com nota maior que 8
count(df[df$media_final >= 8,]) / nrow(df) * 100

#alunos do brasil, com média maior ou igual a 8
brasil_media_maior_8 <- df[df$pais_origem == "Brasil" & df$media_final >= 8,]

#percentual de alunos do brasil com media >= 8
count(brasil_media_maior_8) / nrow(df[df$pais_origem == "Brasil",]) * 100

#raça de estudantes brasileiros
brasil <- df[df$pais_origem == 'Brasil', c('raca', 'pais_origem')]

#tabela de contingencia
table(brasil$raca, brasil$pais_origem)

#percentual de alunos brasileiros por raça
percentual_raca <- as.data.frame(table(brasil$raca, brasil$pais_origem))
names(percentual_raca) <- c("raca", "pais", "total")

percentual_raca$percentual <- percentual_raca$total / sum(percentual_raca$total) * 100
view(percentual_raca)

############# ALTERANDO O CONJUNTO DE DADOS ############

#criando uma nova coluna para indicar se o estudante está reprovado por faltas
percentual_faltas <- (df$numero_faltas / df$ch_total)*100
percentual_faltas <- round(percentual_faltas, digits = 2)

#criando a coluna com o percentual de faltas
df$percentual_faltas <- percentual_faltas #ou df <- mutate(df,percentual_faltas)

#criando a coluna que indica se o estudante está reprovado (reprovado se faltas > 25%)
df$reprovado_faltas <- ifelse(df$percentual_faltas > 25, T, F)

#verifica quantos estão reprovados
table(df$reprovado_faltas)

#####################################
# DPLyR
#####################################

#+ O dplyr é um pacote de R que fornece uma interface para a manipulação de dados. 
#+ Ele é baseado na ideia de uma "gramática de manipulação de dados", que consiste 
#+ em uma série de verbos que podem ser usados para realizar tarefas comuns de manipulação de dados.
#+ 
#+ Nesta sessão vamos aprender a usar os cinco principais verbos:
#+ filter()
#+ arrange()
#+ select()
#+ mutate()
#+ summarise()

#reset dataset
df <- read.csv("data/notas_calculo.csv")

########## Filter #####################
#rbase
df[df$pais_origem == "Brasil",]

#dplyr
filter(df, pais_origem == "Brasil")

#usando pipe (%>%)
df %>% filter(pais_origem == "Brasil")
df %>% filter(pais_origem %in% c("Brasil", "Paraguai"))
df %>% filter(between(numero_faltas, 10,50))
df %>% filter(media_final >= mean(media_final, na.rm = T))

########## Select #####################

df2 <- df %>% select(pais_origem, raca, sexo, media_final, numero_faltas, ch_total)
df3 <- df %>% select(everything(), -raca, -pais_origem)
df4 <- df %>% select(periodo_atual:ira)

########## Arrange ###################
# A função "arrange" faz a ordenação do dataset
# Se retirar o desc(), fica na ordem crescente
df5 <- df %>% select(pais_origem, numero_faltas, media_final) %>% arrange(desc(media_final))

########## Mutate ##################
#cria nova coluna, com o percentual de faltas calculado 
df <- df %>% 
  mutate(percentual_faltas = round((numero_faltas / ch_total) * 100, digits = 2)) %>%
  mutate(reprovado_faltas = ifelse(percentual_faltas > 25, T, F))

#cria variável para atribuir um conceito a nota
df <- df %>% mutate(conceito_nota = case_when(media_final < 5 ~ "D",
                                              media_final >= 5 & media_final < 7 ~ "C",
                                              media_final >= 7 & media_final < 8.5 ~ "B",
                                              media_final >= 8.5 ~ "A"))

#visualiza a distribuição
table(df$conceito)

########## Summarise##################
#Cria um resumo dos dados, é util em conjunto com a função de grupo group_by

#calcula a media de notas por pais de origem, ordenados pela maior média
df %>% group_by(pais_origem) %>% 
  summarise(media_notas = mean(media_final, na.rm = T))

#média e mediana por raça
df %>% group_by(raca) %>% 
  summarise(media_notas = mean(media_final, na.rm = T),
            mediana = median(media_final))

#média e mediana por sexo
df %>% group_by(sexo) %>% 
  summarise(media_notas = mean(media_final, na.rm = T),
            mediana = median(media_final))

#média e mediana por sexo
df %>% group_by(pais_origem) %>% 
  summarise(media_notas = mean(media_final, na.rm = T),
            mediana = median(media_final))
