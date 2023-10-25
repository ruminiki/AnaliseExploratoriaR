library(ggplot2)
library(dplyr)
library(viridis)
library(hrbrthemes)

#+ Neste exercício vamos tentar responder:
#+ Qual o percentual de aprovação na disciplina de Calculo?
#+ Há diferenças nas médias finais e aprovações por sexo, raça e país de origem?
#+ Qual o país possui a melhor média na disciplina de cálculo?

# Para isso, crie uma coluna status:
# APR se nota >= 7 | REC se nota >= 5 e nota < 7 | REP se nota < 5

# Em seguida:
# Faça histograma da média final.
# Faça o box-plot da média final e responda:
# Quantos estudantes estão acima do terceiro quartil?

# Faça o gráfico de dispersão do ira e média final;
# Faça o mapa de correlação e identifique:
# Existe correlação entre o numero de faltas e a média final do estudante?
# Existe correlação entre a idade de ingresso no curso e a média final do estudante?

#####################################
# Carregar arquivo de dados
#####################################
df <- read.csv("data/notas_calculo.csv")

#cria variável para atribuir o status
df <- df %>% mutate(status = case_when(media_final < 5 ~ "REP",
                                       media_final >= 5 & media_final < 7 ~ "REC",
                                       media_final >= 7 ~ "APR"))

######################################################################
# Qual o percentual de aprovação na disciplina de Calculo?
######################################################################
#visualiza a distribuição por conceito
table(df$status)

#calcula o percentual de estudantes aprovados
aux <- df %>% group_by(status) %>% summarise(perc = n()/nrow(df)*100)
print(aux)

#+ GGPLOT2, é a principal biblioteca para a geração de gráficos do R.
#+ Ela funciona criando a estrutura de coordenadas, x e y, e adicionando camadas ao gráfico.
#+ Camadas podem ser barras, linhas, colunas, pontos.
#+ O primeiro argumento é o dataset, então ggplot(data = df), cria um gráfico vazio para o dataset 'df'.
#+ O segundo argumento é o aesthetic, e servem para mapear os dados para elementos visuais.
#+ 
#+ Links úteis:
#+ https://r-graph-gallery.com/
#+ https://r-charts.com/

#plota em gráfico de barras
ggplot(aux, aes(x=status, y=perc))+
  geom_bar(stat="identity")

ggplot(aux, aes(x=status, y=perc, fill=status))+
  geom_bar(stat='identity')+
  scale_fill_viridis(discrete=TRUE) +
  geom_text(aes(label=(paste(round(perc,digits = 2),"%"))), 
            color="black", position=position_dodge(width = 0.9), vjust=-0.30)+
  labs(title="",x="",y="Percentual(%)")
  
######################################################################
# Há diferenças nas médias finais e aprovações, por sexo, raça e país de origem?
######################################################################

df %>% ggplot(aes(x=media_final, fill=sexo)) +
  geom_density(alpha = 0.2) + 
  facet_wrap(~ raca)

#raça
df %>% ggplot(aes(x=status)) +
  geom_bar(alpha = 0.2) + 
  facet_wrap(~ raca)

#país
df %>% ggplot(aes(x=status)) +
  geom_bar(alpha = 0.2) + 
  facet_wrap(~ pais_origem)

######################################################################
# Qual o país possui a melhor média na disciplina de cálculo?
######################################################################

#média geral
df_media_pais <- df %>% group_by(pais_origem) %>% 
  summarise(media = mean(media_final)) %>% 
  arrange(desc(media))

ggplot(df_media_pais, aes(x = reorder(pais_origem, media), y = media)) +
     geom_bar(stat="identity", fill="#8AAAE5") +
     geom_text(aes(label=round(media, digits=2)), 
               color="black", 
               position=position_dodge(width = 0.9), vjust=-0.30)+
     labs(title="",x="",y="") +
     theme(
       axis.line = element_line(linewidth = 1, colour = "grey80"),
       axis.text.x = element_text(angle=90, hjust=1),
     plot.background = element_rect(fill="white"),
     panel.background = element_rect(fill="white"),
     panel.grid.minor.y = element_line(color="grey80"),
     legend.text = element_text(size = 10))

######################################################################
# Faça histograma da média final
######################################################################

#+ Histograma é uma representação gráfica da distribuição de frequências de uma variável quantitativa contínua. 
#+ Ele é um gráfico de barras, onde a altura de cada barra representa a frequência de ocorrência de um valor 
#+ ou intervalo de valores da variável.

df %>% ggplot(aes(x=media_final, fill=raca)) +
  geom_histogram(alpha=0.6, binwidth = 1) + 
  scale_fill_viridis(discrete=TRUE) +
  xlab("") +
  ylab("Quantidade matrículas") +
  facet_wrap(~raca)

#####################################
#Gráficos de densidade
#####################################
df %>% ggplot(aes(x=media_final)) +
  geom_density() 

#gráfico densidade média e sexo
df %>% ggplot(aes(x=media_final, fill=sexo)) +
  geom_density(alpha = 0.2) 

#gráfico densidade média, raça e sexo
df %>% ggplot(aes(x=media_final, fill=raca)) +
  geom_density(alpha = 0.2) +
  facet_grid(sexo ~ .)

######################################################################
# Faça o box-plot da média final e responda:
# Quantos estudantes estão acima do terceiro quartil?
######################################################################
#+ Conhecido como diagrama de caixa, é uma representação gráfica da distribuição de uma variável numérica. 
#+ Ele mostra a mediana, os quartis, os valores extremos e quaisquer outliers. 
#+ Um boxplot é dividido em três partes:
#+ A caixa central representa o interquartil, que é o intervalo entre o primeiro quartil (Q1) e o terceiro quartil (Q3). 
#+ A mediana (Q2) é representada por uma linha horizontal no meio da caixa.
#+ Os bigodes são as linhas que se estendem da caixa até os valores extremos. 
#+ Os valores extremos são definidos como os valores que estão fora de 1,5 vezes o interquartil.
#+ Os outliers são os pontos que estão fora dos bigodes.

ggplot(df, aes(y=media_final))+
  geom_boxplot()

ggplot(df, aes(x=sexo, y=media_final, fill=sexo))+
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) +
  facet_wrap(~ raca)

ggplot(df, aes(y=media_final))+
  geom_boxplot(alpha=0.2) + 
  facet_wrap(~ pais_origem)

#encontrando o valor da nota limite no terceiro quartil
quantile(df$media_final, probs = seq(0, 1, 0.25))

# Quantos estudantes estão acima do terceiro quartil?
df %>% filter(media_final > 5.4) %>% summarise(total = n())

######################################################################
# Faça o gráfico de dispersão do ira e média final;
# Faça o mapa de correlação e identifique:
# Existe correlação entre o numero de faltas e a média final do estudante?
# Existe correlação entre a idade de ingresso no curso e a média final do estudante?
######################################################################
ggplot(df, aes(x=media_final, y=ira))+
  geom_point(alpha=0.2)

ggplot(df, aes(x=media_final, y=numero_faltas))+
  geom_point(alpha=0.2)

ggplot(df, aes(x=media_final, y=idade_ingresso_curso))+
  geom_point(alpha=0.2)

#################### CORRELAÇÃO ####################
#+ O Coeficiente de Correlação indica a força e a direção 
#+ da relação entre duas variáveis. Em geral, 
#+ a característica é boa se tiver uma forte correlação com a variável resposta 
#+ e não tenha forte correlação com outras características.
###################################################

#gera um dataframe somente com as variáveis numéricas
df.cor <- df[,colnames(select_if(df, is.numeric))]

# Cria a matriz de correlação
df.cor <- cor(df.cor)
#plota o mapa das correlações
corrplot(df.cor, method = 'square', sig.level = 0.05)

#outro método de apresentar a correlação, nesse caso a função 
#chart.Correlation calcula a correlação e plota o gráfico
chart.Correlation(df.cor, histogram=FALSE, method = "pearson", pch=25)

