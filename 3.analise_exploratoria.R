library(ggplot2)
library(dplyr)

#####################################
# Carregaro arquivo de dados
#####################################
df <- read.csv("data/notas_calculo.csv")

# aprovações por sexo
# boxplot media_final por pais
# boxplot media_final por pais
# grafico dispersão numero faltas e media_final
# ...



glimpse(df)

df %>%
  ggplot(aes(media_final)) +
  geom_density(adjust=1.5, alpha = 0.2) +
  facet_grid(~raca)


Summarise
#media por determinada coluna
nova_base %>% 
  group_by(periodo) %>% 
  summarise(mean(tempo))

Mutate
Arrange
Contagem
nova_base %>%  count(periodo)
dados_freq_2 <- nova_base %>% 
  count(periodo, perfil, name = "contagem")

results <- test %>%
  select(situacao) %>%
  
  Estatística descritiva
descritivas_nova_base <- summarise(nova_base,
                                   observações = n(),
                                   média = mean(tempo),
                                   mediana = median(tempo),
                                   desv_pad = sd(tempo),
                                   mínimo = min(tempo),
                                   máximo = max(tempo),
                                   quartil_3 = quantile(tempo, probs = 0.75))

descritivas_condic <- nova_base %>% 
  filter(tempo > 20) %>% 
  group_by(perfil) %>% 
  summarise(observações = sum(!is.na(distancia)),
            média = mean(distancia, na.rm = T),
            mediana = median(distancia, na.rm = T),
            desv_pad = sd(distancia, na.rm = T),
            mínimo = min(distancia, na.rm = T),
            máximo = max(distancia, na.rm = T),
            quartil_1 = quantile(distancia, probs = 0.25, na.rm = T),
            quartil_3 = quantile(distancia, probs = 0.75, na.rm = T)) %>% 
  arrange(média)

summary()
descritivas_base_grupo <- base_grupo %>% 
  summarise(média = mean(tempo),
            desvio_pad = sd(tempo),
            n_obs = n())


Análise gráfica
hist()
boxplot()
plot()


Gráficos GGplot2
(mais de um gráfico por janela)
Barras
https://blog.albertkuo.me/post/2022-01-04-reordering-geom-col-and-geom-bar-by-count-or-value/
  Densidade
Histograma
Linhas
Box-plot
Wordcloud (formulários, qual sua expectativa com a semana do servidor?)



