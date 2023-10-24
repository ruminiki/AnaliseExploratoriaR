##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################

library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(plotly)
library(jtools)
library(kableExtra)
library(equatiomatic)
library(correlation)
library(see)
library(ggraph)
library(PerformanceAnalytics)
library(nortest)
library(lavaan)
library(qgraph)
library(olsrr)
library(car)
library(mctest)

#data from 

#ELETRICIDADE: percentage of population with access to electricity.
#CORRUPCAO: perceptions of the extent to which public power is exercised for private gain. Ranging from approximately -2.5 to 2.5.
#SANEAMENTO: percentage of population using at least basic sanitation services
#MORTALIDADE INFANTIL: Infant mortality rate is the number of infants dying before reaching one year of age, per 1,000 live births in a given year.

#carrega o dataset
dataset <- read.csv("world_data.csv", sep = ",")
glimpse(dataset)

# Informações básicas do banco de dados
dim(dataset)
glimpse(dataset)

for(i in 2003:2022){
  var <- paste("X",i,"..YR",i,".", sep="")
  dataset[var] <- as.double(unlist(dataset[var]))
}

dataset <- mutate(dataset, mean = rowMeans(select(dataset,c(5:24)), na.rm = TRUE))
dataset <- dataset %>% select(Country.Name, Series.Name, mean)

dataset <- dataset %>% 
  rename(pais = 1,
         serie = 2,
         indicador = 3)

dataset <- dataset[1:868,]

dataset <- pivot_wider(dataset,
                       id_cols = "pais",
                       names_from = "serie",
                       values_from = "indicador")

dataset <- dataset %>% 
  rename(pais = 1,
         eletricidade = 2,
         corrupcao = 3,
         mortalidade = 4,
         saneamento = 5)

#dataset <- dataset %>%
#  mutate(eletricidade = 100 - eletricidade) %>%
#  mutate(saneamento = 100 - saneamento) %>%
#  mutate(corrupcao = 2.5 - corrupcao)
  
glimpse(dataset)
#remove nulos
dataset <- na.omit(dataset)

##################################################################################
#                         GRÁFICOS DE DISPERSÃO                                  #
##################################################################################

#Estatísticas univariadas
summary(dataset)

# gráfico de dispersão Mortalidade x Saneamento
ggplotly(
  ggplot(dataset, aes(x = saneamento, y = mortalidade)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Saneamento", y = "Mortalidade Infantil")+
    scale_color_manual("Legenda:", values = "grey50") +
    theme_classic()
)

# gráfico de dispersão Mortalidade x Eletricidade
ggplotly(
  ggplot(dataset, aes(x = eletricidade, y = mortalidade)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Eletricidade", y = "Mortalidade Infantil")+
    scale_color_manual("Legenda:", values = "grey50") +
    theme_classic()
)

# gráfico de dispersão Mortalidade x Corrupção
ggplotly(
  ggplot(dataset, aes(x = corrupcao, y = mortalidade)) +
    geom_point(color = "#39568CFF", size = 2.5) +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", formula = y ~ x, se = F, size = 2) +
    labs(x = "Corrupção", y = "Mortalidade Infantil")+
    scale_color_manual("Legenda:", values = "grey50") +
    theme_classic()
)


################################################################################
#           MODELAGEM DE UMA REGRESSÃO LINEAR                                  #
################################################################################
#Estimando o modelo
modelo <- lm(formula = mortalidade ~ saneamento + corrupcao + eletricidade,
             data = dataset)

#Observando os parâmetros do modelo_tempodist
summary(modelo)

#Residual Standart Error
summary(modelo)$sigma / mean(dataset$mortalidade)

step_modelo <- step(modelo, k = 3.841459)
summary(step_modelo)


#Visualização do modelo no ambiente Viewer
#função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)


#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
dataset$ypred <- modelo$fitted.values
dataset$erro <- modelo$residuals


#Visualizando a base de dados com as variáveis yhat e erro
dataset %>%
  select(mortalidade, ypred, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)


################################################################################
#                     DIAGNÓSTICO DE MULTICOLINEARIDADE                        #
################################################################################
#magnitude das correlações entre elas
#Requer instalação e carregamento dos pacotes 'see' e 'ggraph' para a plotagem
#dataset %>% select(saneamento, corrupcao, eletricidade, mortalidade) %>%
#  correlation(method = "pearson") %>%
#  plot()

#A função 'chart.Correlation' do pacote 'PerformanceAnalytics' apresenta as
#distribuições das variáveis, scatters, valores das correlações e suas
#respectivas significâncias
chart.Correlation((dataset[2:5]), histogram = TRUE)

#magnitude das correlações entre as variáveis
dataset %>% 
  correlation() %>%
  qgraph(details = T, # shows details
         mar = c(6,10,6,10), # size of graph
         vsize = 8, # size of nodes
         title = "Q Graph of All Correlations") # title


#Variance Inflation Factor (VIF > 10) 
vif(modelo)


################################################################################
#                     DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #
################################################################################

#verificar a dispersão dos resíduos
dataset %>%
  ggplot(aes(x = ypred, y = erro)) +
  geom_point()


library(lmtest)
#H0: residuals are distributed with equal variance (homoscedasticity)
#H1: resituals are distributed with unequal variance (heteroscedasticity)
lmtest::bptest(modelo) #BP < 0.05 reject h0 and indicates heteroscedasticity


#verificar se resíduos seguem distribuição normal
#H0:Os erros têm distribuição normal
#H1:Os erros não têm distribuição normal
shapiro.test(modelo$residuals) #< 0,05 reject H0
ks.test(modelo$residuals, "pnorm", 1, .16)





