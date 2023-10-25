# Qual a média, mediana, desvio padrão, min e max?
# Remova os dados faltantes; 
# Existe diferença na média final de estudantes de sexo diferentes?
# Faça o box-plot da nota e responda:
# Quantos estudantes estão acima do terceiro quartil?
# Crie uma coluna STATUS:
# APR se nota >= 7 | REC se nota >= 5 e nota < 7 | REP se nota < 5
# Faça a contagem de aprovados por raça e sexo;
# Faça o gráfico de dispersão do ira e média final;
# Faça o mapa de correlação e identifique:
# Existe correlação entre o numero de faltas e a média final do estudante?
# Existe correlação entre a idade de ingresso no curso e a média final do estudante?

#################################
# Carregar arquivo de dados
#################################
df <- read.csv("data/notas_calculo.csv")

dim(df)

# Vamos utilizar o summarise e verificar informações preliminares
summarise(base_cvm_aj,
          observações=n(),
          média=mean(VARIACAO, na.rm = TRUE))

#summary

#box-plot

#bar-chart

#
