#####################################
# Carregar arquivo de dados
#####################################
df <- read.csv("data/notas_calculo.csv")

#teste de normalidade dos dados Shapiro-Wilk
#H0: dados seguem distribuição normal
#H1: dados não seguem distribuição normal
#p-valor < 0.05 indica que os dados não seguem distribuição normal
shapiro.test(df$media_final)

#se normal, usa o teste-t
#H0: não há diferença significativa entre as médias das duas amostras
#H1: há uma diferença significativa entre as médias das duas amostras
#p-valor < 0.05 há evidências de diferença entre as médias
t <- t.test(df$media_final ~ df$sexo, var.equal=TRUE)
print(t)

#se não normal deve-se usar algum teste não paramétrico (Mann-Whitney-Wilcoxon)
#H0: não há diferença significativa entre as médias das duas amostras
#H1: há uma diferença significativa entre as médias das duas amostras
#p-valor < 0.05 há evidências de diferença entre as médias
df1 <- df[df$sexo=="F",'media_final']
df2 <- df[df$sexo=="M",'media_final']
w <- wilcox.test(df1, df2, correct = FALSE,  alternative = "two.sided")  
print(w)

