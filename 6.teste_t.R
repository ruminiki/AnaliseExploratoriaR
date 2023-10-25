#####################################
# Carregar arquivo de dados
#####################################
df <- read.csv("data/notas_calculo.csv")

masculino <- df %>% filter(sexo == "M")
feminino  <- df %>% filter(sexo == "F")

t <- t.test(masculino$media_final ~ feminino$media_final, var.equal=TRUE)

with(df, t.test(df[sexo == "M"], df[sexo == "F"]))

cat(paste("\n Médias: ",round(t$estimate[[1]],digits = 2)," e ", round(t$estimate[[2]],digits = 2)))
cat(paste("\n P-valor: ",t$p.value))
cat(paste("\n Significância 1%? ",t$p.value<0.001))
cat(paste("\n Significância 5%? ",t$p.value<0.05))
cat("\n................................\n")


#teste t student quando amostra distribuição normal
diff2 <- function(a,vars){
  for (n in vars){
    cat(paste("\n> ", n,"\n"))
    t <- t.test(a[[n]] ~ a$situacao, a, var.equal=TRUE)
    cat(paste("\n Médias: ",round(t$estimate[[1]],digits = 2)," e ", round(t$estimate[[2]],digits = 2)))
    cat(paste("\n P-valor: ",t$p.value))
    cat(paste("\n Significância 1%? ",t$p.value<0.001))
    cat(paste("\n Significância 5%? ",t$p.value<0.05))
    cat("\n................................\n")
  }
}

t.test(dfp$ch_total_integralizada ~ dfp$situacao, dataset, var.equal=TRUE)
diff2(dfp, vars[2:length(vars)])

table(dfp$ch_total_integralizada)

do.call(data.frame,lapply(aux,function(x) replace(x, is.infinite(x), NA)))

tapply(dfp$periodo_atual, summary)
aggregate(df['periodo_atual'], list(df$situacao), FUN=mean, na.action = na.omit) 

nrow(dfp['periodo_atual'])
nrow(dfp['situacao'])

ggplot(dfi, aes(x=ispl, y=iech, )) + 
  geom_point() + scale_x_continuous(trans="log2") 
