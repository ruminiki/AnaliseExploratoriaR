library(dplyr)
library(ggplot2)
ano_nascimento <- c(1993,1989,1990,1980,1989,1988,1991,1969,1986,1980,1977,
                    1970,1973,1983,2002,1976,1989,1977)
#mes_nascimento <- c(5,8,11,9,7,5,7,12,10,2,10,12,12,11,2,9,4,11)

mes_nascimento <- factor(c(5,8,11,9,7,5,7,12,10,2,10,12,12,11,2,9,4,11),
                        levels=c(1,2,3,4,5,6,7,8,9,10,11,12),
                        labels=c("Jan","Fev","Mar","Abr","Mai","Jun",
                              "Jul","Ago","Set","Out","Nov","Dez"))

df <- data.frame("ano"=ano_nascimento, "mes"=mes_nascimento)

boxplot(df$ano)
summary(df)

df <- df %>% mutate(idade = as.numeric(format(Sys.time(), "%Y")) - ano)

boxplot(df$idade)
quantile(df$idade, probs = seq(0,1,0.25))
summary(df$idade)

