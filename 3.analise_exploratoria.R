#################################
# Carregaro arquivo de dados
#################################
df <- read.csv("data/notas_calculo.csv")

df %>%
  ggplot(aes(periodo_atual, fill = situacao)) +
  geom_density(adjust=1.5, alpha = 0.2) +
  facet_grid(situacao ~ .)














#------------------- Funções e iterações ---------------------------------------

# Referência: (https://r4ds.had.co.nz/functions.html)

# Uma função é uma forma de simplificar um código
# É adequada sempre que for necessário repetir o mesmo código várias vezes
# Então, funções automatizam tarefas que seriam repetitivas
# Permite que sejam criados e nomeados objetos contendo tais funções

# Reduzir a duplicidade de códigos é importante, pois:
# Facilita a visualização (leitura do código)
# Facilita a mudança do código, caso necessário
# Evita erros durante a duplicação do código

# Para criar uma função, existem três etapas básicas:
# 1. Nomear a função
# 2. Indicar os argumentos (inputs) da função (ficam dentro de function(x, y))
# 3. Indicar o código, a função, que será implementado (fica dentro do {})

# Exemplo: todo dia atualizamos milhares de valores somando 17 e dividindo por 2
# Ao invés de repetir a mesma conta toda vez, poderíamos criar uma função:

atualizar <- function(histórico) {
  atual <- ((histórico + 17)/2)
  return(atual)
}

# O input é o que nomeamos de "histórico", isto é, o valor que vamos atualizar
# O "atual" é o nome que demos ao código que será implementado (a função)
# O "return" é o que queremos que a função retorne, isto é, o valor atualizado

atualizar (1)
atualizar (2)
atualizar (3)
atualizar (4)

# Ainda menos repetitivo, poderíamos criar um vetor com todos os valores
# Em seguida, o vetor entra como input na função para retornar todos os valores

atualizar_hoje <- c(1:15)

atualizar(atualizar_hoje)

# Poderíamos ter uma função com mais de um input. Por exemplo

ajustar <- function(valor1, valor2) {
  ajuste <- ((valor1 + 180)/(valor2 - 60))
  return(ajuste)
}

ajustar(100, 80)
ajustar(200, 80)
ajustar(200, 100)

# Neste contexto de funções, as condições "if, else if e else" são importantes
# Primeiramente, estabelecemos a condição entre parênteses
# Entre as primeiras chaves, vamos estabelecer o resultado caso if = TRUE
# Na sequência, estabelecemos o else, ou seja, o restante caso if = FALSE

valor <- 100000

if (valor >= 1000000) {
  "número grande"
} else {
  "número pequeno"
}

# Também poderíamos ter múltiplos critérios utilizando o "else if"

valor <- 650000

if (valor >= 1000000) {
  "número grande"
} else if (valor >= 500000 & valor <1000000) {
  "número intermediário"
} else {
  "número pequeno"
}

# É possível integrar condições ("if") às funções apresentadas anteriormente
# Voltando ao exemplo, vamos supor que atualizamos valores só até o limite 100
# Valores que seriam atualizados acima dele, ficam no limite estabelecido = 100

# Adicionamos o if (condição) {o que retornar quando for satisfeita}
# Na sequência, o else {o que retornar quando NÃO for satisfeita}

atualizar_teto <- function(histórico) {
  atual <- ((histórico + 17)/2)
  if (atual <= 100) {
    return(atual)}
  else {
    return("Atualizado no Teto = 100")}
}

atualizar_teto(44)
atualizar_teto(199)

# Do mesmo modo, é possível adicionar múltiplas condições às funções ("else if")

ajustar_mult <- function(valor1, valor2) {
  ajuste <- ((valor1 + 180)/(valor2 - 60))
  if(ajuste <= 100) {
    return("baixo")}
  else if(ajuste > 100 & ajuste <= 1000) {
    return("médio")}
  else {
    return("alto")}   
}

ajustar_mult(500, 300) # resultado = 2,8333
ajustar_mult(50000,100) # resultado = 1.254,50
ajustar_mult(1000, 70) # resultado = 118

# Nos exemplos acima, criamos nossas funções que utilizamos em cada código 
# Porém, também poderíamos utilizar funções já existentes
# Ou seja, o código pode conter uma grande diversidade de funções já existentes

médias <- function(x) {
  media <- mean(x, na.rm = T)
  return(media)
}

valores <- c(1, 4, 6, 9, 12, 16)

médias(valores)

# A seguir, vamos analisar o funcionamento do "for()" e do "while()"

# Retomando o trabalho de atualizar valores diariamente, podemos usar o for()

for (i in atualizar_hoje) {
  print((i + 17)/2)
}

# Interpretação: para todo valor i que consta no vetor "atualizar_hoje"
# Faça o print do resultado (valor i + 17)/2

# O while() permite que seja adicionada uma condição do tipo "enquanto"

valores <- 2

while(valores < 100){
  valores <- (valores + 20)
  print(valores)
}

# Interpretação: enquanto o resultado de valores for menor do que 100, some 20

# Portanto, funções e iterações são ferramentas que podem facilitar a escrita

#------------------- Data Visualization ----------------------------------------

# Para a visualização de dados, vamos utilizar principalmente o pacote "ggplot2"

# O "ggplot2" é um pacote para a criação de gráficos integrante do tidyverse  
# (https://ggplot2.tidyverse.org/)
# O ggplot2 foi instalado ao instalar o tidyverse. Agora, basta carregá-lo

library(ggplot2) # Obs.: se já tiver carregado o tidyverse, não precisa fazê-lo

# Os códigos para a criação dos mais diversos tipos de gráficos são semelhantes
# Em essência, muda-se a "geometria" do gráfico -> geometria = tipo de gráfico
# Muitos dos argumentos adicionais são ajustes e formatações
# Dada a necessidade da análise, escolhe-se o tipo de gráfico e sua "geom"

# Além do ggplot2, a seguir, outros pacotes que podem ser úteis:

install.packages(c("plotly","reshape2","ggrepel","rgl","car","sf","esquisse"))

options(rgl.debug = TRUE)

library(plotly)
library(reshape2)
library(ggrepel)
library(rgl)
library(car)
library(sf)
library(esquisse)

# --- Gráfico de Barras ---

# Vamos começar analisando uma variável qualitativa, o perfil do investidor

load("data/(2) perfil_investidor.RData")

# Como é uma variável categórica, vamos criar um gráfico de barras (geom_bar)
# Neste caso, o gráfico apresentará a contagem em cada categoria da variável
# Note que no "aes" (aesthetics) entra como argumento a variável de interesse

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil))

# Poderíamos mudar a ordem de apresentação reorganizando os níveis da variável

perfil_investidor$perfil <- factor(perfil_investidor$perfil,
                                   levels = c("Conservador", 
                                              "Moderado", 
                                              "Agressivo"))

# O novo gráfico seria:

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil))

# Conforme indicado, muitos dos argumentos adicionais são ajustes e formatações
# Vamos adicionar novas legendas ao gráfico

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil)) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020")

# Vamos alterar a cor das barras (argumento "fill")

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), fill = "blue") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020")

# As possíveis cores disponíveis na linguagem base podem ser consultadas

colours()

# Vamos adicionar bordas às barras (argumento "color")

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020")

# Vamos alterar o fundo do gráfico (theme)

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  theme_light()

# Um aspecto importante é que podemos adicionar mais de uma geometria ao gráfico
# Vamos adicionar um texto (geom_text) contendo os valores que foram contados

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", vjust = 2) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  theme_light()

# Também poderíamos apresentar o gráfico com os eixos invertidos

ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "red", fill = "blue") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", hjust = -1) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  coord_flip() +
  theme_light()

# --- Histograma ---

# A seguir, vamos elaborar o histograma de notas dos alunos (desempenho escolar)
# O banco de dados é o mesmo que já utilizamos anteriormente

load("(2) desempenho_aluno.RData")

# Iniciando com o gráfico básico (geom_histogram)

ggplot(data = desempenho_aluno) +
  geom_histogram(aes(x = desempenho))

# Vamos adicionar algumas formatações

ggplot(data = desempenho_aluno) +
  geom_histogram(aes(x = desempenho), color = "black", fill = "light green") +
  labs(x = "Desempenho Escolar",
       y = "Frequência") +
  theme_bw()

# Algumas vezes pode ser importante formatar a quantidade de barras apresentadas

ggplot(data = desempenho_aluno) +
  geom_histogram(aes(x = desempenho), color = "black", fill = "light green", bins = 50) +
  labs(x = "Desempenho Escolar",
       y = "Frequência") +
  theme_bw()

# --- Gráfico de Pontos ---

# Na sequência, vamos elaborar um gráfico de dispersão dos pontos
# Os dados são do atlas ambiental sobre distritos da cidade de São Paulo

load("(2) atlas_ambiental.RData")

# Iniciando com o gráfico básico (geom_point)
# Neste caso, devemos especificar as variáveis dos eixos x e y no "aes"

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade))

# Como há variáveis nos dois eixos, podemos adicionar outras variáveis:

# Na forma de tamanho dos pontos ("size")

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, size = idade))

# Na forma de cores dos pontos ("color")

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, size = idade, color = favel < 6))

# Na próprio formato dos pontos ("shape")

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, 
                 size = idade, color = favel < 6, 
                 shape = mortalidade > 18)) +
  labs(title = "Indicadores dos Distritos do Município de São Paulo",
       x = "Renda",
       y = "Escolaridade") +
  theme_bw()

# Também é possível plotar valores fitted no gráfico

ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, 
                 size = idade, color = favel < 6, 
                 shape = mortalidade > 18)) +
  geom_smooth(aes(x = renda, y = escolaridade), method = "loess", se = FALSE) +
  labs(title = "Indicadores dos Distritos do Município de São Paulo",
       x = "Renda",
       y = "Escolaridade") +
  theme_bw()

# --- Gráfico de Linhas ---

# Vamos elaborar um gráfico de linhas (geom_line) para dados ao longo do tempo
# Neste caso, vamos utilizar o banco de dados com preços de ações

library(readxl)
preco_acao <- read_excel("(2) precos_acao.xlsx")

# Como temos 4 ações no banco de dados, vamos implementar o seguinte gráfico
# Note que vamos separar cada empresa por meio da cor do gráfico

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao))

# Vamos formatar um pouco mais o gráfico

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao)) + 
  labs(x = "Mês de Referência",
       y = "Cotação de Fechamento",
       title = "Série Histórica das Ações",
       color = "Empresa") +
  theme_classic()

# Além disto, vamos adicionar os pontos com os valores da série (geom_point)

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao)) + 
  geom_point(aes(x = data, y = preco, color = acao)) +
  labs(x = "Mês de Referência",
       y = "Cotação de Fechamento",
       title = "Série Histórica das Ações",
       color = "Empresa") +
  theme_classic()

# Adicionar os labels dos pontos dentro do gráfico se torna inviável
# Veja o resultado:

ggplot(preco_acao) +
  geom_line(aes(x = data, y = preco, color = acao)) + 
  geom_point(aes(x = data, y = preco, color = acao)) +
  geom_text(aes(x = data, y = preco, label = preco), size = 3, vjust = 2, angle = 45) + 
  labs(x = "Mês de Referência",
       y = "Cotação de Fechamento",
       title = "Série Histórica das Ações",
       color = "Empresa") +
  theme_classic()

# Para tornar o gráfico mais interativo, vamos usar o ggplotly
# Basta indicar com o cursor do mouse o ponto para ver informações

ggplotly(
  ggplot(preco_acao) +
    geom_line(aes(x = data, y = preco, color = acao)) + 
    geom_point(aes(x = data, y = preco, color = acao)) +
    labs(x = "Mês de Referência",
         y = "Cotação de Fechamento",
         title = "Série Histórica das Ações",
         color = "Empresa") +
    theme_classic()
)

# --- Gráfico de Calor ---

# Vamos gerar um gráfico de calor que distingue informações por meio de cores
# O banco de dados contém informações sobre o tempo para chegar à escola
# Fonte: Fávero & Belfiore (2017, Capítulo 12)

library(readxl) # Se já tiver carregado antes, não precisa fazer novamente
tempo_dist <- read_excel("(2) tempo_dist.xls")

# Vamos trabalhar o gráfico de calor no contexto das correlações entre variáveis
# Portanto, primeiramente, vamos criar a matriz de correlações (função "cor")
# Lembrando: selecionar apenas as variáveis quantitativas da base de dados

correlacoes <- cor(tempo_dist[, 2:4])

correlacoes

# Algumas vezes, é necessário utilizar a base de dados em formatos específicos
# Neste caso, vamos colocar os valores das correlações como uma variável
# Para tanto, será aplicada a função "melt"

correlacoes_reorg <- melt(correlacoes)

View(correlacoes_reorg)

# Agora vamos elaborar um gráfico de calor (geom_tile) com algumas formatações

ggplot(correlacoes_reorg) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) + 
  labs(x = "Variáveis",
       y = "Variáveis",
       fill = "Coef. Correl.")

# Poderíamos trocar as cores para facilitar a visualização
# Ao mesmo tempo, vamos adicionar rótulos aos dados

ggplot(correlacoes_reorg) +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) + 
  geom_text(aes(x = Var1, y = Var2, label = value), size = 5) +
  labs(x = "Variáveis",
       y = "Variáveis",
       fill = "Coef. Correl.") +
  scale_fill_gradient2(low = "blue", 
                       mid = "yellow", 
                       high = "red",
                       midpoint = 0)

# Por fim, podemos deixar o gráfico mais interativo com a função "ggplotly"

ggplotly(
  ggplot(correlacoes_reorg) +
    geom_tile(aes(x = Var1, y = Var2, fill = value)) + 
    geom_text(aes(x = Var1, y = Var2, label = value), size = 5) +
    labs(x = "Variáveis",
         y = "Variáveis",
         fill = "Coef. Correl.") +
    scale_fill_gradient2(low = "blue", 
                         mid = "yellow", 
                         high = "red",
                         midpoint = 0)
)

# --- Gráficos Boxplot ---

# O boxplot apresenta medidas de posição das variáveis
# Mínimo, máximo, 1º quartil, mediana e 3º quartil
# Vemos a distribuição dos dados nas variáveis e eventuais outliers univariados

# Inicialmente, vamos apresentar o boxplot de uma única variável

load("(2) atlas_ambiental.RData")

var_boxplot <- atlas_ambiental[,c(1,3)]

ggplot(var_boxplot) +
  geom_boxplot(aes(y = renda), fill = "gray", color = "blue") +
  labs(x = "Renda",
       y = "Valores")

# Vamos torná-lo mais informativo com o ggplotly

ggplotly(
  ggplot(var_boxplot) +
    geom_boxplot(aes(y = renda), fill = "gray", color = "blue") +
    labs(x = "Renda",
         y = "Valores")
)

# Por fim, vamos utilizar a ferramenta Esquisser
# Basicamente, é uma ferramenta interativa para a criação de gráficos
# Vamos ver algumas funcionalidades

esquisser(atlas_ambiental, viewer = "browser")

# Fim!
