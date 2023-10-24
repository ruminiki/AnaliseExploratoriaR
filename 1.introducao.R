#------------------- Apresentação do R e RStudio -------------------------------

# R é a linguagem de programação que vamos utilizar
# RStudio é um software que torna a utilização do R mais simples para o usuário

# A interface do RStudio divide-se em 4 grandes partes, principalmente para:

# 1ª Parte: script com o histórico de códigos daquele projeto ou análise
# 2ª Parte: console onde os códigos podem ser digitados e são implementados
# 3ª Parte: environment onde ficam listados os objetos
# 4ª Parte: onde aparecem outputs, help de pacotes, arquivos da pasta atual

# Sempre que iniciar uma nova análise, sugere-se criar um "project"

# File -> New Project -> New Directory -> New Project -> Nome do Projeto

# O project cria uma pasta, o que facilita a organização e o compartilhamento
# Também ajuda na importação de dados no RStudio

#------------------- Estrutura básica de operação do R -------------------------

# Normalmente, utiliza-se um script para guardar o histórico das análises

# File -> New File -> R Script

# Vamos conhecer as operações fundamentais do R

1 + 1
10 - 2
25 * 2
100 / 10
3 ^ 2
sqrt(25)

# Um parêntese: perceba que os textos, neste script, se iniciam com #
# Os códigos são digitados diretamente e são identificados como comandos
# O R funciona com base em objetos que ficam listados no environment

############### ESTRUTURAS DE DADOS #####################
# VETORES
#########################################################

# Vamos conhecer os objetos iniciando pelos vetores
# Os vetores mais simples são os atômicos (numéricos; caracteres; lógicos)

numeros <- c(1, 2, 3, 4, 5)

# Acima, criamos um objeto chamado "numeros" que é um vetor numérico
# Para criar o "numeros", utilizamos o indicador <- para atribuir os valores
# A função c() indica que estamos concatenando (combinando) valores em um vetor

numeros

# Os comandos são "case sensitive", isto é, diferenciam maiúsculas e minúsculas 

Numeros # note que houve um erro, não existe o objeto Numeros (maiúscula)

# Seguindo com as funções, poderíamos utilizar o comando print()

print(numeros)

# Vamos criar um vetor com textos, isto é, com caracteres:

pessoas <- c("João", "Maria", "Pedro", "Paula")

# Agora, foi criado um vetor de textos que foram colocados entre aspas duplas

pessoas

# Também poderíamos criar um vetor com argumentos lógicos, TRUE ou FALSE

logico <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
# Igualmente válido:
logico = c(T, F, T, T, F, F)

# Note que tais argumentos não são textos, estão definidos como lógicos
# Para saber a classe de vetor, é possível utilizar:

class(numeros)
class(pessoas)
class(logico)

# Para saber o comprimento do vetor, é possível utilizar:

length(numeros)
length(pessoas)
length(logico)

# Cada objeto tem seu nome e não há nomes iguais no ambiente

sequencia <- c(1:10) # esta é uma forma de gerar uma sequência de números

# Se atribuir o mesmo nome a outro objeto, o objeto antigo é substituído

sequencia <- c(500:600)

# Existem vetores que guardam informações de variadas classes:

varios <- c(1, 2, 3, "Azul", "Verde", "Vermelho", TRUE, FALSE, TRUE)

class(varios)
print(varios)

# O R atribui a classe "caracteres" a este vetor, ou seja, tornaram-se textos
# Isto ocorre, pois a prioridade estabelecida pelo R é a seguinte:

# É possível realizar operações com os vetores. A seguir, alguns exemplos:
# Observe os operadores comumente utilizados na linguagem R

numeros == 1
numeros * 2
triplo_numeros <- numeros * 3
metade_numeros <- numeros / 2

pessoas != "João"

sequencia > 550
sequencia >= 598
sequencia > 100
sequencia <= 100


############### ESTRUTURAS DE DADOS #####################
# FATORES
#########################################################

# Fatores: contêm variáveis categóricas que podem ser ordenadas
# A seguir, para criar um fator, vamos utilizar o c() dentro da função factor()
# A função factor() tem dois argumentos adicionais: levels e labels

altura <- factor(c("alto", "médio", "alto", "baixo", "médio", "alto"), 
                 levels = c("baixo", "médio", "alto"),
                 labels = c("prédios baixos",
                            "prédios médios",
                            "prédios altos"))

class(altura)

altura

# No caso dos fatores, a variável, inclusive, pode estar indicada como numérica

respostas <- factor(c(1, 2, 2, 3, 1, 2, 3, 3, 1, 2, 1, 1, 3, 2, 3),
                    levels = c(1, 2, 3),
                    labels = c("discordo totalmente",
                               "não concordo, nem discordo",
                               "concordo totalmente"))

class(respostas)

respostas

############### ESTRUTURAS DE DADOS #####################
# MATRIZES
#########################################################
# Matrizes: As matrizes são estruturas de dados usadas para armazenar dados em um formato tabular. 
# Cada linha da matriz representa uma observação e cada coluna representa uma variável.

# Cria um vetor de dados
carros <- c("Gol", "Strada", "Montana", "Celta", "Mobi")
potencia <- c(65, 105, 103, 75, 68)
consumo <- c(12.5, 9.5, 9.3, 11.2, 13.4)

# Cria uma matriz com um único elemento
matriz <- matrix(c(carros, potencia, consumo), nrow = 5, ncol = 3)

# Calcula a média de consumo
mean(as.numeric(matriz[,3]))

############### ESTRUTURAS DE DADOS #####################
# DATA FRAMES
#########################################################

# Outro tipo de objeto bastante relevante na análise de dados são os data frames

# Data frames: são os objetos que guardam informações como nas bases de dados
# Assim, nos data frames estão colunas (variáveis) e linhas (observações)

banco_dados_um <- data.frame(var1 = c("pessoa 1", "pessoa 2", "pessoa 3"),
                             var2 = c(42, 55, 28))

print(banco_dados_um)

# Note que foi criado um objeto do tipo "Data" no ambiente do RStudio

# Na função data.frame as variáveis devem ter o mesmo comprimento
# Vamos adicionar 3 vetores para formar o dataset

variavel_um <- c(1:10)
variavel_dois <- c(11:18, NA, NA)
variavel_tres <- c("a","b","c","d","e","f","g","h","i","j")

banco_dados_dois <- data.frame(variavel_um, variavel_dois, variavel_tres)

# No caso acima foi adicionado um argumento relevante: NA
# O NA é a indicação do dado "não disponível", isto é, missing value
# Note que o NA não é texto

#renomear a coluna
variaveis <- c("col1", "col2", "col3")
names(banco_dados_dois) <- variaveis

