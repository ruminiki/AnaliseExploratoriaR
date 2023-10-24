library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("tibble")
library("tm")

############################
# Read the text file
text <- read_file("data/pequeno_principe.txt")
# Create a corpus  
docs <- Corpus(VectorSource(text))

gsub("https\\S*", "", text) 
gsub("@\\S*", "", text) 
gsub("amp", "", text) 
gsub("[\r\n]", "", text)
gsub("[[:punct:]]", "", text)

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 5,           
          max.words=200, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))
