library(Rfacebook)
token <- "EAACEdEose0cBAHaBFKdd9HyowOfJjdoWusgdtvMC2KUSbstYbkLigZAM1Fqh0bDboweJOUufcQjHPZCvl3OZA07NifnzdRy8fIKDt5bSLPm70PZBdtKUirZAEOzZBqfQueEbX7yLiMAmPw1UhRhgTLMQ1NXc6tE03J4ZCEWMdWnYgjTAZCxqkhHZBHybmC8LxZBUdiZCGXGdjPH2wZDZD"
me <- getUsers("me", token, private_info = TRUE)
me$name
page.id <- "5281959998" 
page <- getPage(page.id, token=token, n = 300, since = "2018/1/1", until = "2018/4/1")


library(quanteda)
train.tokens <- tokens(page$message, what = "word",
                       remove_numbers = TRUE, remove_punct = TRUE,
                       remove_symbols = TRUE, remove_hyphens = TRUE)
train.tokens <- tokens_tolower(train.tokens)
train.tokens <- tokens_select(train.tokens,stopwords(),
                              selection = "remove")
train.tokens <- tokens_select(train.tokens,c("new","york"),
                              selection = "remove")
train.tokens <- tokens_select(train.tokens,"one",
                              selection = "remove")
train.tokens <- tokens_wordstem(train.tokens, language = "english")

train.tokens.dfm <- dfm(train.tokens, tolower = FALSE, remove = stopwords())
dtm <- as.matrix(train.tokens.dfm)
tdm <- t(dtm)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=12)
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip()
#關注焦點: president Donald Trump


tf <- function(row){
  row / sum(row)
}

idf <- function(col){
  corpus.size <- length(col)
  doc.count <- length(which(col>0))
  log10(corpus.size / doc.count)
}

tf.idf <- function(tf,idf){
  tf * idf
}

docs.df <- apply(dtm, 1, tf)
docs.idf <- apply(dtm, 2, idf)
docs.tfidf <- apply(docs.df, 2, tf.idf, idf <- docs.idf)

a <- as.data.frame(head(docs.tfidf[,1:5]))
ggplot(a,aes(y=a$text1,x=rownames(a)))+geom_bar(stat = "identity")+ xlab("word")+ ylab("TFIDF")


b <- as.data.frame(sort(docs.tfidf[,1],decreasing = TRUE)[1:5])
colnames(b) <- "text1keyword"
ggplot(b,aes(y=b$text1keyword, x = rownames(b))) +geom_bar(stat = "identity")+ xlab("word")+ ylab("TFIDF")+ coord_flip()


