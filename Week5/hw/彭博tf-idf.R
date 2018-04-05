
library(Rfacebook)
token <- "EAACEdEose0cBACkdNw7N8MnOFP588ZB9ZBVZBtQohegsmb5lSjRYZAWSn5bSOodZA7d2iG8zp5BrO8ZByoHSWz7zSGv1a5oZABiVVmUn4wjQr5Ri1WxZC1uBUysBwdkpU9rTU21mXzl5NWKZBUZB7e6p6fZAXZAtxOCaNd9c27dsnLPr2uWRKKZCKwCvv7bln7EpBJZAtTrlr2FWNwlQZDZD"
me <- getUsers("me", token, private_info = TRUE)
me$name
page.id <- "214578858689103" 
page <- getPage(page.id, token=token, n = 100)
str(page)
View(page)

library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(slam)
library(RColorBrewer)
library(wordcloud)

docs <- Corpus(VectorSource(page$message))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))}
)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs

library(rJava)
library(Rwordseg)
library(tmcn)  
library(rjson)
mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
seg
freqFrame = as.data.frame(table(unlist(seg)))
freqFrame = freqFrame[-c(1:34),]
tail(freqFrame)

d.corpus <- Corpus(VectorSource(seg))
tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
inspect(tdm)


Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jre-10")
Sys.getenv("JAVA_HOME")

library(rJava)
library(SnowballC)
library(slam)
library(Matrix)

N = tdm$ncol
tf <- apply(tdm, 2, sum)
idfCal <- function(word_doc)
{
  log2( N / nnzero(word_doc) )
}
idf <- apply(tdm, 1, idfCal)

doc.tfidf <- as.matrix(tdm)
for(x in 1:nrow(tdm))
{
  for(y in 1:ncol(tdm))
  {
    doc.tfidf[x,y] <- (doc.tfidf[x,y] / tf[y]) * idf[x]
  }
}

tail(doc.tfidf)

